{-# LANGUAGE OverloadedStrings #-}
-- | Serve static files, subject to a policy that can filter or
--   modify incoming URIs. The flow is:
--
--   incoming request URI ==> policies ==> exists? ==> respond
--
--   If any of the polices fail, or the file doesn't
--   exist, then the middleware gives up and calls the inner application.
--   If the file is found, the middleware chooses a content type based
--   on the file extension and returns the file contents as the response.
module Network.Wai.Middleware.Static
    ( -- * Middlewares
      static, staticPolicy, staticPolicy', unsafeStaticPolicy, unsafeStaticPolicy'
    , -- * Cache Control
      CachingStrategy(..), FileMeta(..)
    , -- * Policies
      Policy, (<|>), (>->), policy, predicate
    , addBase, addSlash, contains, hasPrefix, hasSuffix, noDots, isNotAbsolute, only
    , -- * Utilities
      tryPolicy
    ) where

import Caching.ExpiringCacheMap.HashECM (newECMIO, lookupECM, CacheSettings(..), consistentDuration)
import Control.Monad.Trans (liftIO)
import Data.List
import Data.Maybe (fromMaybe)
import Data.Monoid
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Types (status200, status304)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.Wai
import System.Directory (doesFileExist)
import System.Locale
import System.Posix.Files
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString as B
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Map as M
import qualified Data.Text as T
import qualified System.FilePath as FP

-- | Take an incoming URI and optionally modify or filter it.
--   The result will be treated as a filepath.
newtype Policy = Policy { tryPolicy :: String -> Maybe String -- ^ Run a policy
                        }

-- | A cache strategy which should be used to
-- serve content matching a policy. Meta information is cached for a maxium of
-- 100 seconds before being recomputed.
data CachingStrategy
   -- | Do not send any caching headers
   = NoCaching
   -- | Send common caching headers for public (non dynamic) static files
   | PublicStaticCaching
   -- | Compute caching headers using the user specified function.
   -- See <http://www.mobify.com/blog/beginners-guide-to-http-cache-headers/> for a detailed guide
   | CustomCaching (FileMeta -> RequestHeaders)

-- | Note:
--   'mempty' == @policy Just@ (the always accepting policy)
--   'mappend' == @>->@ (policy sequencing)
instance Monoid Policy where
    mempty = policy Just
    mappend p1 p2 = policy (maybe Nothing (tryPolicy p2) . tryPolicy p1)

-- | Lift a function into a 'Policy'
policy :: (String -> Maybe String) -> Policy
policy = Policy

-- | Lift a predicate into a 'Policy'
predicate :: (String -> Bool) -> Policy
predicate p = policy (\s -> if p s then Just s else Nothing)

-- | Sequence two policies. They are run from left to right. (Note: this is `mappend`)
infixr 5 >->
(>->) :: Policy -> Policy -> Policy
(>->) = mappend

-- | Choose between two policies. If the first fails, run the second.
infixr 4 <|>
(<|>) :: Policy -> Policy -> Policy
p1 <|> p2 = policy (\s -> maybe (tryPolicy p2 s) Just (tryPolicy p1 s))

-- | Add a base path to the URI
--
-- > staticPolicy (addBase "/home/user/files")
--
-- GET \"foo\/bar\" looks for \"\/home\/user\/files\/foo\/bar\"
--
addBase :: String -> Policy
addBase b = policy (Just . (b FP.</>))

-- | Add an initial slash to to the URI, if not already present.
--
-- > staticPolicy addSlash
--
-- GET \"foo\/bar\" looks for \"\/foo\/bar\"
addSlash :: Policy
addSlash = policy slashOpt
    where slashOpt s@('/':_) = Just s
          slashOpt s         = Just ('/':s)

-- | Accept only URIs with given suffix
hasSuffix :: String -> Policy
hasSuffix = predicate . isSuffixOf

-- | Accept only URIs with given prefix
hasPrefix :: String -> Policy
hasPrefix = predicate . isPrefixOf

-- | Accept only URIs containing given string
contains :: String -> Policy
contains = predicate . isInfixOf

-- | Reject URIs containing \"..\"
noDots :: Policy
noDots = predicate (not . isInfixOf "..")

-- | Reject URIs that are absolute paths
isNotAbsolute :: Policy
isNotAbsolute = predicate $ not . FP.isAbsolute

-- | Use URI as the key to an association list, rejecting those not found.
-- The policy result is the matching value.
--
-- > staticPolicy (only [("foo/bar", "/home/user/files/bar")])
--
-- GET \"foo\/bar\" looks for \"\/home\/user\/files\/bar\"
-- GET \"baz\/bar\" doesn't match anything
--
only :: [(String,String)] -> Policy
only al = policy (flip lookup al)

-- | Serve static files out of the application root (current directory).
-- If file is found, it is streamed to the client and no further middleware is run. Disables caching.
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
static :: IO Middleware
static = staticPolicy mempty

-- | Serve static files subject to a 'Policy'. Disables caching.
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
staticPolicy :: Policy -> IO Middleware
staticPolicy = staticPolicy' NoCaching

-- | Serve static files subject to a 'Policy' using a specified 'CachingStrategy'
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
staticPolicy' :: CachingStrategy -> Policy -> IO Middleware
staticPolicy' cs p = unsafeStaticPolicy' cs $ noDots >-> isNotAbsolute >-> p

-- | Serve static files subject to a 'Policy'. Unlike 'static' and 'staticPolicy', this
-- has no policies enabled by default, and is hence insecure. Disables caching.
unsafeStaticPolicy :: Policy -> IO Middleware
unsafeStaticPolicy = unsafeStaticPolicy' NoCaching

-- | Serve static files subject to a 'Policy'. Unlike 'static' and 'staticPolicy', this
-- has no policies enabled by default, and is hence insecure. Also allows to set a 'CachingStrategy'.
unsafeStaticPolicy' :: CachingStrategy -> Policy -> IO Middleware
unsafeStaticPolicy' cacheStrategy p =
    do getFileMeta <- initializeFileMetaCache
       return $ middlewareHook getFileMeta cacheStrategy p

middlewareHook ::
    (FilePath -> IO FileMeta)
    -> CachingStrategy
    -> Policy
    -> (Request -> (Response -> IO b) -> IO b)
    -> Request
    -> (Response -> IO b)
    -> IO b
middlewareHook getFileMeta cs p app req callback =
    maybe (app req callback)
          (\fp ->
               do exists <- liftIO $ doesFileExist fp
                  if exists
                  then case cs of
                         NoCaching -> sendFile fp
                         _ ->
                             do wasNotModified <- checkNotModified fp (readHeader "If-Modified-Since") (readHeader "If-None-Match")
                                if wasNotModified
                                then sendNotModified fp
                                else sendFile fp
                  else app req callback)
          (tryPolicy p $ T.unpack $ T.intercalate "/" $ pathInfo req)
    where
      readHeader header =
          lookup header $ requestHeaders req
      checkNotModified fp modSince etag =
           do fm <- getFileMeta fp
              return $ or [ Just (fm_lastModified fm) == modSince
                          , Just (fm_etag fm) == etag
                          ]
      computeHeaders fp =
          case cs of
            NoCaching -> return []
            PublicStaticCaching ->
                do fm <- getFileMeta fp
                   return [ ("Cache-Control", "no-transform,public,max-age=300,s-maxage=900")
                          , ("Last-Modified", fm_lastModified fm)
                          , ("ETag", fm_etag fm)
                          , ("Vary", "Accept-Encoding")
                          ]
            CustomCaching f ->
                do fm <- getFileMeta fp
                   return (f fm)
      sendNotModified fp =
          do cacheHeaders <- computeHeaders fp
             callback $ responseLBS status304 cacheHeaders BSL.empty
      sendFile fp =
          do let basicHeaders =
                     [ ("Content-Type", getMimeType fp)
                     ]
             cacheHeaders <- computeHeaders fp
             let headers =
                     basicHeaders ++ cacheHeaders
             callback $ responseFile status200 headers fp Nothing

-- | Meta information about a file to calculate cache headers
data FileMeta
   = FileMeta
   { fm_lastModified :: !BS.ByteString
   , fm_etag :: !BS.ByteString
   , fm_fileName :: FilePath
   } deriving (Show, Eq)

initializeFileMetaCache :: IO (FilePath -> IO FileMeta)
initializeFileMetaCache =
    do let cacheAccess =
               consistentDuration 100 $ \state fp ->
                   do fileMeta <- computeFileMeta fp
                      return $! (state, fileMeta)
           cacheTick =
               do time <- getPOSIXTime
                  return (round (time * 100))
           cacheFreq = 1
           cacheLRU =
               CacheWithLRUList 100 100 200
       filecache <- newECMIO cacheAccess cacheTick cacheFreq cacheLRU
       return (lookupECM filecache)

computeFileMeta :: FilePath -> IO FileMeta
computeFileMeta fp =
    do mtime <- getModTime fp
       ct <- BSL.readFile fp
       return $ FileMeta
                { fm_lastModified =
                      BSC.pack $ formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" mtime
                , fm_etag = B16.encode (SHA1.hashlazy ct)
                , fm_fileName = fp
                }

getModTime :: FilePath -> IO UTCTime
getModTime fullFilePath =
    do stat <- getFileStatus fullFilePath
       return $ (\t -> posixSecondsToUTCTime (realToFrac t :: POSIXTime)) $ modificationTime stat

type Ascii = B.ByteString

getMimeType :: FilePath -> Ascii
getMimeType = go . extensions
    where go [] = defaultMimeType
          go (ext:exts) = fromMaybe (go exts) $ M.lookup ext defaultMimeTypes

extensions :: FilePath -> [String]
extensions [] = []
extensions fp = case dropWhile (/= '.') fp of
                    [] -> []
                    s -> let ext = tail s
                         in ext : extensions ext

defaultMimeType :: Ascii
defaultMimeType = "application/octet-stream"

-- This list taken from snap-core's Snap.Util.FileServe
defaultMimeTypes :: M.Map String Ascii
defaultMimeTypes = M.fromList [
  ( "asc"     , "text/plain"                        ),
  ( "asf"     , "video/x-ms-asf"                    ),
  ( "asx"     , "video/x-ms-asf"                    ),
  ( "avi"     , "video/x-msvideo"                   ),
  ( "bz2"     , "application/x-bzip"                ),
  ( "c"       , "text/plain"                        ),
  ( "class"   , "application/octet-stream"          ),
  ( "conf"    , "text/plain"                        ),
  ( "cpp"     , "text/plain"                        ),
  ( "css"     , "text/css"                          ),
  ( "cxx"     , "text/plain"                        ),
  ( "dtd"     , "text/xml"                          ),
  ( "dvi"     , "application/x-dvi"                 ),
  ( "gif"     , "image/gif"                         ),
  ( "gz"      , "application/x-gzip"                ),
  ( "hs"      , "text/plain"                        ),
  ( "htm"     , "text/html"                         ),
  ( "html"    , "text/html"                         ),
  ( "jar"     , "application/x-java-archive"        ),
  ( "jpeg"    , "image/jpeg"                        ),
  ( "jpg"     , "image/jpeg"                        ),
  ( "js"      , "text/javascript"                   ),
  ( "json"    , "application/json"                  ),
  ( "log"     , "text/plain"                        ),
  ( "m3u"     , "audio/x-mpegurl"                   ),
  ( "mov"     , "video/quicktime"                   ),
  ( "mp3"     , "audio/mpeg"                        ),
  ( "mp4"     , "video/mp4"                         ),
  ( "mpeg"    , "video/mpeg"                        ),
  ( "mpg"     , "video/mpeg"                        ),
  ( "ogg"     , "application/ogg"                   ),
  ( "ogv"     , "video/ogg"                         ),
  ( "pac"     , "application/x-ns-proxy-autoconfig" ),
  ( "pdf"     , "application/pdf"                   ),
  ( "png"     , "image/png"                         ),
  ( "ps"      , "application/postscript"            ),
  ( "qt"      , "video/quicktime"                   ),
  ( "sig"     , "application/pgp-signature"         ),
  ( "spl"     , "application/futuresplash"          ),
  ( "svg"     , "image/svg+xml"                     ),
  ( "swf"     , "application/x-shockwave-flash"     ),
  ( "tar"     , "application/x-tar"                 ),
  ( "tar.bz2" , "application/x-bzip-compressed-tar" ),
  ( "tar.gz"  , "application/x-tgz"                 ),
  ( "tbz"     , "application/x-bzip-compressed-tar" ),
  ( "text"    , "text/plain"                        ),
  ( "tgz"     , "application/x-tgz"                 ),
  ( "torrent" , "application/x-bittorrent"          ),
  ( "ttf"     , "application/x-font-truetype"       ),
  ( "txt"     , "text/plain"                        ),
  ( "wav"     , "audio/x-wav"                       ),
  ( "wax"     , "audio/x-ms-wax"                    ),
  ( "wma"     , "audio/x-ms-wma"                    ),
  ( "wmv"     , "video/x-ms-wmv"                    ),
  ( "xbm"     , "image/x-xbitmap"                   ),
  ( "xml"     , "text/xml"                          ),
  ( "xpm"     , "image/x-xpixmap"                   ),
  ( "xwd"     , "image/x-xwindowdump"               ),
  ( "zip"     , "application/zip"                   ) ]
