{-# LANGUAGE CPP, OverloadedStrings #-}
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
      static, staticPolicy, unsafeStaticPolicy
    , static', staticPolicy', unsafeStaticPolicy'
    , staticWithOptions, staticPolicyWithOptions, unsafeStaticPolicyWithOptions
    , -- * Options
      Options, defaultOptions, cacheContainer, setCacheContainer, mimeTypes, setMimeTypes
    , -- * Cache Control
      CachingStrategy(..), FileMeta(..), initCaching, CacheContainer
    , -- * Policies
      Policy, (<|>), (>->), policy, predicate
    , addBase, addSlash, contains, hasPrefix, hasSuffix, noDots, isNotAbsolute, only
    , -- * Utilities
      tryPolicy
    , -- * MIME types
      getMimeType
    ) where

import Caching.ExpiringCacheMap.HashECM (newECMIO, lookupECM, CacheSettings(..), consistentDuration)
import Control.Monad.Trans (liftIO)
import Data.List
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid (Monoid(..))
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup (Semigroup(..))
#endif
import Data.Time
import Data.Time.Clock.POSIX
import Network.HTTP.Types (status200, status304)
import Network.HTTP.Types.Header (RequestHeaders)
import Network.Mime (MimeType, defaultMimeLookup)
import Network.Wai
import System.Directory (doesFileExist, getModificationTime)
#if !(MIN_VERSION_time(1,5,0))
import System.Locale
#endif
import Crypto.Hash.Algorithms
import Crypto.Hash
import Data.ByteArray.Encoding
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified System.FilePath as FP

-- | Take an incoming URI and optionally modify or filter it.
--   The result will be treated as a filepath.
newtype Policy = Policy { tryPolicy :: String -> Maybe String -- ^ Run a policy
                        }

-- | Options for 'staticWithOptions' 'Middleware'.
data Options = Options { cacheContainer :: CacheContainer -- ^ Cache container to use
                       , mimeTypes :: FilePath -> MimeType -- ^ Compute MimeType from file name
                       }

-- | Default options.
--
-- 'cacheContainer' = 'CacheContainerEmpty' -- no caching
-- 'mimeTypes' = 'getMimeTypes' -- use 'defaultMimeLookup' from 'Network.Mime'
defaultOptions :: Options
defaultOptions = Options { cacheContainer = CacheContainerEmpty, mimeTypes = getMimeType }

-- | Update cacheContainer of options.
setCacheContainer :: CacheContainer -> Options -> Options
setCacheContainer cc options = options { cacheContainer = cc }

-- | Update mimeTypes of options.
setMimeTypes :: (FilePath -> MimeType) -> Options -> Options
setMimeTypes mts options = options { mimeTypes = mts }

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
--   '(<>)' == @>->@ (policy sequencing)
instance Semigroup Policy where
    p1 <> p2 = policy (maybe Nothing (tryPolicy p2) . tryPolicy p1)

-- | Note:
--   'mempty' == @policy Just@ (the always accepting policy)
--   'mappend' == @>->@ (policy sequencing)
instance Monoid Policy where
    mempty  = policy Just
    mappend = (<>)

-- | Lift a function into a 'Policy'
policy :: (String -> Maybe String) -> Policy
policy = Policy

-- | Lift a predicate into a 'Policy'
predicate :: (String -> Bool) -> Policy
predicate p = policy (\s -> if p s then Just s else Nothing)

-- | Sequence two policies. They are run from left to right. (Note: this is `mappend`)
infixr 5 >->
(>->) :: Policy -> Policy -> Policy
(>->) = (<>)

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
only :: [(String, String)] -> Policy
only al = policy (flip lookup al)

-- | Serve static files out of the application root (current directory).
-- If file is found, it is streamed to the client and no further middleware is run. Disables caching.
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
static :: Middleware
static = staticPolicy mempty

-- | Serve static files out of the application root (current directory).
-- If file is found, it is streamed to the client and no further middleware is run. Allows a 'CachingStrategy'.
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
static' :: CacheContainer -> Middleware
static' cc = staticPolicy' cc mempty

-- | Serve static files out of the application root (current directory).
-- If file is found, it is streamed to the client and no further middleware is run. Takes 'Options'.
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
staticWithOptions :: Options -> Middleware
staticWithOptions options = staticPolicyWithOptions options mempty

-- | Serve static files subject to a 'Policy'. Disables caching.
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
staticPolicy :: Policy -> Middleware
staticPolicy = staticPolicy' (cacheContainer defaultOptions)

-- | Serve static files subject to a 'Policy' using a specified 'CachingStrategy'
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
staticPolicy' :: CacheContainer -> Policy -> Middleware
staticPolicy' cc p = unsafeStaticPolicy' (mimeTypes defaultOptions) cc $ noDots >-> isNotAbsolute >-> p

-- | Serve static files subject to a 'Policy' using specified 'Options'
--
-- Note: for security reasons, this uses the 'noDots' and 'isNotAbsolute' policy by default.
staticPolicyWithOptions :: Options -> Policy -> Middleware
staticPolicyWithOptions options p = unsafeStaticPolicyWithOptions options $ noDots >-> isNotAbsolute >-> p

-- | Serve static files subject to a 'Policy'. Unlike 'static' and 'staticPolicy', this
-- has no policies enabled by default and is hence insecure. Disables caching.
unsafeStaticPolicy :: Policy -> Middleware
unsafeStaticPolicy = unsafeStaticPolicy' (mimeTypes defaultOptions) (cacheContainer defaultOptions)

-- | Serve static files subject to a 'Policy'. Unlike 'staticWithOptions' and 'staticPolicyWithOptions',
-- this has no policies enabled by default and is hence insecure. Takes 'Options'.
unsafeStaticPolicyWithOptions :: Options -> Policy -> Middleware
unsafeStaticPolicyWithOptions options = unsafeStaticPolicy' (mimeTypes options) (cacheContainer options)

-- | Serve static files subject to a 'Policy'. Unlike 'static' and 'staticPolicy', this
-- has no policies enabled by default, and is hence insecure. Also allows to set a 'CachingStrategy'.
unsafeStaticPolicy' ::
    (FilePath -> MimeType)
    -> CacheContainer
    -> Policy
    -> Middleware
unsafeStaticPolicy' getMimeTypeFn cc p app req callback =
    maybe (app req callback)
          (\fp ->
               do exists <- liftIO $ doesFileExist fp
                  if exists
                  then case cc of
                         CacheContainerEmpty ->
                             sendFile fp []
                         CacheContainer _ NoCaching ->
                             sendFile fp []
                         CacheContainer getFileMeta strategy ->
                             do fileMeta <- getFileMeta fp
                                if checkNotModified fileMeta (readHeader "If-Modified-Since") (readHeader "If-None-Match")
                                then sendNotModified fileMeta strategy
                                else sendFile fp (computeHeaders fileMeta strategy)
                  else app req callback)
          (tryPolicy p $ T.unpack $ T.intercalate "/" $ pathInfo req)
    where
      readHeader header =
          lookup header $ requestHeaders req
      checkNotModified fm modSince etag =
          or [ Just (fm_lastModified fm) == modSince
             , Just (fm_etag fm) == etag
             ]
      computeHeaders fm cs =
          case cs of
            NoCaching -> []
            PublicStaticCaching ->
                [ ("Cache-Control", "no-transform,public,max-age=300,s-maxage=900")
                , ("Last-Modified", fm_lastModified fm)
                , ("ETag", fm_etag fm)
                , ("Vary", "Accept-Encoding")
                ]
            CustomCaching f -> f fm
      sendNotModified fm cs =
          do let cacheHeaders = computeHeaders fm cs
             callback $ responseLBS status304 cacheHeaders BSL.empty
      sendFile fp extraHeaders =
          do let basicHeaders =
                     [ ("Content-Type", getMimeTypeFn fp)
                     ]
                 headers =
                     basicHeaders ++ extraHeaders
             callback $ responseFile status200 headers fp Nothing

-- | Container caching file meta information. Create using 'initCaching'
data CacheContainer
    = CacheContainerEmpty
    | CacheContainer (FilePath -> IO FileMeta) CachingStrategy

-- | Meta information about a file to calculate cache headers
data FileMeta
   = FileMeta
   { fm_lastModified :: !BS.ByteString
   , fm_etag :: !BS.ByteString
   , fm_fileName :: FilePath
   } deriving (Show, Eq)

-- | Initialize caching. This should only be done once per application launch.
initCaching :: CachingStrategy -> IO CacheContainer
initCaching cs =
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
       return (CacheContainer (lookupECM filecache) cs)

computeFileMeta :: FilePath -> IO FileMeta
computeFileMeta fp =
    do mtime <- getModificationTime fp
       ct <- BSL.readFile fp
       return $ FileMeta
                { fm_lastModified =
                      BSC.pack $ formatTime defaultTimeLocale "%a, %d-%b-%Y %X %Z" mtime
                , fm_etag = convertToBase Base16 (hashlazy ct :: Digest SHA1)
                , fm_fileName = fp
                }

-- | Guess MIME type from file extension
getMimeType :: FilePath -> MimeType
getMimeType = defaultMimeLookup . T.pack
