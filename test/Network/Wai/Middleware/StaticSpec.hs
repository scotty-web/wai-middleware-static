{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.StaticSpec (spec) where

import           Control.Monad (forM_)
import           Data.String (fromString)
import           Data.Time (addUTCTime, defaultTimeLocale, formatTime)
import           System.Directory (getModificationTime)

import           Test.Hspec hiding (shouldBe, shouldReturn)
import           Test.Hspec.Expectations.Lifted
import           Test.Hspec.Wai
import           Test.Mockery.Directory

import           Web.Scotty (scottyApp)
import           Network.Wai.Test (SResponse(..))
import           Network.HTTP.Types.Status
import qualified Network.HTTP.Client as Http
import qualified Network.Wai.Handler.Warp as Warp

import           Network.Wai.Middleware.Static

spec :: Spec
spec = around_ inTempDirectory $ do
  describe "static" $ with (static `fmap` scottyApp (return ())) $ do
    before_ (writeFile "foo.html" "some content") $ do
      context "on HEAD" $ do
        it "serves static file" $ do
          request "HEAD" "/foo.html" [] "" `shouldReturn` SResponse {
            simpleStatus = status200
          , simpleHeaders = [("Content-Type", "text/html")]
          , simpleBody = "some content"
          }

      context "on GET" $ do
        it "serves static file" $ do
          get "/foo.html" `shouldReturn` SResponse {
            simpleStatus = status200
          , simpleHeaders = [("Content-Type", "text/html")]
          , simpleBody = "some content"
          }

      context "otherwise" $ do
        it "serves upstream" $ do
          post "/foo.html" "" `shouldReturn` SResponse {
            simpleStatus = status404
          , simpleHeaders = [("Content-Type", "text/html")]
          , simpleBody = "<h1>404: File Not Found!</h1>"
          }

  describe "staticWithOptions" $ with cachedApp $ do
    before_ (writeFile "foo.html" "some content") $ do
      it "sends a quoted weak ETag and accepts it on revalidation" $ do
        let etag = "W/\"94e66df8cd09d410c62d9e0dc59d3a884e458e05\""
        response <- get "/foo.html"
        lookup "ETag" (simpleHeaders response) `shouldBe` Just etag
        request "GET" "/foo.html" [("If-None-Match", etag)] ""
          `shouldRespondWith` 304 {matchHeaders = ["ETag" <:> etag], matchBody = ""}

      it "sends Last-Modified in the standard GMT format" $ do
        modified <- liftIO $ getModificationTime "foo.html"
        let expected = fromString $ formatTime defaultTimeLocale "%a, %d %b %Y %H:%M:%S GMT" modified
        response <- get "/foo.html"
        lookup "Last-Modified" (simpleHeaders response) `shouldBe` Just expected

      it "weakly compares a single ETag and handles the wildcard" $ do
        forM_ ["\"94e66df8cd09d410c62d9e0dc59d3a884e458e05\"",
               " \tW/\"94e66df8cd09d410c62d9e0dc59d3a884e458e05\"\t ",
               "*", " \t*\t "] $ \etag ->
          request "GET" "/foo.html" [("If-None-Match", etag)] ""
            `shouldRespondWith` 304 {matchBody = ""}
        forM_ ["\"outdated\"", "94e66df8cd09d410c62d9e0dc59d3a884e458e05"] $ \etag ->
          request "GET" "/foo.html" [("If-None-Match", etag)] ""
            `shouldRespondWith` "some content"
        request "GET" "/missing.html" [("If-None-Match", "*")] "" `shouldRespondWith` 404

      it "compares modern HTTP dates and ignores invalid dates" $ do
        modified <- liftIO $ getModificationTime "foo.html"
        let dateAt offset = fromString $ formatTime defaultTimeLocale
              "%a, %d %b %Y %H:%M:%S GMT" (addUTCTime offset modified)
        forM_ [dateAt 0, dateAt 60] $ \date ->
          request "GET" "/foo.html" [("If-Modified-Since", date)] ""
            `shouldRespondWith` 304 {matchBody = ""}
        forM_ [dateAt (-60), "not a date", "Thu, 29 Oct 2099 00:00:00 GMT garbage"] $ \date ->
          request "GET" "/foo.html" [("If-Modified-Since", date)] ""
            `shouldRespondWith` "some content"

  describe "conditional requests through Warp" $ do
    it "ignores If-Modified-Since when If-None-Match does not match" $ withCachedServer $ \manager req -> do
      initial <- Http.httpLbs req manager
      modified <- maybe (fail "Missing Last-Modified") return $ lookup "Last-Modified" (Http.responseHeaders initial)
      forM_ ["\"outdated\"", "", "not-an-etag"] $ \etag -> do
        response <- Http.httpLbs req {Http.requestHeaders = [
            ("If-None-Match", etag), ("If-Modified-Since", modified)]} manager
        Http.responseStatus response `shouldBe` status200
        Http.responseBody response `shouldBe` "some content"

    it "returns an empty 304 with cache headers when the ETag matches" $ withCachedServer $ \manager req -> do
      initial <- Http.httpLbs req manager
      etag <- maybe (fail "Missing ETag") return $ lookup "ETag" (Http.responseHeaders initial)
      response <- Http.httpLbs req {Http.requestHeaders = [
          ("If-None-Match", etag), ("If-Modified-Since", "Sat, 01 Jan 2000 00:00:00 GMT")]} manager
      Http.responseStatus response `shouldBe` status304
      Http.responseBody response `shouldBe` ""
      forM_ ["ETag", "Last-Modified", "Cache-Control", "Vary"] $ \header ->
        lookup header (Http.responseHeaders response) `shouldBe` lookup header (Http.responseHeaders initial)
  where
    cachedApp = do
      cache <- initCaching PublicStaticCaching
      staticWithOptions defaultOptions {cacheContainer = cache}
        `fmap` scottyApp (return ())

    withCachedServer action = do
      writeFile "foo.html" "some content"
      Warp.testWithApplication cachedApp $ \port -> do
        manager <- Http.newManager $ Http.managerSetProxy Http.noProxy Http.defaultManagerSettings
        let req = Http.defaultRequest {Http.host = "127.0.0.1", Http.port = port, Http.path = "/foo.html"}
        action manager req
