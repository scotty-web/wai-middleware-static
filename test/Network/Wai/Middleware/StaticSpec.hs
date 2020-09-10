{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.StaticSpec (spec) where

import           Test.Hspec hiding (shouldReturn)
import           Test.Hspec.Expectations.Lifted
import           Test.Hspec.Wai
import           Test.Mockery.Directory

import           Web.Scotty (scottyApp)
import           Network.Wai.Test (SResponse(..))
import           Network.HTTP.Types.Status

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
