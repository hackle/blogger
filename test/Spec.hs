{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blog (readArticle)
import Test.Hspec
import Text.Blaze.Html.Renderer.Text (renderHtml)
-- import qualified Text.Blaze.Html5 as BH
import Data.Text.Lazy (unpack)

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "get articles" $ do
        it "get a proper file" $ do
            article <- readArticle "./src/raw/about.md"
            maybe (pure ()) (\art -> unpack (renderHtml art) `shouldSatisfy` (not.null)) article
        
        it "null for non existent" $ do
            article <- readArticle "./src/raw/non-existent.md"
            maybe [] (const "blah") article `shouldSatisfy` null