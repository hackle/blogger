{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blog (readArticle)
import Test.Hspec
import Text.Blaze.Html.Renderer.Text
import qualified Text.Blaze.Html5 as BH
import Data.Text.Lazy

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "get articles" $ do
        it "get one" $ do
            let article = readArticle "./src/raw/about.md" in
                article >>= \art -> renderHtml art `shouldBe` "blaha"