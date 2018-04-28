{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blog (renderPage)
import Test.Hspec
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Text.Blaze.Html5 (style)
import Data.Text.Lazy (unpack)
import Data.List (isSuffixOf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "markdown" $
        it "should compile code" $
            renderPage "```putStrLn()```" "" `shouldBe` "blah"
    describe "style" $
        it "should render styles" $
            renderHtml (style ".page { color: 'black'; }") `shouldBe` "blah"
