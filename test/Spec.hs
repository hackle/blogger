{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blog (renderPage)
import Test.Hspec
import Text.Blaze.Html.Renderer.Text (renderHtml)
-- import qualified Text.Blaze.Html5 as BH
import Data.Text.Lazy (unpack)
import Data.List (isSuffixOf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "markdown" $
        it "should compile code" $
            renderPage "```putStrLn()```" "" `shouldBe` "blah"