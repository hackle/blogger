{-# LANGUAGE OverloadedStrings #-}

module Main where

import Blog (getArticlePath, cannotFindDefaultArticle)
import Test.Hspec
import Text.Blaze.Html.Renderer.Text (renderHtml)
-- import qualified Text.Blaze.Html5 as BH
import Data.Text.Lazy (unpack)
import Data.List (isSuffixOf)

main :: IO ()
main = hspec spec

spec :: Spec
spec = 
    describe "get article path" $ do
        it "get a proper file" $ do
            filePath <- getArticlePath "blah" "about" -- blah does not exist
            filePath `shouldSatisfy` ("about.md" `isSuffixOf`)
        
        it "can default to a file" $ do
            filePath <- getArticlePath "about" "blah" -- blah does not exist
            putStrLn filePath
            filePath `shouldSatisfy` ("about.md" `isSuffixOf`)

        it "will fail if even default does not exist" $ 
            getArticlePath "blah" "halb" `shouldThrow` errorCall cannotFindDefaultArticle