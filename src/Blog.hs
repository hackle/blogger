{-# LANGUAGE OverloadedStrings #-}

module Blog where

import Text.Markdown
import Text.Blaze.Html5
import qualified Data.Text.Lazy.IO as IOT
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Control.Monad (forM_, guard)
import qualified Data.Text.Lazy as LT
import System.Directory
import Data.Text

type HtmlBody = H.Html
type ArticleName = String
type DefaultArticleName = ArticleName
type ValidFilePath = FilePath

cannotFindDefaultArticle :: String
cannotFindDefaultArticle = "Cannot find default article"

readArticle :: ValidFilePath -> IO Html
readArticle fn = do 
    content <- IOT.readFile fn
    return $ markdown def content

makePage :: HtmlBody -> H.Html
makePage body = H.docTypeHtml $ do
  H.head $
    H.title "hackmann's blog"
  H.body $ do
    H.h1 $
      H.a ! href "index" $ "hackmann"
    H.span "experienced imperative programmer turned functional advocate"
    body

renderPage :: HtmlBody -> Text
renderPage body = LT.toStrict $ renderHtml (makePage body)

getArticlePath :: DefaultArticleName -> ArticleName -> IO FilePath
getArticlePath def an = do
  cd <- getCurrentDirectory
  let fp = cd ++ "/src/raw/" ++ an ++ ".md"
  valid <- doesFileExist fp
  if valid 
    then return fp 
    else getArticlePath (error cannotFindDefaultArticle) def