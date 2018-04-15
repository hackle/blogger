{-# LANGUAGE OverloadedStrings #-}

module Blog where

import Text.Markdown
import Text.Blaze.Html5
import qualified Data.Text.Lazy.IO as IOT
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Control.Monad (forM_)
import qualified Data.Text.Lazy as LT
import System.Directory
import Data.Text

readArticle :: FilePath -> IO Html
readArticle fn = do 
    content <- IOT.readFile fn
    return $ markdown def content

type HtmlBody = H.Html
type ArticleName = String

makePage :: HtmlBody -> H.Html
makePage body = H.docTypeHtml $ do
  H.head $ do
    H.title "hackmann's blog"
  H.body $ do
    H.h1 "hackmann"
    H.span "experienced imperative programmer turned functional advocate"
    body

renderPage :: HtmlBody -> Text
renderPage body = LT.toStrict $ renderHtml (makePage body)

getArticlePath :: ArticleName -> IO FilePath
getArticlePath an = do
  cd <- getCurrentDirectory
  return $ cd ++ "/src/raw/" ++ an ++ ".md"