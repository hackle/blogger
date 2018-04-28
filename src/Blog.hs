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
import Control.Monad.Trans.Maybe
import Control.Monad.Trans

type HtmlBody = H.Html
type ArticleName = String
type DefaultArticleName = ArticleName
type ValidFilePath = FilePath
type BasePath = Text
type CurrentDirectory = FilePath
type Styles = Html

cannotFindDefaultArticle :: String
cannotFindDefaultArticle = "Cannot find default article"

readArticle :: ValidFilePath -> IO Html
readArticle fn = do 
    content <- IOT.readFile fn
    return $ markdown def content

makePage :: Styles -> HtmlBody -> H.Html
makePage styles body = H.docTypeHtml $ do
  H.head $ do
    H.title "hackman"
    H.link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.10.0/github-markdown.min.css"
    H.style styles 
  H.body ! class_ "markdown-body" $ do
    H.h1 $
      H.a ! class_ "title" ! href "index" $ "hackman"
    H.span "Between the abstractions I want and the abstractions I get" ! class_ "subtitle" 
    body

renderPage :: Styles -> HtmlBody -> Text
renderPage styles body = LT.toStrict $ renderHtml (makePage styles body)

mkArticlePath :: ArticleName -> IO FilePath
mkArticlePath an = do
  cd <- getCurrentDirectory
  return $ cd ++ "/src/raw/" ++ an ++ ".md"

loadArticle :: DefaultArticleName -> ArticleName -> IO Html
loadArticle def an = do 
  fp <- mkArticlePath an
  valid <- doesFileExist fp
  if valid 
    then readArticle fp
    else loadArticle undefined def
    
loadStyles :: IO Html
loadStyles = do
  cd <- getCurrentDirectory
  body <- IOT.readFile (cd ++ "/src/styles.css")
  return $ toHtml body