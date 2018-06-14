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
import qualified Data.List as List
import Data.Maybe
import Control.Applicative
import Template
import Contents

type HtmlBody = H.Html
type ArticleName = String
type DefaultArticleName = ArticleName
type ValidFilePath = FilePath
type CurrentDirectory = FilePath
type Styles = Html
  
loadPage :: FilePath -> Maybe FilePath -> IO Text
loadPage urlBase path = do
    let entry = path >>= \p -> List.find (\e -> p == getSlug e) siteContents
    single <- sequence $ loadArticle <$> entry
    index <- loadIndex
    renderPage urlBase (fromMaybe index single)

articleDir :: IO FilePath
articleDir = do
  cd <- getCurrentDirectory
  return $ cd ++ "/src/raw/"  

mkArticlePath :: ArticleName -> IO FilePath
mkArticlePath an = do
  cd <- articleDir
  return $ cd ++ an

loadArticle :: ContentEntry -> IO Html
loadArticle entry = do 
  fp <- mkArticlePath $ getFile entry
  content <- IOT.readFile fp
  return $ mconcat [ pageTitle, markdown def content ] where
    pageTitle = H.h1 $ toMarkup (getTitle entry)

loadIndex :: IO Html
loadIndex = do
  fullOfLatest <- loadArticle latest
  return $ mconcat (fullOfLatest:links) where
    (latest:rest) = blogContents
    links = List.map mkLink rest
    mkLink :: ContentEntry -> H.Html
    mkLink entry = 
      H.a ! href (toValue $ getSlug entry) $
        H.h2 (toMarkup $ getTitle entry)

renderPage :: FilePath -> H.Html -> IO Text
renderPage base content = do
    styles <- loadStyles
    let html = fromTemplate (toValue base) content styles
    return $ LT.toStrict (renderHtml html)

loadStyles :: IO Html
loadStyles = do
  cd <- getCurrentDirectory
  body <- IOT.readFile (cd ++ "/src/styles.css")
  return $ toHtml body