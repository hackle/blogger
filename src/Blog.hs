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
    let loadPath = case path of
                    Nothing -> return Nothing
                    Just p -> loadArticle p
    single <- loadPath
    index <- loadIndex
    renderPage urlBase (fromMaybe index single)

articleDir :: IO FilePath
articleDir = do
  cd <- getCurrentDirectory
  return $ cd ++ "/src/raw/"  

readArticle :: ValidFilePath -> IO Html
readArticle fn = do 
    content <- IOT.readFile fn
    return $ markdown def content

mkArticlePath :: ArticleName -> IO FilePath
mkArticlePath an = do
  cd <- articleDir
  return $ cd ++ an ++ ".md"

loadArticle :: ArticleName -> IO (Maybe Html)
loadArticle artName = do 
  fp <- mkArticlePath artName
  valid <- doesFileExist fp
  if valid 
    then Just <$> readArticle fp
    else return Nothing

loadIndex :: IO Html
loadIndex = do
  cd <- articleDir
  files <- getDirectoryContents cd
  let blogs = List.filter (`elem` posts) files
  let articles = List.map (\p -> IOT.readFile (cd ++ p)) blogs
  bodies <- sequence articles
  return $ mconcat (markdown def <$> bodies)

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