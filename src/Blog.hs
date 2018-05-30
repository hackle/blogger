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

type HtmlBody = H.Html
type ArticleName = String
type DefaultArticleName = ArticleName
type ValidFilePath = FilePath
type BasePath = Text
type CurrentDirectory = FilePath
type Styles = Html

cannotFindDefaultArticle :: String
cannotFindDefaultArticle = "Cannot find default article"

articleDir :: IO FilePath
articleDir = do
  cd <- getCurrentDirectory
  return $ cd ++ "/src/raw/"  

readArticle :: ValidFilePath -> IO Html
readArticle fn = do 
    content <- IOT.readFile fn
    return $ markdown def content

makePage :: BasePath -> Styles -> HtmlBody -> H.Html
makePage base styles body = H.docTypeHtml $ do
  H.head $ do
    H.title "Hackle's blog"
    H.base ! href (toValue base)
    H.link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.10.0/github-markdown.min.css"
    H.style styles 
  H.body ! class_ "markdown-body" $ do
    H.h1 ! class_ "title" $
      H.a ! href "/" $ "hackman"
    H.span "between the abstractions we want and the abstractions we get." ! class_ "subtitle" 
    body

renderPage :: BasePath -> Styles -> HtmlBody -> Text
renderPage base styles body = LT.toStrict $ renderHtml (makePage base styles body)

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

posts :: [FilePath]
posts = [
    "setup-haskell.md"
    , "modeling.md"
    , "linq.md"
    , "lens-csharp.md"
    ]

loadIndex :: IO Html
loadIndex = do
  cd <- articleDir
  files <- getDirectoryContents cd
  let blogs = List.filter (\f -> elem f posts) files
  let articles = List.map (\p -> IOT.readFile (cd ++ p)) blogs
  contents <- sequence articles
  return $ toHtml (List.foldl1 LT.append contents)
    
loadStyles :: IO Html
loadStyles = do
  cd <- getCurrentDirectory
  body <- IOT.readFile (cd ++ "/src/styles.css")
  return $ toHtml body