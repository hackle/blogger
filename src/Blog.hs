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
        single <- loadArticle toLoad
        renderPage (getTitle toLoad) urlBase $ mconcat (single:seeAlso:otherBlogEntries)
        where
          entry = path >>= \p -> List.find (\e -> p == getSlug e) siteContents
          (latestArticle:_) = blogContents
          toLoad = fromMaybe latestArticle entry
          seeAlso = H.h4 "See also:"
          otherBlogEntries = List.map mkLink (List.filter (toLoad /=) blogContents)
          mkLink :: ContentEntry -> H.Html
          mkLink entry = do
              H.a ! href (toValue $ getSlug entry) $ toMarkup (getTitle entry)
              H.br

articleDir :: IO FilePath
articleDir = do
  cd <- getCurrentDirectory
  return $ cd ++ "/src/raw/"

mkArticlePath :: ArticleName -> IO FilePath
mkArticlePath an = do
  cd <- articleDir
  return $ cd ++ an

markdownDef :: MarkdownSettings
markdownDef = def { msBlockCodeRenderer = renderer }
  where renderer lang (_,rendered) = case lang of
                                       Just l -> H.pre $ H.code H.! A.class_ (H.toValue $ "language-" `mappend` l) $ rendered
                                       Nothing -> H.pre $ H.code $ rendered

loadArticle :: ContentEntry -> IO Html
loadArticle entry = do
  fp <- mkArticlePath $ getFile entry
  content <- IOT.readFile fp
  return $ mconcat [ pageTitle, markdown markdownDef content ] where
    pageTitle = H.h1 $ toMarkup (getTitle entry)

renderPage :: ArticleTitle -> FilePath -> H.Html -> IO Text
renderPage articleTitle base content = do
    styles <- loadStyles
    idrisPrism <- loadIdrisPrism
    let html = fromTemplate articleTitle (toValue base) content styles idrisPrism
    return $ LT.toStrict (renderHtml html)

loadIdrisPrism :: IO Html
loadIdrisPrism = loadFromCurrentDir "/src/prism-idris.js"

loadStyles :: IO Html
loadStyles = loadFromCurrentDir "/src/styles.css"

loadFromCurrentDir :: FilePath -> IO Html
loadFromCurrentDir fp = do
  cd <- getCurrentDirectory
  body <- IOT.readFile (cd ++ fp)
  return $ toHtml body
