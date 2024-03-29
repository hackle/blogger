{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

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
import Network.URI.Encode (encode)
import Control.Monad.Reader

type HtmlBody = H.Html
type ArticleName = String
type DefaultArticleName = ArticleName
type ValidFilePath = FilePath
type CurrentDirectory = FilePath
type Styles = Html

data Env = Env {
  currentDir :: FilePath
  , urlBase :: FilePath
  , currentArticle :: ContentEntry
}

class Monad m => MonadBlog m where
  readAFile :: String -> m LT.Text

instance (MonadIO m) => MonadBlog (ReaderT Env m) where
  readAFile file = do
    env <- ask
    let dir = currentDir env
    liftIO $ IOT.readFile (dir ++ "/src/raw/" ++ file)

loadPage :: String -> CurrentDirectory -> Maybe ArticleName -> IO Text
loadPage urlBase currentDir requested = do
  let env = Env {
    urlBase = urlBase
    , currentArticle = findFileToLoad requested
    , currentDir = currentDir
  } 
  html <- runReaderT renderPage env
  return $ LT.toStrict (renderHtml html)

renderPage :: (MonadBlog m, MonadReader Env m) => m H.Html
renderPage = do
    styles <- loadStyles
    idrisPrism <- loadIdrisPrism
    body <- articleBody
    env <- ask
    let toLoad = currentArticle env
        base = toValue $ urlBase env
    return $ fromTemplate toLoad base body styles idrisPrism

articleBody :: (MonadReader Env m, MonadBlog m) => m H.Html
articleBody = do
    env <- ask
    main <- loadArticle
    toShare <- sharing
    others <- otherBlogEntries
    return $ mconcat (main:toShare:seeAlso:others)


loadArticle :: (MonadReader Env m, MonadBlog m) => m H.Html
loadArticle = do
  env <- ask
  let entry = currentArticle env
      articlePath = getFile entry
      pageTitle = H.h1 $ toMarkup (getTitle entry)
  content <- readAFile articlePath
  return $ mconcat [ pageTitle, markdown markdownDef content ]

loadIdrisPrism :: (MonadReader Env m, MonadBlog m) => m H.Html
loadIdrisPrism = loadFromCurrentDir "prism-idris.js"

loadStyles :: (MonadReader Env m, MonadBlog m) => m H.Html
loadStyles = loadFromCurrentDir "styles.css"

loadFromCurrentDir :: (MonadReader Env m, MonadBlog m) => FilePath -> m H.Html
loadFromCurrentDir fp = do
  env <- ask
  body <- readAFile fp
  return $ toHtml body

findFileToLoad :: Maybe ArticleName -> ContentEntry
findFileToLoad requested =
  let found = requested >>= \r -> List.find (eqSlug r) siteContents
  in  fromMaybe (List.head blogContents) found

seeAlso = H.h4 "See also:"

sharing :: (MonadReader Env m) => m H.Html 
sharing = do 
  env <- ask
  let toLoad = currentArticle env
  return $
    H.p $ shareLinks toLoad

shareLinks :: ContentEntry -> H.Html
shareLinks ent = do
    "Share on "
    twitterLink ent
    " "
    facebookLink ent
    " "
    linkedInLink ent

otherBlogEntries :: (MonadReader Env m) => m [H.Html]
otherBlogEntries = do
  env <- ask
  let current = currentArticle env
  return $
    List.map mkLink (List.filter (\e -> e /= current) blogContents)

mkLink :: ContentEntry -> H.Html
mkLink entry = do
    H.a ! href (toValue $ getSlug entry) $ toMarkup (getTitle entry)
    H.br

markdownDef :: MarkdownSettings
markdownDef = def { msBlockCodeRenderer = renderer }
  where renderer lang (_,rendered) = case lang of
                                       Just l -> H.pre $ H.code H.! A.class_ (H.toValue $ "language-" `mappend` l) $ rendered
                                       Nothing -> H.pre $ H.code $ rendered

twitterLink :: ContentEntry -> H.Html
twitterLink entry = do
    let lnk = "https://twitter.com/intent/tweet?url=https%3A%2F%2Fwww.hacklewayne.com%2F" ++ getSlug entry 
                    ++ "&text=" ++ encode (getTitle entry)
    H.a ! href (toValue lnk) $ "Twitter"

facebookLink :: ContentEntry -> H.Html
facebookLink entry = do
    let lnk = "https://www.facebook.com/sharer/sharer.php?u=https%3A%2F%2Fwww.hacklewayne.com%2F" ++ getSlug entry
    H.a ! href (toValue lnk) $ "Facebook"

linkedInLink :: ContentEntry -> H.Html
linkedInLink entry = do
    let lnk = "http://www.linkedin.com/shareArticle?mini=true&url=https%3A%2F%2Fwww.hacklewayne.com%2F" ++ getSlug entry
                ++ "&title=" ++ encode (getTitle entry)
    H.a ! href (toValue lnk) $ "LinkedIn"
