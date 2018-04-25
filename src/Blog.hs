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

cannotFindDefaultArticle :: String
cannotFindDefaultArticle = "Cannot find default article"

readArticle :: ValidFilePath -> IO Html
readArticle fn = do 
    content <- IOT.readFile fn
    return $ markdown def content

makePage :: HtmlBody -> BasePath -> H.Html
makePage body base = H.docTypeHtml $ do
  H.head $ do
    H.title "hackmann's blog"
    H.base ! href (H.toValue base)
  H.body $ do
    H.h1 $
      H.a ! href "index" $ "hackmann"
    H.span "experienced imperative programmer turned functional advocate"
    body

renderPage :: HtmlBody -> BasePath -> Text
renderPage body base = LT.toStrict $ renderHtml (makePage body base)

mkArticlePath :: ArticleName -> IO FilePath
mkArticlePath an = do
  cd <- getCurrentDirectory
  return $ cd ++ "/src/raw/" ++ an ++ ".md"

loadArticle :: ArticleName -> MaybeT IO Html
loadArticle an = do 
  fp <- lift $ mkArticlePath an
  valid <- lift $ doesFileExist fp
  if valid 
    then lift $ readArticle fp
    else MaybeT $ return Nothing
    