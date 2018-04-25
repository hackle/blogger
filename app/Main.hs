{-# LANGUAGE OverloadedStrings #-}
module Main where

import           AWSLambda.Events.APIGateway
import           Control.Lens
import qualified Data.HashMap.Strict         as HashMap
import           Data.Semigroup
import           Data.Text                   (Text, unpack, toLower, pack)
import Blog
import Text.Blaze.Html
import Control.Monad.Trans.Maybe
import Data.Maybe
import Control.Applicative

main :: IO ()
main = apiGatewayMain pageIndex

success :: BasePath -> Html -> APIGatewayProxyResponse Text
success base body = responseOK 
    & responseBody ?~ renderPage body base
    & agprsHeaders `over` HashMap.insert "content-type" "text/html"

tryLoad :: BasePath -> FilePath -> IO (APIGatewayProxyResponse Text)
tryLoad base path = do
    f1 <- runMaybeT $ loadArticle path
    if isJust f1 
        then return $ success "" (fromJust f1)
        else success base <$> (mkArticlePath "index" >>= readArticle)

pageIndex :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
pageIndex request = do
    let urlPath = request ^. agprqPath
    putStrLn (unpack urlPath)
    let path = HashMap.lookup "name" (request ^. agprqPathParameters)
    tryLoad urlPath (maybe "" unpack path)