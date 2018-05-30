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

success :: BasePath -> Styles -> Html -> APIGatewayProxyResponse Text
success base styles body = responseOK 
    & responseBody ?~ renderPage base styles body
    & agprsHeaders `over` HashMap.insert "content-type" "text/html"

pageIndex :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
pageIndex request = do
    let urlPath = request ^. agprqPath
    putStrLn (unpack urlPath)
    let path = HashMap.lookup "name" (request ^. agprqPathParameters)
    let urlBase = fromMaybe "/" $ HashMap.lookup "url_base" (request ^. agprqStageVariables)
    let tryLoadPath = case path of
                        Nothing -> return Nothing
                        Just p -> loadArticle $ unpack p -- improve? Maybe string -> (string -> IO Maybe string) -> IO Maybe string
    single <- tryLoadPath
    index <- loadIndex
    let content = fromMaybe index single
    styles <- loadStyles
    return $ success urlBase styles content