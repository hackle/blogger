{-# LANGUAGE OverloadedStrings #-}
module Main where

import AWSLambda.Events.APIGateway
import Control.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup
import Data.Text (Text, unpack, toLower, pack)
import Blog (loadPage)
import Text.Blaze.Html

main :: IO ()
main = apiGatewayMain pageIndex

pageIndex :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
pageIndex request = do
    page <- loadPage urlBase path
    return $ success page
    where
        path = HashMap.lookup "name" (request ^. agprqPathParameters)
        urlBase = fromMaybe "/" $ HashMap.lookup "url_base" (request ^. agprqStageVariables)

success :: Html -> APIGatewayProxyResponse Text
success page = responseOK 
    & responseBody ?~ page
    & agprsHeaders `over` HashMap.insert "content-type" "text/html"