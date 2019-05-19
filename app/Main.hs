{-# LANGUAGE OverloadedStrings #-}
module Main where

import AWSLambda.Events.APIGateway
import Control.Lens
import qualified Data.HashMap.Strict as HashMap
import Data.Semigroup
import Data.Text (Text, unpack, toLower, pack)
import Data.Text.Encoding (decodeUtf8)
import Blog (loadPage)
import Text.Blaze.Html
import Data.Maybe
import Data.Aeson

main :: IO ()
main = apiGatewayMain pageIndex

pageIndex :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
pageIndex request = do
    page <- loadPage urlBase path
    return $ success page
    where
        path = unpack <$> HashMap.lookup "name" (request ^. agprqPathParameters)
        urlBase = unpack $ fromMaybe "/" $ HashMap.lookup "url_base" (request ^. agprqStageVariables)

success :: Text -> APIGatewayProxyResponse Text
success page = responseOK 
    & responseBody ?~ (getText $ toJSON page)
    & agprsHeaders `over` HashMap.insert "content-type" "text/html"
getText (String t) = t