{-# LANGUAGE OverloadedStrings #-}
module Main where

import           AWSLambda.Events.APIGateway
import           Control.Lens
import qualified Data.HashMap.Strict         as HashMap
import           Data.Semigroup
import           Data.Text                   (Text, unpack)
import Blog
import Text.Blaze.Html

main :: IO ()
main = apiGatewayMain pageIndex

success :: Html -> APIGatewayProxyResponse Text
success body = responseOK 
    & responseBody ?~ renderPage body
    & agprsHeaders `over` HashMap.insert "content-type" "text/html"
    
pageIndex :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
pageIndex request = 
  case HashMap.lookup "name" (request ^. agprqPathParameters) of
    Just name -> do
      articlePath <- getArticlePath $ unpack name
      article <- readArticle articlePath
      return $ maybe responseNotFound success article
    Nothing -> return responseNotFound