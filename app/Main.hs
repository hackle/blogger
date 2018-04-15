{-# LANGUAGE OverloadedStrings #-}
module Main where

import           AWSLambda.Events.APIGateway
import           Control.Lens
import qualified Data.HashMap.Strict         as HashMap
import           Data.Semigroup
import           Data.Text                   (Text, unpack)
import Blog

main :: IO ()
main = apiGatewayMain pageIndex

pageIndex :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
pageIndex request = do
  case HashMap.lookup "name" (request ^. agprqPathParameters) of
    Just name -> do
      articlePath <- getArticlePath $ unpack name
      article <- readArticle articlePath
      return $ responseOK 
        & responseBody ?~ (renderPage article)
        & agprsHeaders `over` (HashMap.insert "content-type" "text/html") 
    Nothing -> return responseNotFound