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

success :: Styles -> Html -> APIGatewayProxyResponse Text
success styles body = responseOK 
    & responseBody ?~ renderPage styles body
    & agprsHeaders `over` HashMap.insert "content-type" "text/html"

pageIndex :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
pageIndex request = do
    let urlPath = request ^. agprqPath
    putStrLn (unpack urlPath)
    let path = HashMap.lookup "name" (request ^. agprqPathParameters)
    article <- loadArticle "index" (unpack $ fromMaybe "index" path)
    styles <- loadStyles
    return $ success styles article