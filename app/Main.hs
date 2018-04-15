{-# LANGUAGE OverloadedStrings #-}
module Main where

import           AWSLambda.Events.APIGateway
import           Control.Lens
import qualified Data.HashMap.Strict         as HashMap
import           Data.Semigroup
import           Data.Text                   (Text)
import           qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text
import Control.Monad (forM_)
import qualified Data.Text.Lazy as LT

main :: IO ()
main = apiGatewayMain hello

htmlBody :: H.Html
htmlBody = H.docTypeHtml $ do
  H.head $ do
    H.title "The hack blog"
  H.body $ do
    H.p "A list of topics"
    H.ul $ forM_ [ "Haskell"::Text, "F#", "QuickCheck" ] (H.li . H.toHtml)

textBody :: Text
textBody = renderHtml htmlBody & LT.toStrict

hello :: APIGatewayProxyRequest Text -> IO (APIGatewayProxyResponse Text)
hello request = do
  putStrLn "This should go to logs"
  case HashMap.lookup "name" (request ^. agprqPathParameters) of
    Just name -> 
      return $ responseOK 
        & responseBody ?~ textBody
        & agprsHeaders `over` (HashMap.insert "content-type" "text/html") 
    Nothing -> return responseNotFound
