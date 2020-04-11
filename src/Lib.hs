module Lib where

import AWSLambda.Events.APIGateway
import Control.Lens hiding (Context)
import Data.Semigroup
import Data.Text (Text, unpack, toLower, pack)
import Blog (loadPage)
import Text.Blaze.Html
import Data.Maybe
import qualified Data.HashMap.Strict as HashMap
import Aws.Lambda

handler :: APIGatewayProxyRequest Text -> Context -> IO (Either String (APIGatewayProxyResponse Text))
handler request context = do
    page <- loadPage urlBase path
    return $ return $ success page
    where
        path = unpack <$> HashMap.lookup "name" (request ^. agprqPathParameters)
        urlBase = unpack $ fromMaybe "/" $ HashMap.lookup "url_base" (request ^. agprqStageVariables)

success :: Text -> APIGatewayProxyResponse Text
success page = responseOK 
    & responseBody ?~ page
    & agprsHeaders `over` (("content-type", "text/html; charset=UTF-8"):)