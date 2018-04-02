module Main where

import qualified Data.Aeson as Aeson

import AWSLambda (lambdaMain)

main = lambdaMain handler

handler :: Aeson.Value -> IO [Int]
handler evt = do
    putStrLn "This should go to logs"
    print evt
    pure [1, 2, 3]