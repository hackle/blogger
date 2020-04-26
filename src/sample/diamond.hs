import Data.Char

diamond :: Char -> IO ()
diamond c = do
    mapM putStrLn $ build c
    return ()
            
build :: Char -> [[Char]]
build c = keep <$> lettersToKeep
    where
        chars = ['A'..c] -- 1. A B C D
        row = (reverse $ tail chars) ++ chars -- 2. D C B ++ A B C D
        lettersToKeep = chars ++ (tail $ reverse chars) -- 3. A B C D ++ C B A, similar to above
        keep letter = (\l -> if l == letter then letter else '-') <$> row
