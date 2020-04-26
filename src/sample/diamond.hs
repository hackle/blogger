diamond :: Char -> IO ()
diamond c = mapM_ putStrLn $ build c
            
build :: Char -> [[Char]]
build c = makeRow <$> lettersToKeep
    where
        letters = ['A'..c] -- 1. A B C D
        squashedRow = (reverse $ tail letters) ++ letters -- 2. D C B ++ A B C D
        lettersToKeep = letters ++ (tail $ reverse letters) -- 3. A B C D ++ C B A, similar to above
        makeRow letter = (\l -> if l == letter then letter else '-') <$> squashedRow
