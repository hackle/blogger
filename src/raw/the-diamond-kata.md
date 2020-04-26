The Diamond Kata is an exercise typically used for Test-Driven Development. Briefly, given a letter from A to Z - in the below example, letter D - to build a pattern like below.

```
---A---
--B-B--
-C---C-
D-----D
-C---C-
--B-B--
---A---
```

There are different solutions to this kata, typically with loops and sentinel values. Here I want to share a solution that can be slightly different.

First, build a table filled with letters in this fashion. We need to decide 

1. how to build a single row
2. how many rows to populate

```
DCBABCD
DCBABCD
DCBABCD
DCBABCD
DCBABCD
DCBABCD
DCBABCD
```

Now we only need to decide

3. what letter to **KEEP** in each row, replace other letters, and we are done.

With the table layout, it's easy to see the letters to keep across all rows form the same pattern as the letters in each row, but reversed.

```
DCBABCD <-- keep "A" only
DCBABCD <-- keep "B" only
DCBABCD <-- keep "C" only
DCBABCD <-- keep "D" only
DCBABCD <-- keep "C" only
DCBABCD <-- keep "B" only
DCBABCD <-- keep "A" only
```

Here is a simple solution in Haskell (which you'll agree with me is a great choice).

```haskell
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
```

In summary,
1. usually it's a good idea to spend some time looking at the problem closely, in this case, the result dataset. There can be patterns waiting to be discovered!
2. and it might not always the best idea to jump into coding without doing step 1 thoroughly, even with TDD!