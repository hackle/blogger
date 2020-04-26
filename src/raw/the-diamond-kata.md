The Diamond Kata is an exercise typically used for Test-Driven Development. Briefly put, given a letter from A to Z - in our example, D - the task is to build a pattern like below, with the letters forming a diamond shape.

```
---A---
--B-B--
-C---C-
D-----D
-C---C-
--B-B--
---A---
```

(SPOILER ALERT! If you are new to the Diamond Kata, this is the time to stop reading, and try it out first for yourself.)

Simple, visual and intuitive, this Kata lends itself perfectly to learning of testing techniques such as Property Based Testing. 

As we all know, the point of this Kata is not the solution itself, but how we arrive at it. Regardless, I want to share one that I found interesting, representative and equally intuitive.

Here is the idea: consider the above "diamond", what if we squash it from top to bottom into a single row, keeping only one letter for each column? We get `DCBABCD`.

Next, we replace each row in the table with this squashed row.

```
DCBABCD
DCBABCD
DCBABCD
DCBABCD
DCBABCD
DCBABCD
DCBABCD
```

What can we do with this table? Well, if we look and squint long and closely enough, we can see the "diamond" is hidden within! This is hardly any surprise - each row is a squashed "diamond" to begin with.

The problem is, how can we recover the "diamond"? Well, by removing the letters we don't need! Specifically, keep only 'A' in the first row, and 'B' in the second, and so on until 'D', then back to 'A'. 

```
DCBABCD <-- keep "A" only
DCBABCD <-- keep "B" only
DCBABCD <-- keep "C" only
DCBABCD <-- keep "D" only
DCBABCD <-- keep "C" only
DCBABCD <-- keep "B" only
DCBABCD <-- keep "A" only
```

And we arrive at...

```
---A---
--B-B--
-C---C-
D-----D
-C---C-
--B-B--
---A---
```

Doesn't get any easier, does it? To quickly recap, the keys to this algorithm are

1. making the **squashed** row
3. knowing what letter to **KEEP** in each row.

Isn't this strange? We haven't mentioned anything like loops, coordinates or sentinel values, and the solution is already jumping out at us!

Of course, by now, I trust you already have these 2 simple steps figured out, so it's time for you to open up your favourite IDE, and for me to bid you good luck...

Nonetheless, for completeness's sake, below is an implementation in Haskell (which you'll agree with me is a good choice).

```haskell
diamond :: Char -> IO ()
diamond c = mapM_ putStrLn $ build c
            
build :: Char -> [[Char]]
build c = makeRow <$> lettersToKeep
    where
        letters = ['A'..c] -- 1. A B C D
        squashedRow = (reverse $ tail letters) ++ letters -- 2. D C B ++ A B C D
        lettersToKeep = letters ++ (tail $ reverse letters) -- 3. A B C D ++ C B A, similar to above
        makeRow letter = (\l -> if l == letter then letter else '-') <$> squashedRow
```

As customary for any Kata, the moral of the story:

1. it's often a good idea to spend time on the problem at hand first. Sometimes, by simply staring at the dataset long enough, we can find patterns that lead to intuitive solutions.
2. conversely, it might not always be the best idea to dive into coding without doing step 1 thoroughly, with or without TDD.
