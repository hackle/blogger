One way to comprehend the concept of Comonad, is to see it as a **context** plus a **focus**. Good examples are Conway's game of life, calculating [blur effect on a image](https://jaspervdj.be/posts/2014-11-27-comonads-image-processing.html), or "You Are The Average Of The Five People You Spend The Most Time With", or, what we can start with "Pass the ball in a circle".

## Passing the ball in a circle, Comonad in JavaScript

We want to write a program to record how a group of football players training together by passing the ball in a circle. It goes as "A passes the ball to B, B passes the ball to C ... E passes the ball to A, A passes the ball to B ...", you get the idea.

I am sure you can think of a good solution already but let's say the requirement is to use Comonad, so we need to find the **context** and the **focus**. 

I hope it's straightforward here - **context** is the group of players, A, B, C, D and E, and the **focus** is the **index** to the current player with the ball. With this in mind, we come to this nonchalant attempt in JavaScript, with some explicit naming.

```TypeScript
const players = [ 'A', 'B', 'C', 'D', 'E' ];
const passes = players.map((currentPlayer, focus, context) => `${currentPlayer} passes the ball to ${context[(focus + 1) % context.length]}`);

console.log(passes.join(', '));
```

This is almost a Comonad! We have both **context** and **focus**. But not quite - a Comonad instance really needs to encapsulate the focus, which brings us to the the `Haskell` version [here](https://github.com/hackle/comonad-matrix-conway/blob/master/src/Footy.hs) using `NonEmpty`.

```Haskell
players = fromList $ cycle [ 'A' .. 'E' ]

makePass :: NonEmpty Char -> String
makePass (a :| b : _) = a : " passes the ball to " ++ [ b ]

passes = extend makePass players
```

Do you see the **focus** of `NonEmpty`? It's a bit hidden as the "head" element, a neat trick.

Now let's look at `extend` which comes with any Comonad. Does it look awfully similar to `map`?

```Haskell
extend  :: (w a -> b) -> w a -> w b
map     :: (a   -> b) -> w a -> w b
```

`extend` takes the entire list `w a` as **context** when mapping to `b`. Because every single element still needs to be mapped, so we must build a different `w a` with a different **focus** to pass to the mapper function `(w a -> b)`. That's what `duplicate` does.

```Haskell
> import Control.Comonad
> import Data.List.NonEmpty
Control.Comonad Data.List.NonEmpty> duplicate $ 'A' :| ['B'..'E']
('A' :| "BCDE") :| ['B' :| "CDE",'C' :| "DE",'D' :| "E",'E' :| ""]
```

It builds a list of lists, with the "head" moving towards the "tail" for each list. Remember, "head" is the **focus** of the list. This gives rise to `makePass`. 

You will notice we also use `cycle` to build an infinite list so there is always at least 2 players for passing the ball. But be warned a `cycle`d list won't stop printing in GHCI if you try.

I hope that's clear enough.

## Comonad and Conway's game of life

Admittedly, for a long time I couldn't get an intuition for "contextual computation" that of a Comonad, until I see it used to solve Conway's game of life, in various ways. (I came up with the football example later on).

Now that we know `extend` is much like `map`, while the latter projects each element directly, `extend` projects the **context** `w a`. This alone makes `extend` a good match for Conway's game of life, where we'll remember, each cell dies or lives depending on its context (in the form of neighbouring cells).

Also now we know `w a` encapsulates a **focus**, so each call to `(w a -> b)` can result in a different `b` (or the same `b` so desired). To build a `w a` for each element `a`, we implement either `duplicate` or `extend`.

Thus [my solution to Conway's game of life](https://github.com/hackle/comonad-matrix-conway/blob/master/src/Game.hs) which should hopefully look straightforward by now.

```Haskell
type Coord = (Int, Int)
data Game a = Game { 
    coords :: Matrix a
    , focus :: Coord 
    } deriving (Functor, Show)

instance Comonad Game where
    extract (Game coords (r, c))= getElem r c coords
    duplicate g@(Game coords focus) = Game games focus where
        games = mapPos makeOne coords
        makeOne p _ = Game coords p

rule :: Game Bool -> Bool
rule g@(Game coords (r, c)) = liveNbrs == 3 || (isAlive && liveNbrs == 2) where
    isAlive = extract g
    liveNbrs = length $ filter id neighbours
    neighbours = isAliveOne <$> neighbourCoords `at` (r, c)
    isAliveOne (r, c) = maybe False id $ safeGet r c coords
```

Just to iterate on the above points,

* `Game` encapsulates a matrix
* the **focus** is an index into the matrix
* `duplicate` builds a matrix of `Game` each focusing on its own index
* `rule` takes advantage of `Game` as the context to calculate the total of live neighbouring cells

## Sexiness for nothing?

The discerning reader would have noticed we don't really need the sexiness of Comonad for Conway's game of life either, much like passing the football, we just need a `map` for matrix, and it's indeed [handy](https://mathjs.org/docs/datatypes/matrices.html#iterating). I'll leave it to you to figure out an implementation - it can be quite a bit less hassle than the Haskell version. 

Of course, Comonad as an abstraction can be much more powerful than just encapsulating an index in a list/matrix. There are more interesting instances such as `Store` or `ZipperList` as can be found in the "reference" section below.

## references

I first heard the idea of using Comonad for Conway's game of life casually mentioned in this excellent [Bartosz' video](https://www.youtube.com/watch?v=C5oogxdX_Bo), then I found implementations with [`Store`](https://stackoverflow.com/questions/45506813/performance-of-conways-game-of-life-using-the-store-comonad), more impressively [Representable Store by Chris Penner](https://chrispenner.ca/posts/conways-game-of-life), and 
[Zippers by Samuel Gélineau](https://github.com/gelisam/conway/blob/master/src/Conway.hs).

The `Zippers` is a mind bender and totally did my head in. The plain `Store` features conceptual clarity but suffers from performance penalty. It's better with `Representable Store` which has to represent `Vector` that is not itself [representable](https://bartoszmilewski.com/2015/07/29/representable-functors/) (warning: Category Theory mumbo), and I couldn't help wondering if it can be made more intuitive.

After staring at `extract :: w a -> a` and `duplicate :: w a -> w (w a)` long enough, I somehow find the intuition resemble that of fractals, for example Koch's snowflake (from [Wikipedia](https://en.wikipedia.org/wiki/Koch_snowflake#/media/File:Von_Koch_curve.gif)).

![Koch's snowflake](https://upload.wikimedia.org/wikipedia/commons/f/fd/Von_Koch_curve.gif)

Note though, `duplicate` usually only goes one level down (or else my head explodes) but I found the intuition helpful. 

Of course, the risk of being specific with any example for anything Haskell or Category Theory, is no example can fulfill the whole abstraction. So once you get an intuition for Comonad, it's then time to forget about **context** and **focus** (as far as it can take you!) and accept it as is - `(w a) -> w (w a)`, no more, no less.