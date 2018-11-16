I've long since promised myself to write about callCc in Haskell. Not because I have a crystal understanding of its intricacies, but because I think it's such a thing of beauty and by writing about it, I will better commit it to my sieve-like memory. So, here we go...

(Reference: this is [where I first learned about callCC](https://en.wikibooks.org/wiki/Haskell/Continuation_passing_style). There is no way I can make a better writing on ``callCC`` - again I am only trying to capture my journey in comprehending it.)

## The types

Let's start with the ``Cont`` type:

```haskell
data Cont r a = Cont { unCont:: ((a -> r) -> r) }
```
This wraps around a continuation, which basically says, if you give me a function ``a->r``, you will get a ``r`` back.

What does this mean?

* first ``a`` must come from somewhere, and chances are, it will be encoded in ``Cont``.
* secondly the type of ``r`` is completely up to the consumer

That's when ``runCont`` comes handy:

```Haskell
runCont :: Cont r a -> (a -> r) -> r
runCont k f = unCont k $ f
```

Let's see an example:

```haskell
*Main> let c = Cont ($ 1)
*Main> runCont c (+ 2)
3
*Main> runCont c show
"1"
*Main> runCont c (\x -> [x])
[1]
```
So on and on, you get the idea. You'll notice it's pretty similar to call-backs.

## the ``Cont`` monad

Yes yes I hear you - everything has a chance to be a Monad until we prove otherwise. In fact let's go through the whole Functor - Applicative - Monad thing (manually), just to get used to the whole ``Cont`` idea.

### Functor

```haskell
instance Functor (Cont r) where
  a2b `fmap` m = Cont $ \b2r -> runCont m $ \a -> b2r (a2b a)
```
I've named the functions to better illustrate their types so save us a bit of guessing.

With this we can do:

```Haskell
*Main> runCont ((* 2) <$> Cont ($ 3)) id
6
```

### Applicative

```Haskell
instance Applicative (Cont r) where
  pure a = Cont $ \a2r -> a2r a
  ma2b <*> m = Cont $ \b2r -> runCont m $ \a -> runCont ma2b $ \a2b -> b2r (a2b a)
```

Again I try to keep it verbose here. It may appear a bit overwhelming with the levels of nesting. I find understanding ``runCont`` is essential here, it takes a function with type ``a -> r``, and the easiest way to get such function, is to make it by going ``\a -> ...try to get an r``,

### Monad

After ``Applicative`` hopefully ``Monad`` becomes straightforward here.

```Haskell
instance Monad (Cont r) where
  return = pure
  ma >>= a2mb = Cont $ \b2r -> runCont ma $ \a -> runCont (a2mb a) $ \b -> b2r b
```

Again I find explicit naming and verbosity really helpful here. Now we can do

```Haskell
*Main> let a2mb = \n -> Cont (\f -> f $ n * 2)
*Main> let mb = Cont (\f -> f 3) >>= a2mb
*Main> runCont mb id
6
```

## ``CallCC``

Below it's an implementation of the ~~in~~famous ``CallCC``:

```haskell
myCallCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
myCallCC a2mb2ma =
  Cont $ \a2r -> let  a2mb = \a1 -> Cont $ \b2r -> a2r a1
                      ma = a2mb2ma a2mb
                      in runCont ma $ \a2 -> a2r a2
```

I won't blame you if you find this a bit overwhelming... but in all honesty, I simply followed the types to complete the implementation... and by the time it type checks, I actually still couldn't fully comprehend what I have just written - but it works! Gotta love the type system. Well, let's try to analyse this! (Or skip to the next section if my analysis makes you dizzy).

* the name & type of ``a2mb2ma :: (a -> Cont r b) -> Cont r a`` is a strange one, to say the least. So there is this function ``(a -> Cont r b)``, which is parameter to another function that somehow to return a ``Cont r a``, and this as ``yet`` another function, is parameter to ``myCallCC`` that returns a ``Cont r a``... but wait, what happens to ``Cont r b``? Is it thrown away?

* now look at the implementation, ``a2r`` is applied twice, first time to satisfy the return types of ``Cont r b``, as we don't have any ways to get a value of type ``b``, so to satisfy ``b -> r``, we apply ``a2r a``. Which means ``b2r`` is actually thrown away!

* the second time a2r is applied, is to run ``ma`` which is the result of applying ``a2mb2ma``, the difference is, now we do have a value of type ``a``.

My gut feeling is - out these 2 applications, one of them is redundant.

Now I am not really one of these super smart people who can decode this, so I will need a dumb way to find out what's going on. In Haskell, luckily, thanks to purity, mostly of the time we can just use **[Equational Reasoning](http://www.haskellforall.com/2013/12/equational-reasoning.html)**.

## Equational Reasoning

Now first let's see ``myCallCC`` in action. Copied from the Reference page.

```haskell
quux = myCallCC $ \k -> do
    k 5
    return 25
```

And here comes the magic bit, if you run the result continuation...

```Haskell
*Main> let quux = myCallCC $ \k -> do { k 5; return 25 }
*Main> runCont quux id
5
```

So the Monad block ``do { k 5; return 25 }`` has been transformed by ``myCallCC`` to not execute ``return 25``. In fact, anything after invocation of ``k`` will be ignored. Isn't that the weirdest thing?! Oh well, let's find out why.

To get on with equational reasoning, first we expand the above call to ``myCallCC`` with the argument. It'll be lengthy but bear with me.

```haskell
quux =
  Cont $ \a2r -> let  a2mb = \a1 -> Cont $ \b2r -> a2r a1
                      ma = (\k -> do { k 5; return 25 }) a2mb
                      in runCont ma $ \a2 -> a2r a2
```

Try apply ``a2mb`` to ``\k...``

```Haskell
quux =
  Cont $ \a2r -> let  a2mb = \a1 -> Cont $ \b2r -> a2r a1
                      ma = do { a2mb 5; return 25 }
                      in runCont ma $ \a2 -> a2r a2
```

And expand a2mb

```Haskell
quux =
  Cont $ \a2r -> let  ma = do { (\a1 -> Cont $ \b2r -> a2r a1) 5; return 25 }
                      in runCont ma $ \a2 -> a2r a2
```

And apply ``5`` to ``\a1 ...``

```Haskell
quux =
  Cont $ \a2r -> let  ma = do { Cont $ \b2r -> a2r 5; return 25 }
                      in runCont ma $ \a2 -> a2r a2
```

And expand ``ma``,

```Haskell
quux =
  Cont $ \a2r -> runCont (do { Cont $ \b2r -> a2r 5; return 25 }) $ \a2 -> a2r a2
```

Now if we try to put ``\a2 -> a2r a2`` in place of ``b2r``, we'll find it's never used! So the above expression can be simplified as

```Haskell
quux = Cont $ \a2r -> runCont (do { Cont $ \_ -> a2r 5; return 25 }) undefined
```

Now if we go one level up and try to expand ``runCont quux id``:

```Haskell
runCont quux id = runCont (do { Cont $ \_ -> id 5; return 25 }) undefined
```

And let's bring it to the REPL:

```Haskell
*Main> runCont (do { Cont $ \_ -> id 5; return 25 }) undefined
5
```

Aha! That is it - the trick is in the ``do`` block, or, in the ``Cont`` Monad! For record's sake, let's desugar the whole ``do`` block:

```haskell
runCont $ (Cont \_ -> id 5) >>= (\_ -> return 25) ...
-- and according to   
-- ma >>= a2mb = Cont $ \b2r -> runCont ma $ \a -> runCont (a2mb a) $ \b -> b2r b

runCont $ Cont $ \b2r -> runCont (Cont \_ -> id 5) $ \a -> runCont (\_ -> return 25) $ \b -> b2r b ...

-- remember runCont k f = unCont k $ f
runCont $ Cont $ \b2r -> runCont (Cont \_ -> id 5) $ \a -> return 25 ...
-- again
runCont $ (Cont $ \b2r -> id 5) undefined

-- eventually
id 5
```

There we are! ``myCallCC`` demystified!

To recap:

* ``a2mb``/``k`` captures an argument (``5`` in our example) and turns that into a ``Cont``, which is special in that it will ignore any code after it in the do block.
* ``a2r`` is applied to the argument to ``a2mb``/``k``, in our example, value ``5``, but not to the seeming result - the last ``Cont`` in the ``do`` block - which is ignored in our example!

## Simplify

With the above finding, we can now simplify ``myCallCC`` to

```Haskell
myCallCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
myCallCC a2mb2ma =
  Cont $ \a2r -> let  a2mb = \a1 -> Cont $ \_ -> a2r a1
                      ma = a2mb2ma a2mb
                      in runCont ma undefined
```

It may seem a bit ungainly to leave an ``undefined`` at the end, but since it clearly shows the intent of ignoring any input in its place, I personally think it's fine.

## Summary

At first look, ``callCC`` might not be for the faint-hearted, but as we look closer and closer, there is no magic (although almost indistinguishable from it!) but a clever usage of ``Monad``.

In trying to comprehend it, Equational Reasoning proves a powerful tool to me. Unfortunately this is not the case for non-pure functions / languages. What that means to me - all the more reason for us to use pure languages like Haskell, or if that's not immediately possible, try to write as much pure code as we can!

You can find all the source code for this article [here](https://github.com/hackle/blogger/blob/master/src/sample/callcc.hs)
