If you are looking for a good example of taking something simple and turn it into something indistinguishable from magic, look no further, I have just the thing: the `Const` type.

## definition

The definition of `Const` is a simple one, as from `Data.Functor.Const`:

```haskell
newtype Const a b = Const { getConst :: a }
```

Note the second type parameter `b` is not used in the constructor. Which leads to interesting effect in its `Functor` instance. To construct a value, we simply do e.g. `Const "a"`, which is of type `Const [Char] b`. 

One effect of this, is that `b` can be interpreted as any type, we can try this out with `GHCi`.

```haskell
Data.Functor.Const> :t (Const "a" :: Const [Char] Int)
(Const "a" :: Const [Char] Int) :: Const [Char] Int

Data.Functor.Const> :t (Const "a" :: Const [Char] Bool)
(Const "a" :: Const [Char] Bool) :: Const [Char] Bool
```

Possibly because of the strangeness of `b`, `Const` is called a `phantom type`.

## as a Functor

With the above knowledge, its `Functor` instance appears pretty routine.

```haskell
instance Functor (Const m) where
    fmap _ (Const v) = Const v
```

Remember `fmap :: (a -> b) -> (F a) -> (F b)`, so the above implementation is specialized to type `fmap :: (a -> b) -> (Const x a) -> (Const x b)`, however, both `a` and `b` are ignored, which means that `(a -> b)` has nothing to act on and will be ignored too, hence its replaced with `_` above.

It works like 

```haskell
Data.Functor.Const> (+1) <$>  Const "a"
Const "a"

Data.Functor.Const> length <$>  Const "a"
Const "a"
```

We can throw any function at it, in vain, a `Const` value is resistant to `fmap`.

## applicative

As an applicative things get more interesting. (Note this is different than [the definition in Prelude](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Functor.Const.html#line-82))

```haskell
instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    Const x <*> Const y = Const (x `mappend` y)
```

`m` is required to be a `Monoid`. Why? Well, remember the type

```haskell
<*> :: Const x (a -> b) -> Const x (a) -> Const x b
```

As with the `Functor` instance, `(a -> b)`, `a` and `b` will be ignored all together. But different than the `Functor` instance, a value of `x` appears 3 times: twice in the arguments and once in the result. True its of the same type `x`, but its values can be different, as in `Const "a" <*> Const "b"`. What do we do with these values? The easiest solution is just to somehow combine them, and what better way to express that than `Monoid`?

```haskell
Data.Functor.Const> Const "a" <*> Const "b"
Const "ab"

-- but not this as char is not a monoid
Data.Functor.Const> :t Const 'a' <*> Const 'b'
<interactive>:1:1: error:
    • No instance for (Monoid Char) arising from a use of '<*>'
    • In the expression: Const 'a' <*> Const 'b'
```

## a trickery

One the most appalling use of `Const`, is with `lens`. Simply put, let say we have a `Person` type.

```haskell
data Person = Person { name :: String } deriving (Show, Eq)
```

And to recap the famous `Lens` definition, double primed to avoid conflict,

```haskell
type Lens'' s t a b = forall f. Functor f => (a -> f b) -> s -> f t
```

We can define `lname` to focus on `name` of `Person`.

```haskell
lname :: Lens'' Person Person String String
lname fname p = (\n -> p { name = n }) <$> fname (name p)
```

What's interesting is, looking at the definition of `Lens''` and then `lname`, we can easily spot that `lname` will update a `Person` record, as is evident if we pass in `Identity.reverse` to reverse the name.

```haskell
> runIdentity $ lname (Identity . reverse) $ Person "Hackle"
Person {name = "elkcaH"}
```

What's less obvious, is when we pass in a `Const`,

```haskell
> getConst $ lname Const $ Person "Hackle"
"Hackle"
```

We get the name back! But what happened to `Person` which should have been modified? Well, it's been thrown away! Let's recap the implementation of `lname`,

```haskell
lname fname p = (\n -> p { name = n }) <$> fname (name p)
```

and with equational reasoning, put `Const` in place of `fname`, we'll have

```haskell
lname p = (\n -> p { name = n }) <$> Const (name p)
```

Now remember the implementation of `fmap` for `Const`? It will keep `name p` through `fmap`. Isn't that clever? It certainly fits the grand scheme of `lens` pretty well. However I do understand when people complain that it's not straightforward enough.

## `Identity` and `Const` as one

It's well known that for `lens`, `Identity` is used for setting / overriding of values, and `Const` for viewing values (as shown above). However, a pretty well hidden secret is, they can be seen as two sides of the same coin. Which coin, you ask? Just the good old tuple, I say.

Looking at `(a, b)`, `Const` only needs `a` so it's like `(a, undefined)`. `Identity`, on the other hand, only uses `b`, so it's isomorphic to `(undefined, b)`.

To see how this works let's make a couple of helper functions,

```haskell
mkIdentity a = (undefined, a)
mkConst a = (a, undefined)
```

Then we can use then in place of `Identity` and `Const` as follows:

```haskell
> snd $ lname mkIdentity $ Person "Hackle"
Person {name = "Hackle"}
>
> fst $ lname mkConst $ Person "Hackle"
"Hackle"
```

Be warned when trying them out in `GHCI` - they won't work without applying `snd` and `fst` as `undefined` will kick in. Thanks to laziness, if we avoid touching `undefined` by picking the other value in the tuple, there would be no exception.

In practice we'd still be using `Const` and `Identity` as they are safer and more expressive, but if you find them a bit confusing, then the above understanding may be helpful.

## summary
As with most things in Haskell, there is no real magic, it's all about solid reasoning around solid basic ideas. However, the use of such basic ideas, when combined with other ones, can appear quite extraordinary.

We'll also need to build an intuition around it - which requires much practice, or at times, we can take the easy way out, such as tuple for `Identity` and `Const`.

