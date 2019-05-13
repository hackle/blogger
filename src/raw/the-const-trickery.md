If you are looking for a good example of taking something simple and turning it into something indistinguishable from magic, look no further, I have just the thing: the `Const` type.

## definition

The definition of `Const` is a simple one, as from `Data.Functor.Const`:

```haskell
newtype Const a b = Const { getConst :: a }
```

Note the second type parameter `b` is not used in the constructor. For example, `Const "a"`, which is of type `Const [Char] b`. 

One interesting effect of this, is that `b` can be interpreted as any type, we can try this out with `GHCi`.

```haskell
Data.Functor.Const> :t (Const "a" :: Const [Char] Int)
(Const "a" :: Const [Char] Int) :: Const [Char] Int

Data.Functor.Const> :t (Const "a" :: Const [Char] Bool)
(Const "a" :: Const [Char] Bool) :: Const [Char] Bool
```

Due to such strangeness as with the ignored `b`, `Const` is called a `phantom type`.

## as a Functor

With the above definition, its `Functor` instance is pretty routine but equally interesting.

```haskell
instance Functor (Const m) where
    fmap _ (Const v) = Const v
```

Remember `fmap :: (a -> b) -> (F a) -> (F b)`,  the above implementation thus specializes to type `fmap :: (a -> b) -> (Const x a) -> (Const x b)`, however, both `a` and `b` are discarded, so `(a -> b)` has nothing to act on, and is thus also discarded and replaced with `_`.

```haskell
Data.Functor.Const> (+1) <$>  Const "a"
Const "a"

Data.Functor.Const> length <$>  Const "a"
Const "a"
```

We can throw any function at it, in vain, a `Const` value is resistant to `fmap`.

## applicative

The Applicative instance is even more interesting. (Note for simplicity this is different than [the definition in Prelude](http://hackage.haskell.org/package/base-4.12.0.0/docs/src/Data.Functor.Const.html#line-82))

```haskell
instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    Const x <*> Const y = Const (x `mappend` y)
```

`m` is required to be a `Monoid`. Why? Well, remember the instance specializes to

```haskell
<*> :: Const x (a -> b) -> Const x (a) -> Const x b
```

As with the `Functor` instance, `(a -> b)`, `a` and `b` are ignored all together. But different than the `Functor` instance, we now have two values `x` and `y` to dispose of. What do we do with them? We can't just throw them away, or just keep one of them (we CAN but it'll be like *cheating*), One easy solution is just to somehow combine them, and what better way to express that than with `Monoid`?

```haskell
Data.Functor.Const> Const "a" <*> Const "b"
Const "ab"

-- but not this as Char is not a monoid
Data.Functor.Const> :t Const 'a' <*> Const 'b'
<interactive>:1:1: error:
    _ No instance for (Monoid Char) arising from a use of '<*>'
    _ In the expression: Const 'a' <*> Const 'b'
```

Hence the `Monoid` constraint. Makes sense.

## a trickery

Now the introduction is over, let's look at an intriguing use of `Const`, with the `lens` library as described [here](https://en.wikibooks.org/wiki/Haskell/Lenses_and_functional_references). Say we have a `Person` type.

```haskell
data Person = Person { name :: String } deriving (Show, Eq)
```

And to recap the famous `Lens` definition, double primed to avoid conflict,

```haskell
type Lens'' s t a b = forall f. Functor f => (a -> f b) -> s -> f t
```

We can define a lens `lname` to focus on `name` of `Person`. Note `fname` is a function that act on the `name` field.

```haskell
lname :: Lens'' Person Person String String
lname fname p = (\n -> p { name = n }) <$> fname (name p)
```

This innocuous `lname` updates a `Person` record, but wrapped in a `Functor` - as a matter of fact, **any** Functor. As a trivial example, we pass in `Identity.reverse` to reverse the name.

```haskell
> runIdentity $ lname (Identity . reverse) $ Person "Hackle"
Person {name = "elkcaH"}
```

Pretty straightforward, right? What is less so, is when we pass in `Const` instead,

```haskell
> getConst $ lname Const $ Person "Hackle"
"Hackle"
```

We get the name back, and the name only! What happened to the `Person`? Shouldn't it have been modified and returned? Well, it's been thrown away, thanks to `Cosnt`. Let's recap the implementation of `lname`,

```haskell
lname fname p = (\n -> p { name = n }) <$> fname (name p)
```

and put `Const` in place of `fname`, with a bit of partial application, we get,

```haskell
lname p = (\n -> p { name = n }) <$> Const (name p)
```

Now remember the implementation of `fmap` for `Const`? It will keep the `name` value through `fmap` as it's resistant to it. 

Clever right? For one thing, it certainly fits the grand scheme of `lens` pretty well.

## `Identity` and `Const` as one

It's well known that for `lens`, `Identity` is used for setting / overriding of values, and `Const` for viewing values (as shown above). However, a pretty well hidden secret is, they can be seen as two sides of the same coin. Which coin, you ask? Just the good old tuple, I say.

Taking a tuple `(a, b)`, `Const` can be seen as only needing `a`, as in `(a, undefined)`; `Identity`, on the other hand, only uses `b`, so it's isomorphic to `(undefined, b)`.

To see how this works let's make a couple of helper functions,

```haskell
mkIdentity a = (undefined, a)
mkConst a = (a, undefined)
```

Then we can use them in place of `Identity` and `Const` as follows:

```haskell
> snd $ lname mkIdentity $ Person "Hackle"
Person {name = "Hackle"}
>
> fst $ lname mkConst $ Person "Hackle"
"Hackle"
```

You would have noticed that `(a, b)` is both a `Functor` and an `Applicative` - that's why we get no complaints from `GHCi`.

Be warned though, when trying them out in `GHCi`, it won't work without applying `snd` and `fst`, as `undefined` will kick in. Thanks to laziness, if we avoid touching `undefined` in the tuple, there would be no exception.

The acute reader would have been screaming already - why use `undefined` at all? Just duplicate the value for the tuple as `(a, a)`! And we don't have to worry about `GHCi` blowing up. Indeed that works.

```haskell
> dup = \a -> (a, a)
> lname (dup . reverse) $ Person "Hackle"
("elkcaH",Person {name = "elkcaH"})
> fst $ lname (dup . reverse) $ Person "Hackle"
"elkcaH"
> snd $ lname (dup . reverse) $ Person "Hackle"
Person {name = "elkcaH"}
```

This of course is no coincidence. In essence `Identity`, `Const` and `(a, b)` are all product types and therefore have much in common. In practice we'd still be using `Const` and `Identity` as they are safer and more expressive, but if you find them confusing, then the above understanding may be helpful.

## summary
As with most things in Haskell, there is no real magic with `Const`, it's all about solid reasoning around simple, solid ideas. However, the use of such basic ideas, when combined with one another, can appear quite extraordinary.

To spot and use such trickery, we'll also need to build an intuition around it - which requires much practice; or at times, shortcuts may come in handy, such as tuple for `Identity` and `Const`.