
`in` and `out` in C# are good keywords for getting an intuition. For a different perspective though, let's move to Haskell.

## `Haskell`

In Haskell `Functor` is sometimes called a covariant functor. Upon hearing this, we know there are 2 things that change in the same direction. Which 2 things?

```haskell
class Functor f where
    fmap :: (a -> b) -> f a -> f b
```

See? They are `a -> b` and `f a -> f b`. So `Maybe a`, `List a` are covariant in `a`.

And there is also the **contravariant** Functor that goes

```haskell
class Contravariant f where
    contramap :: (a -> b) -> f b -> f a
```

See the direction is changed from `a -> b` to `f b -> f a`? A good example of `Contravariant` is predicates, 

```haskell
newtype Predicate a = Predicate { getPredicate :: a -> Bool }

instance Contravariant Predicate where
  contramap f g = Predicate $ getPredicate g . f
```

If you can ignore the wrapping and unwrapping, `Predicate` is just a function `a -> Bool`. Let's get an intuition: say I have a function `isEven :: Int -> Bool` to tell if a number is even, and we want to know if the length of a string is even, how can we do that? Well, first we need `length :: String -> Int`, and then compose `length` and `isEven`, we get `isEven . length :: String -> Bool`. In `Predicate` / `Contravariant` parlance, it looks like

```haskell
getPredicate $ length `contramap` Predicate isEven
```

It's not hard to figure out that we can swap `Bool` out for any other type to get a new instance of `Contravariant`, so what's the pattern here? Roughly put, for a function, `contramap` is just pre-composition (Note this is for illustration purpose and its syntax is not legit):

```haskell
contramap (f :: a -> b) (g :: b -> c) = g . f = flip (.)
```

On the other hand, post-composition is `fmap`

```haskell
instance Functor ((->) r) where
    fmap (f :: a -> b) (g :: r -> a) = f . g = (.)
```

And now, back to C#.

## `Haskell` again

Subtyping is not just a thing in Object Oriented Programming, it can (kind of) appear in Haskell too. For example, any `Monad` must be `Applicative`, and any `Applicative` must be `Functor`. Let's say we have the following (useless) functions. 

```haskell
{-# Language RankNTypes #-}

functor_to_int :: forall f. Functor f => f Int -> Int
functor_to_int = const 1

applicative_to_int :: forall f. Applicative f => f Int -> Int
applicative_to_int = const 1

any_to_int :: a -> Int
any_to_int = const 1

applicative_to_int__to_int :: (forall f. Applicative f => f Int -> Int) -> Int
applicative_to_int__to_int _ = 1

functor_to_int__to_int :: (forall f. Functor f => f Int -> Int) -> Int
functor_to_int__to_int _ = 1
```

`applicative_to_int__to_int functor_to_int` works, because there is an implicit "upcast" `Applicative -> Functor`, just like `Dog -> Animal`.

Not wo with `functor_to_int__to_int applicative_to_int`, because `Functor -> Applicative` is a "downcast" and there is no guarantee it will work.


## `C#` again

Let's try interpret the covariants and contravariants in C# with the understanding gained from `Functor` and `Contravariant`.

First `IEnumerable<out T>` can be seen as a `Functor` (it's actually very similar to `List`), what's its `fmap`? Here is how `IEnumerable<Dog>` can be assigned to `IEnumerable<Animal>`:

```haskell
fmap (upcast :: Dog -> Animal) IEnumerable<Dog> = IEnumerable<Animal>
```

And do we have the function `Dog -> Animal`? Well, it's an upcast, a `Dog` is an `Animal`. The other way around won't work because `Animal -> Dog` is a downcast and downcast is not always successful.

Now onto `Func<Animal>`, because it's a function, its `fmap` works just as in Haskell,

```haskell
fmap (upcast :: Dog -> Animal) (f :: () -> Dog) = upcast . f
```

So, `Func<Dog>` can be assigned to `Func<Animal>`.

And finally, why `Action<Animal>` can be assigned to `Action<Dog>`,

```haskell
contramap (upcast :: Dog -> Animal) (feed :: Animal -> ()) = feed . upcast
```

Again the intuition goes: `feed` can feed any `Animal`, `Dog` is an `Animal`, so `feed` can fee a `Dog`.

Now, back to Haskell.
