Covariance is just a fancy way to say one thing changes in the same direction as another, and Contravariance, the opposite direction. For example, if 5 people are to evenly share some apples, then the more apples there are, the more apples each person will get. In this case, each person's share and total of apples are covariant, as they increase or decrease together.

Now if we fix the number of apples, say, we have 20 apples only, then the more people there are to share amongst, the fewer each person will get. In this case, each person's share and the number of people are contravariant , as when the latter increases, the former decreases, and vice versa.

In programming with types, convariance and contravariance appear in different forms.

## `C#`

A popular example of covariant is `IEnumerable<T>`. Say we have

```csharp
public class Creature {}
public class Animal : Creature {}
public class Dog : Animal {}
```

And we can assign a value of `IEnumerable<Dog>` to a value of `IEnumerable<Animal>`.

```csharp
IEnumerable<Creature> creatures = Enumerable.Empty<Creature>();
IEnumerable<Dog> dogs = Enumerable.Empty<Dog>();
IEnumerable<Animal> animals = dogs;

animals = creatures; // <-- this won't compile
```

Type `Dog` is assignable to `Animal`, so `IEnumerable<Dog>` is assignable to `IEnumerable<Animal>`, `T` and `IEnumerable<T>` change in the same direction, and are therefore covariant! Or it's also said that `IEnumerable<T>` is covariant in `T`. 

You'd be thinking, if `IEnumerable<T>` is covariant in `T`, sure `List<T>` too? Well, not quite so. try,

```csharp
List<Animal> animals = new List<Dog>();
```

It won't compile! The difference is that `IEnumerable<T>`, is actually defined as `IEnumerable<out T>`, and `out` here specifics that `T` is the type of `output` and is therefore covariant. Is `output` necessarily covariant? Well yes but first let's look at `Action<T>` and `Func<T>`.

`Func<T>` is defined as:

```csharp
public delegate TResult Func<out TResult>();
```

See the `out` keyword? It's what makes `Func<T>` *covariant* in `T`. Now that you've seen `IEnumerable<out T>`, this example wouldn't be too surprising,

```csharp
Func<Animal> findAnimal = new Func<Dog>(() => new Dog());

// not the other way around!
Func<Dog> findDog = new Func<Animal>(() => new Animal());
```

If I need a way to *find* an `Animal`, and am given a way to find a `Dog`, I am happy - a `Dog` is an `Animal`. However, if I need to find a `Dog` but am given a way to find any `Animal`, it's clearly no good.

`Action<in T>` is the other way around. It's an easy guess that `in` means *input*.

```csharp
Action<Dog> feedDog = new Action<Animal>(a => Console.WriteLine("come eat!"));

// this won't work!
Action<Animal> feedAnimal = new Action<Dog>(a => Console.WriteLine("come eat!"));
```

I need a way to *feed* a `Dog`, and am given a way to feed any `Animal`, I am happy. However, if I need a way to feed any `Animal`, but am given a way to feed only `Dog`, then it's no good.

Analogies are nice, but what really is going on here? When in doubt, try code it. So here we go. Starting with `Func`:

```csharp
Func<Animal> findAnimal = () => ?? must be animal
```

What happens if we only have a `Dog` at hand? It's

```csharp
Func<Animal> findAnimal = () => new Dog() as Animal;
```

You'll note the upcast is not really necessary. Let's try `Action` next. First, `Action<Animal>` can act on `Animal` or its subtypes like `Dog`. `Action<Dog>` can act on `Dog` but not `Animal`. So how can we assign `Action<Animal>` to `Action<Dog>`? Let's say we have,

```csharp
Action<Dog> feedDog => dog => Console.WriteLine("woof!"); 
Action<Animal> feedAnimal = animal => Console.WriteLine("I am full!");
```
And need to define,

```csharp
Action<Dog> actOnDog => dog => ??
```

One option would be `dog => feedDog(dog)`, or, `dog => feedAnimal(dog as Animal)`, because again the upcast is for demonstration purpose.

This is even more interesting when we nest them, e.g. `Action<Action<Dog>>`.

```csharp
var dog = new Dog();
var animal = new Animal();
Action<Action<Dog>> nestedDog = f => f(dog);
Action<Action<Animal>> nestedAnimal = f => f(animal);
```

And watch this:

```csharp
Action<Action<Animal>> nestedAnimal = nestedDog;

// because 
Action<Action<Animal>> nestedAnimal = f => f(dog as Animal);
```

To grasp nesting fully, we'll need to read up on positive / negative positions - see the reference at the end. 

Covariant and contravariant can exist in delegates and interfaces, if used in an interface, there is a constraint - `T` in a `out T` type can only appear in *output* position, or as return type; `in T` only in *input* position, or as parameter type. If we look at interface `IEnumerable<out T>` again, it has only one method, `IEnumerator<out T> GetEnumerator ()`. Note `T` appears as a return type, wrapped in `IEnumerator<out T>`, just to make things easier :) So we have to look at `IEnumerator<out T>`, which has just a getter `T Current { get; }`, and `T` appears as a return type.

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

## further reading

If we've made good sense this far, try take it one step further and read about [positive and negative positions](https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance). Be warned, it can be a bit mind-bending!
