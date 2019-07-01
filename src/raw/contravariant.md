Have you ever noticed, in C# (or Java for that matter), `IEnumerable<Subtype>` can be assigned to `IEnumerable<Supertype>`, but not `IList<Subtype>` to `IList<Supertype>`? Well, that's co- and contra-variance in action.

## the basic idea

Covariance is just a fancy way to say one thing changes in the same direction as another, and Contravariance, the opposite direction. For example, if 5 people are to evenly share some apples, then the more apples there are, the more apples each person will get. In this case, each person's share and total of apples are covariant, as they increase or decrease together.

Now if we fix the number of apples, say, we have 20 apples only, then the more people there are to share amongst, the fewer each person will get. In this case, each person's share and the number of people are contravariant , as when the latter increases, the former decreases, and vice versa.

In programming with types, convariance and contravariance appear in different forms.

## IEnumerable<T>

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

Type `Dog` is assignable to `Animal`, so `IEnumerable<Dog>` is assignable to `IEnumerable<Animal>`, `T` and `IEnumerable<T>` change in the same direction, and are therefore covariant! We can also say that `IEnumerable<T>` is covariant in `T`. 

You'd be thinking, if `IEnumerable<T>` is covariant in `T`, sure `List<T>` too? Well, not quite so. try,

```csharp
List<Animal> animals = new List<Dog>();
```

It won't compile! The difference is that `IEnumerable<T>`, is actually defined as `IEnumerable<out T>`, and `out` here specifics that `T` is the type of output and is therefore covariant. What does `out` mean? Let's first let's look at `Func<T>` and `Action<T>`.

## `Func<T>`

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

Here is an analogy: if I need a way to *find* an `Animal`, and am given a way to find a `Dog`, I am happy - a `Dog` is an `Animal`. However, if I need to find a `Dog` but am given a way to find any `Animal`, it's clearly no good.

## `Action<T>`

`Action<in T>` is the other way around. It's an easy guess that `in` means *input*.

```csharp
Action<Dog> feedDog = new Action<Animal>(a => Console.WriteLine("come eat!"));

// this won't work!
Action<Animal> feedAnimal = new Action<Dog>(a => Console.WriteLine("come eat!"));
```

I need a way to *feed* a `Dog`, and am given a way to feed any `Animal`, I am happy. However, if I need a way to feed any `Animal`, but am given a way to feed only `Dog`, then it's no good.

This works for `Func` the same way, for example `Func<in T, string>`. In fact `Action<T>` is kind of like `Func<T, void>`. Of course C# won't allow using `void` as a type here - which is a bummer!

## try to make sense by coding them

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

One option would be `dog => feedDog(dog)`, or, if we have a `Dog` at hand, `dog => feedAnimal(dog as Animal)`. Again the upcast is only for demonstration purpose.

Through coding it, we can see that it's nothing magic, but how type safety for sub-typing works through various composite types.

## `IEnumerable<T>` vs `IList<T>`

We know because `T` in `IList<T>` is not marked as `out`, so we can't assign `IList<Dog>` to `IList<Animal>`, the next question is, why can't we make it `IList<out T>` so such assignments become possible? Wouldn't that make our lives easier?

Let's assume that is possible, so we can do 

```csharp
IList<Animal> animals = new List<Dog>();
```

That looks innocent enough until we do 

```csharp
animals.Add(new Animal());
```

This doesn't make sense - `animals` is actually a `List<Dog>` and it cannot accept an `Animal`!

`IEnumerable<T>` is different - it does not support a `Add` method, in fact it has only one method `IEnumerator<out T> GetEnumerator ()`. What's important here, is that the `T` also appears as **return** type. If we go one level deeper, `IEnumerator<out T>` also has only one getter, `T Current { get; }`, note `T` is the **return** type. 

So what's the pattern here?

For an interface to be covariant in `T`, `T` must be used only as **return** type in its methods or properties.

`IList<T>` does not follow that rule - it has a method `void Add<T>(T item)` with `T` appearing as **input** type.

Another way to look at this is - an interface just groups `Func`s or `Action` together. If `T` is covariant in all `Func`s that use `T`, then and only then does it makes sense for `T` to stay covariant. Same goes for contra-variant.

Properties can be considered similarly. A getter is a `Func<T>` and a setter `Action<T>`.

There you go - that's why `IList<T>` and `IEnumerable<T>` behave differently. We call `T` in `IList<T>` **invariant** as it is neither co- nor contra-variant.

## further reading

If we've made good sense this far, try take it one step further and read about positive and negative positions on [this blog](./contravariant-positions) or [explained in Haskell](https://www.schoolofhaskell.com/user/commercial/content/covariance-contravariance). Be warned, it can be a bit mind-bending!
