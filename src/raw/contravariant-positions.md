We'll continue to explore [covariance and contravariance](./contravariant) and see what happens if we nest them.

## recap

To recap the types we will be using:

```csharp
class Creature { }
class Animal : Creature { }
class Dog : Animal { }
```

And a simple covariance example, a sub type can be used where a super type is required:

```csharp
IEnumerable<Animal> animals = Enumerable.Empty<Dog>();
// or
Func<Animal> getAnimal = new Func<Dog>(() => new Dog());
```

and contravariant - a super type can be used in place of a sub type:

```csharp
Action<Dog> actOnDog = new Action<Animal>(ani => { });
```

## nested `Action`s

This is all very well, except when we nest them. For example, Can `Action<Action<Dog>>` be assigned to `Action<Action<Animal>>`, or is it the other way around? Let's see,

```csharp
Action<Action<Animal>> actOnActionOnAnimal = actionOnAnimal => ?;
```

As the type indicates, the parameter will be a `Action<Animal>` which it's called `actionOnAnimal` above. What can be done with this `Action`? One very obvious thing to do, is to apply it to an `Animal`.

```csharp
Action<Action<Animal>> actOnActionOnAnimal = actionOnAnimal => actionOnAnimal(new Animal());
```

But if it accepts an `Animal`, sure it accepts a `Dog`? Indeed this works.

```csharp
Action<Action<Animal>> actOnActionOnAnimal = actionOnAnimal => actionOnAnimal(new Dog());
```

However, it won't accept a `Creature` as below

```csharp
Action<Action<Animal>> actOnActionOnAnimal = actionOnAnimal => actionOnAnimal(new Creature());
// Error CS1503: Argument 1: cannot convert from 'Contra.Creature' to 'Contra.Animal'
```

As far as our types are concerned, a `Creature` is not necessarily an `Animal`. 

So while `Action<T>` is contravariant in `T`, `Action<Action<T>>` is covariant.

A potentially helpful analogy would be: suppose my job is to feed something, and my tool happens to be an "animal feeder". I need to use this tool to feed something - I can surely use it to feed animals, as its name indicates. I can also use it to feed dogs, as dogs are animals. But I can't use it to feed any creature. (A bit stretched I admit.)

## nested `Func`s

How about `Func`?

```csharp
Func<Func<Animal>, int> nestedFuncAnimal = f => 1;
Func<Func<Dog>, int> nestedFuncDog = f => 1;

nestedFuncDog = nestedFuncAnimal;
```

So while `Func<T>` is covariant in `T`, `Func<Func<T>` is contravariant. 

If we are to define `Func<Func<Dog>, int>`,

```csharp
Func<Func<Dog>, int> withDogGetter = dogGetter => dogToInt(dogGetter());
```

`withDogGetter` needs to somehow turn a `Dog` (as is returned from `Func<Dog>`) into an `int`, or, we could say, it's a `Func<Dog, int>` in disguise. `Func<Dog, int>` is actually **contravariant** in `Dog`, which means we can use a `Func<Animal, int>`.

Note I analysed `Action` and `Func` separately for ease of explanation. The reasoning would stay the same if we mix them up.

## positivity and negativity

As nesting gets deeper and deeper - why anybody would want to do that I have no idea - it's harder and harder to come up with analogies, and even if it's possible, the analogies would be quite stretched to the point of being more confusing than helpful (try google for Monad analogies!)

At the same time it becomes harder and harder to understand if we resort only to analogies and intuition.

Luckily, some smart people made it easy for us, with the notation of positive and negative positions. Below is how it works taking `Func` and `Action` as examples.

1. A parameter `T` is in a *positive* position if it's the return type, usually, the last type in the list of type parameters, such as in `Func<T>`, or `Func<int, T>` and so on.
2. `T` is in a *negative* position if it's not the return type, such as `Func<T, int>` or `Func<string, T, int>`. Note `T` is negative in `Action<T>` as `Action` always returns `void` which does not appear in the parameter list.
3. When function types are nested, positivity and negativity are changed as in multiplication. Let me explain.

In `Func<int, T>`, `T` is positive, as in a positive number, say `+1`. In `Func<T, int>` or `Action<T>`, `T` is negative as in `-1`.

Now in `Func<Func<int, T>, string>`, `T` is positive in `Func<int, T>`, but `Func<int, T>` as a type is negative in `Func<Func<int, T>, string>`. Now we multiply the numbers as in Algebra, `+1 * -1 = -1`, so `T` is negative in the whole type.

Or in `Action<Action<T>>`: `T` is negative in `Action<T>`, which in turn is also negative in `Action<Action<T>>`, when multiplied, `-1 * -1 = 1`, so `T` is positive in `Action<Action<T>>`.

Try it out yourself - it's pretty wicked!

## correct or useful

If you come this far you'd think this should be a thing for any static-type languages with support for generics (or parametric polymorphism). It's not always the case - for example, in TypeScript.

```typescript
class Creature {}
class Animal extends Creature {}
class Dog extends Animal {}

type Func<T> = () => T;

let dogGetter : Func<Dog> = () => new Dog();
let animalGetter : Func<Animal> = () => new Animal();
let creatureGetter : Func<Creature> = () => new Creature();

animalGetter = dogGetter;
console.log(animalGetter());

animalGetter = creatureGetter;
console.log(animalGetter());
```

This would look like an issue for correctness but in practice it's not as simple as black or white - there is a nice explanation [here](https://github.com/Microsoft/TypeScript/wiki/FAQ#why-are-function-parameters-bivariant) which I find very reasonable and insightful.

**CORRECTION**
This is only true with default function type checking mode. When `--strictFunctionTypes` is set, TypeScript checks function types for contravariance. See https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-6.html

## Summary

Understanding co- and contra-variance can help us design correct types when generics are involved, as well as troubleshoot or avoid *variance* problems - such problems can be hard to penetrate to the unknowing.

Nesting of variance is not really for the faint of heart! It is one of these things that are nice to know, but might not come up very often, or matter that much in real-life application development for most of us. In fact on the rare occasions that I had to use such concepts, I usually find it's best to steer away from them, and the solution would be cleaner and better off.