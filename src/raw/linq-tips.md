As a big fan of Erik Meijer's I feel necessary to write about LINQ, although he himself might be thinking differently [about this topic.](https://twitter.com/headinthebox/status/813801779628380160)

There are fascinating things with LINQ that can elude even the most experienced users. Here I will try to share what I have learned.

## Infinity

We start with infinity.

```csharp
    var infinity = Infinity.Of(index => index); // the index can be useful
    // 0, 1, 2, 3 ...

    infinity.Take(10);
    // 10 elements

    infinity.Take(100000);
    // 100000 elements
```

Or, what's a fluent way to validate a password with 3 tries?

```csharp
		var authenticated = Infinity.Of(_ => {
			Console.WriteLine("What's the password?");
			return Console.ReadLine();
		}).Take(3).Any(pass => pass == "boss");
```

Isn't this magical? Well in fact it's only a simple use of laziness, or, the deferred execution model of LINQ.

Here is the implementation!

```csharp
public static class Infinity
{
    public static IEnumerable<T> Of<T>(Func<int, T> f)
    {
        var i = 0;
        while (true)
        {
            yield return f(i++);
        }
    }
}
```

To understand this, you'll need to learn about ``yield return`` ([here](https://docs.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/yield)).

Simply put:

* `yield return` does not really `return`, in fact, it does not even get executed. It does capture the state of computation at the moment it's called.
* execution only happens when the result `IEnumerable<T>` is iterated, such as in a `foreach` loop.
* now, LINQ acts on `IEnumerable<T>` fluently, without the need of an explicit `foreach` loop

All things combined - we get the magical `Infinity` helper!

## lazy, or eager?

One popular practice I regret to find in using LINQ methods, is to invariably evaluate immediately after a query, for example, `Select(...).ToArray()`.

While this does save us from undesired effects of LINQ, it basically reduces LINQ to any ordinary list comprehension methods, thus stopping us from getting the benefits of nuanced use of LINQ.

As an example, if we had evaluated Infinity eagerly, we wouldn't have been able to write the password validator:

```csharp
Infinity.Of(_ => {
  Console.WriteLine("What's the password?");
  return Console.ReadLine();
}).ToArray()... // <--- this never ends, or result in stack overflow
```

The beauty of lazy evaluation, is that it gives us control over when and how exactly evaluation can happen. And before we get to that point, there is much we can do to keep the evaluation to a minimum:

* `Any(predicate)` stops the evaluation as soon the `predicate` is satisfied, no matter how many elements the `IEnumerable` holds, same for `First()`
* filtering and mapping in the form of `Where()` or `Select()` also only happens to elements that's absolutely required for the final evaluation.

All this is obvious with the `Infinity` example - have a play!

So the moral is - embrace the laziness, and accept the power and the challenge.

## but when to evaluate eagerly?

In the example below, `ToArray()` becomes necessary (or even essential).

Assume that `GetPeople()` returns `IEnumerable<Person>`, which once evaluated, will call DB to query for a collection of `Person`.

```csharp

public IEnumerable<Person> Get()
{
  var people = DbRepo.GetPeople();
  return people;
}
```

This can catch developers out quite often and quite badly - if the result `IEnumerable<Person>` is not evaluated now, and later used several times, each time it's used will result in a query to the database, poor!

Easy fix: add `.ToArray()` after `GetPeople()`. Because we want the side-effect to take place now, so further evaluation on `people` will not hit the database.

Now more about side effects.

## keep out the side effects!

Developers who are used to `for` loops would usually try to shoehorn LINQ into loops, and inevitably, use FirstOrDefault() to emulate exiting a loop.

For example: update people's profile and stop on any error.

```csharp
people.FirstOrDefault(person => {
  if (updateProfileInDb(person))
    return true; // failed, exit loop!

  ...and More
  return false; // keep updating
});
```

This may seem smart, but I would argue is a form of abuse to `FirstOrDefault()`. Firstly, it reverts the semantics, error becomes true, and success becomes false. Secondly, and possibly most importantly, I am one to believe that LINQ is not meant for side-effects.

Side-effects is actually where a lot of grievances for LINQ are from, for example, people complain that nothing gets saved in database with the below code:

```csharp
people.Select(person => updateProfileInDb(person));
```

You would have guessed it: LINQ is lazy and because the result of the above `Select` is not used, the evaluation never takes place.

An easy fix:

```csharp
people.Select(person => updateProfileInDb(person)).ToArray();
```

While this forces the evaluation, it's hardly idiomatic - `Select` is supposed to transform a collection of data, but the result is here thrown away.

A more LINQ-idiomatic way to write this, in my opinion, is to simply use a `foreach` loop, as it's made for side-effects.

```csharp
foreach (var person in people) {
  updateProfileInDb(person);
}
```

And now a quiz: I believe there is a reason for LINQ to not come with a `ForEach` method, care to guess why?
