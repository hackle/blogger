## What do we want?

A way in C# to update data within nested data structure, that is safe from null references.

Given

```C#
public class Address 
{
	public string Street { get; set; }
}

public class Person
{
	public Address Address { get; set; }
}
```

And a SafeUpdater can satisfy the below assertions:

```C#
var addressUpdater = SafeUpdater.For<Person>(p => p.Address.Street);

var noAddress = new Person();
var unchanged = addressUpdater.Update(noAddress);
noAddress == unchanged;

var full = new Person { Address = new Address { Street = "Queen" } };
var changed = addressUpdater.Update(full, "Victory");
changed.Address.Street = "Victory";
```

## The problem

As far as our topic is concerned, Lens offers a way to reach through nesting to view and update data structure. (The mighty Simon Peyton Jones explains it the best [here](https://www.youtube.com/watch?v=wguYuQwjTtI)).

In a language like C# this is usually unnecessary - we use it mostly in a imperative way, and mutation is hardly frowned upon. There is also no algebraic data types that are better dealt with a great tool like Lens.

Although, in C# do we need to constantly deal with null values, when the values are nested, things become a bit painful.

In the same example, a usual way to update the street of the address of a person, is

```C#
// pretty bad
if (person == null) return;
if (person.Address == null) return;

// better, still verbose
if (person == null || person.Address == null) return;

// or, pretty good
if (person?.Address == null) return;

person.Address.Street = "Victory";
```

The last one is pretty good, I have to admit. But it would be nice to abstract over this too (at least as good exercise).

This also reminds us of [Law of demeter](https://en.wikipedia.org/wiki/Law_of_Demeter)

## Implementation

### The idea
Look at our specification, 

```C#
SafeUpdater.For<Person>(p => p.Address.Street).Update(person, "Queen")
```

to avoid null reference with 

```C#
p => p.Address.Street
```

we'll have to make it safe, or, to conditionally apply each dot, obviously, we need to make it an expression tree.

So the idea is - parse the expression tree, and evaluate each member access conditionally, if we can get to the end of the expression without any null values, then set through a PropertyInfo, otherwise (if there null before we reach the end) return and stop trying.