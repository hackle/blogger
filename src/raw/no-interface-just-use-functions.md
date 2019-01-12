We'll see how functions can replace interfaces in the case of dependency injection, leading to less and cleaner code, and potentially make the use of dependency injectors and mocking frameworks unnecessary.

## example: `LatteMaker`:

Let's stick with C# - my favorite imperative language - for the example, a made-up `LatteMaker`.

```csharp
public interface ICoffeeBeanProvider
{
    CoffeeBean GetInGram(int grams);
}

public class CoffeeBeanProvider : ICoffeeBeanProvider 
{ 
    public CoffeeBean GetInGram(int grams)
    {
        // ...
    }
}

public interface IMilkProvider
{
    Milk GetInOunce(int ounces);
}

public class MilkProvider : IMilkProvider
{
    public Milk GetInOunce(int ounces)
    {
        // ...
    }
}

public interface ILatteMaker
{
    Latte Make();
}

public class LatteMaker : ILatteMaker 
{
    private readonly ICoffeeBeanProvider beanProvider;
    private readonly IMilkProvider milkProvider;

    public LatteMaker(ICoffeeBeanProvider beanProvider, IMilkProvider milkProvider)
    {
        this.beanProvider = beanProvider;
        this.milkProvider = milkProvider;
    }

    public Latte Make() 
    {
        var beans = this.beanProvider.GetInGram(10);
        var milk = this.milkProvider.GetInOunce(8);

        return new Latte(this.Grind(beans), this.Steam(milk));
    }

    // Grinding and Steaming algorithms not included
}
```

Regrettably, that's a lot of code! However, I hope the style of these **52** (keep this in mind) lines looks familiar to you - `LatteMaker` takes its dependencies in the form of interfaces through its constructor. Responsibilities are clearly separated. We can see this will work well with dependency injectors, and will be a breeze to unit test with the help of mocking frameworks.

## using interfaces as a popular practice

In object oriented programming, it's a common practice that for each concrete class, there is a corresponding interface for various "benefits". The example above represents that style.

But after a while, I couldn't help but finding such style repetitive.

* to inject `ICoffeeBeanProvider` and `IMilkProvider` to `LatteMaker`, I need to write them 3 times, respectively 
    - declaring them as fields, 
    - injecting them as constructor parameters, 
    - and assigning the arguments to the fields. 

    (P/S I love how `TypeScript` makes this more concise with constructor assignment)

* I am generally very cautious about blanket rules in the form of `every X must have a Y` - usually such rules are either wrong, or masks other problems, and result in either waste or bad solutions.

* what also occasionally bugs me, is that libraries and frameworks don't always use this style. For example, `System.Net.WebClient` does not implement an interface called `IWebClient`. Why? Surely Microsoft should champion best practices?!

## functions for less code

Looking closer at how `ICoffeeBeanProvider` and `IMilkProvider` are used, an easy observation is that essentially, all we need is the methods they provide - the interfaces does little else than wrapping around them. What if we just use the methods themselves, without the overhead of writing the interfaces?

This may sound outlandish to some, but `C#` has had support for first-class function types `Func<>`, `Action<>` as well as lambda for ages now, so do most other main-stream languages (even `Java` does!) these days. It's nothing strange to pass functions around. So, let's get started!

### inject functions to constructor

An easy first step is to inject functions instead of interfaces to the constructor of `LatteMaker`, so we get something like this:

```csharp
public class LatteMaker : ILatteMaker 
{
    private readonly Func<int, CoffeeBean> getBeansInGram;
    private readonly Func<int, Milk> getMilkInOunce;

    public LatteMaker(Func<int, CoffeeBean> getBeansInGram, Func<int, Milk> getMilkInOunce)
    {
        this.getBeansInGram = getBeansInGram;
        this.getMilkInOunce = getMilkInOunce;
    }

    public Latte Make() 
    {
        var beans = this.getBeansInGram(10);
        var milk = this.getMilkInOunce(8);

        return new Latte(this.Grind(beans), this.Steam(milk));
    }
    // ... 
}
```

To use `LatteMaker`, we simply do

```csharp
var beanProvider = new CoffeeBeanProvider();
var milkProvider = new MilkProvider();
var latteMaker = new LatteMaker(beanProvider.GetInGram, milkProvider.GetInOunce);
var latte = latteMaker.Make();
```

Not much difference to the original `LatteMaker` yet, but since now we don't depend on the interfaces `ICoffeeBeanProvider` and `IMilkProvider`, both can be deleted. Less code, always nice to see.

## remove the constructor

I am still not happy with the same repetition of *declare, inject and assign* for the dependent functions. 

Look at the present `LatteMaker`, it's obvious that both `Func`s are used only in the `Make` method. It's a lot of boilerplate to inject them through the constructor, then assign them to private fields, only for `Make` to pick up later. 

Why don't we just let `Make` accept the functions directly? Directness is a virtue, especially for programming. That turns out to be easy to do and now we have:

```csharp
public class LatteMaker 
{
    public Latte Make(Func<int, CoffeeBean> getBeansInGram, Func<int, Milk> getMilkInOunce) 
    {
        var beans = getBeansInGram(10);
        var milk = getMilkInOunce(8);

        return new Latte(this.Grind(beans), this.Steam(milk));
    }

    // Grinding and Steaming algorithms not included
}
```

To use it,

```csharp
var latte = new LatteMaker().Make(beanProvider.GetInGram, milkProvider.GetInOunce);
```

Surprisingly, the constructor and the private fields are all gone, the *declare, inject and assign* repetition is removed, we have much less code, but the `Make` method changed very little, and remains very straight-forward.

## static-ise

When a class needs no constructor, it's usually a good sign it can be made static. When a classes is made static, it becomes less stateful, and therefore much easier to reason with. Needless to say, I am an advocate for static classes.

A static `LatteMaker` looks like

```csharp

public static class LatteMaker 
{
    public static Latte Make(Func<int, CoffeeBean> getBeansInGram, Func<int, Milk> getMilkInOunce) 
    {
        var beans = getBeansInGram(10);
        var milk = getMilkInOunce(8);

        return new Latte(Grind(beans), Steam(milk));
    }

    // Grinding and Steaming algorithms not included
}
```

To use it,

```csharp
var latte = LatteMaker.Make(beanProvider.GetInGram, milkProvider.GetInOunce);
```

In other words, `LatteMaker` is now a namespace, and `Make` is now a pure function (presumed there is no side-effects in `Grind` or `Steam`). We have arrived on the land of functional programming.

And finally, our example is reduced to 29 lines of code from 52.

```csharp
public class CoffeeBeanProvider 
{ 
    public CoffeeBean GetInGram(int grams)
    {
        // ...
    }
}

public class MilkProvider
{
    public CoffeeBean GetInOunce(int ounces)
    {
        // ...
    }
}

public static class LatteMaker 
{
    public static Latte Make(Func<int, CoffeeBean> getBeansInGram, Func<int, Milk> getMilkInOunce) 
    {
        var beans = getBeansInGram(10);
        var milk = getMilkInOunce(8);

        return new Latte(Grind(beans), Steam(milk));
    }

    // Grinding and Steaming algorithms not included
}
```

## is anything lost?

Before we celebrate, it's important to make sure that we haven't done anything terrible. 

Have we lost any benefits of using interfaces / dependency injection via constructor? Let's go over some best practices:

### separation of concern
Each class still has the same responsibilities as before - just with much less code.

### dependency injection

`LatteMaker` does not depend on concrete instances of `CoffeeBeanProvider` or `MilkProvider`, in fact, instead of depending on specific interfaces, `LatteMaker.Make` requires functions that satisfy the specified type signatures only. This is a **significant** difference.

Think about it, any class can easily provide such functions, no matter what interface it does or does not implement. In fact, one does not have to use a method wrapped in a class, a lambda is sufficient for trivial cases, and requires even less code. In other words, for our example and potentially many similar scenarios, `interfaces are not necessary at all`!

Also - complaints about lack of support for **duck typing** in the likes of `C#` can now stop. The solution is as illustrated above: if an API is designed to accept functions instead of interfaces, then we get duck typing for free. Two interfaces with the exact same methods are not inter-exchangeable, but two methods with the same type signatures are.

### testability

Because we still use **dependency injection**, only through method `Make` instead of the constructor, unit testing `LatteMaker.Make` remains equally easy, if not more so, for that the necessity for mocking is completely removed, `Make` happily accepts lambdas as arguments, and lambdas are cheap to write and change.

## summary

Interface has its place in programming, but popular usage of interfaces for the convenience of dependency injection and unit testing results in a state of abuse, or as Martin Fowler terms, [header interface](https://martinfowler.com/bliki/HeaderInterface.html).

As in most main-stream languages, functions are first-class in `C#`. Replacing interfaces with functions results in much less code, simpler and more straight-forward style, as well as less stateful, more reasonable solutions.

Some best practices still apply, for example, naming the function signatures becomes more important as types don't always express everything (to be discussed in yet another post). 

In the above example,  `getBeansInGram` has the signature `Func<int, CoffeeBean>`, while `CoffeeBean` is straight-forward, `int` can mean anything such as age, height or times of winks to signal yes. Therefore it's important to reveal its meaning `gram` by the name `getBeansInGram`. (further reading: how F# makes this a breeze with [units of measure in F#](https://fsharpforfunandprofit.com/posts/units-of-measure/)).

Last but not least, this is not a novel invention of mine. One naturally comes to this realisation with enough time spent in functional programming. Let that be the moral of the story.
