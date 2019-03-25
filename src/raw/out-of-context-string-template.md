**Or, string templates are not magic strings.**

If a string template is placed away from its context, then the semantical benefit disappears and confusion arises. Such placement is a design smell.

## example

Many programming languages provide functions for simple string templating like that of `sprintf` in C. We'll use C# as example and here is the C# equivalent. (This style of string formatting is called [Composite Formatting](https://docs.microsoft.com/en-us/dotnet/standard/base-types/composite-formatting?view=netframework-4.7.2) in .NET)

### the Dumb

Straight from its [documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=netframework-4.7.2) with a slight adjustment.

```csharp
Decimal pricePerOunce = 17.36m;
String s = String.Format("The current price is {0:C2} per ounce.",
                         pricePerOunce);
```

(Exactly ounces of what? Microsoft is ever so mysterious...)

This is simple and clear, the kind of code I like.

### the "Best Practice"

Before long, we will hear some developers shouting, "magic strings!" And we will be shamed into extracting the string template to a constant value with a descriptive name:

```csharp
public class Foo 
{
    const string priceTemplate = "The current price is {0:C2} per ounce.";

    // .. constructors and other methods
    // in a method far below
    void Bar()
    {
        Decimal pricePerOunce = 17.36m;
        String s = String.Format(priceTemplate, pricePerOunce);
        // ...
    }
}
```

No more magic string, very good naming, everybody happy.

Except there is a small issue here: with `{0:C2}`, `priceTemplate` encodes a requirement for a valid number value. But in `Bar`, this requirement is now not immediately visible - one has to jump back and forth between the value of `priceTemplate` and `Bar` to understand the requirement. The experience of reading such code is a broken one.

Another way to put it is that this is a [leaky abstraction](https://en.wikipedia.org/wiki/Leaky_abstraction). We tried to abstract the template away by introducing a name `priceTemplate`. However, to consume `priceTemplate`, we have to look at its implementation (the value) to see that it requires a number - the abstraction is therefore broken. It actually makes the code harder to read and understand, and we would have been better off without such "best practice".

It does not stop here either - things can get even more interesting.

### the ultimate flexibility

Presumably, another developer sees this, and comes up with a brilliant idea: what if we make this template string fully configurable? Put it in a configuration file, or in the database, so we can change from 

**"The current price is {0:C2} per ounce."**

to 

**"The price is currently {0:C2} per ounce."**

without re-compiling the source code, flexibility!

Excellent idea, says everybody, let's do it. After hours of architectural design, coding, reviewing, testing and deployment, we put this in production:

```csharp
public class Foo 
{
    private string priceTemplate;

    public Foo(IConfig config) 
    {
        this.priceTemplate = config.ReadByKey("TemplateOfPrice");
    }

    // in a method far below
    void Bar()
    {
        Decimal pricePerOunce = 17.36m;
        String s = String.Format(priceTemplate, pricePerOunce);
        // ...
    }
}
```

Premium engineering. Except... 

- to understand `priceTemplate` and its requirements, we'll have to dig into configuration. Not exactly the most straight-forward.

- with a dynamic value loaded from config, any broken templates can only be discovered and tested at runtime. (versus string literals that can be unit tested or even checked by an IDE)

- finally, suppose we decided to put another slot in the template string, say, you guessed it, the subject of the price, in **the Dumb** way, this would be easy to do:

```csharp
String.Format("The current price for {0} is {1} per ounce.", item, pricePerOunce)
```

Not as easy with **the Ultimate Flexibility** design, in fact, we have to change both application code and configuration. Now suppose the configuration lives in a different place than the application, for example, in a database, we are heading into a race condition!

This might not be obvious yet but let's consider again the **old** version of `Bar()`

```csharp
void Bar()
{
    Decimal pricePerOunce = 17.36m;
    // expecting template: "The current price is {0:C2} per ounce."
    String s = String.Format(priceTemplate, pricePerOunce);
    // ...
}
```

And the **new** version to account for the new slot **item**.

```csharp
void Bar()
{
    String item = "gold";
    Decimal pricePerOunce = 17.36m;
    // expecting template: "The current price for {0} is {1} per ounce."
    String s = String.Format(priceTemplate, item, pricePerOunce);
    // ...
}
```

How exactly can we get a race condition? Well, simply put, 

* it's impossible to guarantee that the database and the application are updated at the **exact** same time (remember the assumption that they live in different places). So,

* if the string template is updated in database first, the application will be using the **new** template `The current price for {0} is {1} per ounce.` with **old** code, which does not provide any value for the **item** slot `{0}`,  leading to an runtime exception, e.g. `[System.FormatException: Index (zero based) must be greater than or equal to zero and less than the size of the argument list.]`.

* if application code is updated first, the **new** code will provide both `item` and `pricePerOunce`, but the template is still **old**, so we get an incorrect result, **"The current price is gold per ounce."**

Pretty nasty business isn't it?! After all, distributing `String.Format` and the **string template** turns out to be a pretty poor idea.

## string templates are not magic strings

String templates do carry behaviours, but in a well encoded, well documented and (hopefully) well accepted way, so it's not fair to consider it **magic**.

Even if we do call it magic, it's still a questionable practice to place it away from its site of use, making its requirements for data implicit, thus leaving the poor reader digging through source code (or worse, database) to understand what data to provide.

Therefore, it's more pragmatic and helpful to place the string template where it is consumed, to make requirements immediate and clear.

With that we now declare **out-of-context string template is an anti-pattern** because it damages context and introduces friction to understanding and maintaining source code.

## what if the template is re-used?

In the case of reusing a string template, we can create a function to hold the template, and the function can expose the data requirements as parameters. For example,

```csharp
string FormatPrice(string item, decimal pricePerOunce)
{
    return String.Format("The current price for {0} is {1} per ounce.", item, pricePerOunce);
}
```

We can then reuse `FormatPrice` instead of the string template.

## alternatives

### string interpolation

Whenever possible, string interpolation is a much more preferable solution. It allows inlining names to the template, for our example,

```
$"The current price for {item} is {pricePerOunce} per ounce."
```

This is much easier to read and maintain, and leaves little room for mistakes.

### big templates

In case of a large template with many slots / placeholders, inlining can be cumbersome and noisy. It is then more tempting to keep the template in a separate place.

It is recommended to place the template in a separate method, potentially wrapped in a class or namespace. Its required data are made explicit through the parameters of the method. Such as,

```csharp
static class XxxFormat 
{
    static string Bar(string item, decimal price, ...)
    {
        return $"The current price for {item} is {pricePerOunce} per ounce." +
                $"..." +
                $"and on and on and on";
    }
}
```

### configurable template

Requirement for a configurable template can occasionally crop up, in which case string interpolation can no longer help. We've seen that `String.Format` is a bad choice, so are we out of luck here?

Not yet. This is a typical use case for a **template engine**. Such engines offers formatting with fault tolerance (usually amongst other powerful features). Search for **template engine** and you'll find plenty.

## summary

String templates are not magic strings - they encode requirements that should be kept explicit rather than implicit. It's recommended to keep them at site of use. Placing them away from context is an anti-pattern.

Another similar anti-pattern, is to treat Regular Expression patterns as magic strings. I'll leave the fun of reasoning that out to you.

## further reading

F# provides a type-checked version of `sprintf`. See [this excellent write-up by Scott Wlaschin](https://fsharpforfunandprofit.com/posts/printf/)

In Idris you can make your own type-checked `sprintf`, thanks to Idris' support for dependent types. You can see my implementation (with limited feature set) [here](https://github.com/hackle/idris/blob/master/printF1.idr).