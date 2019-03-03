If a string template is placed away from its context, then the semantical benefit disappears and confusion arises. Such placement is a design smell.

## example

Many programming languages provide functions for simple string templating like that of `sprintf` in C. We'll use C# as example and here is the C# equivalent.

### simple as

Straight from its [documentation](https://docs.microsoft.com/en-us/dotnet/api/system.string.format?view=netframework-4.7.2)

```csharp
Decimal pricePerOunce = 17.36m;
String s = String.Format("The current price is {0} per ounce.",
                         pricePerOunce);
```

(Exactly ounces of what? Microsoft is ever so mysterious...)

This is simple and clear, the kind of code I like.

### magic string and "best practice"

However, before long, there will be people shouting, "magic strings!" And I will be shamed into extracting the string template to a constant value with a descriptive name:

```csharp
public class Foo 
{
    const string priceTemplate = "The current price is {0} per ounce.";

    // .. constructors and other methods
    // in a remote method
    void Bar()
    {
        Decimal pricePerOunce = 17.36m;
        String s = String.Format(priceTemplate, pricePerOunce);
        // ...
    }
}
```

No more magic string, very good naming, everybody happy.

Except that to grasp what `s` will look like, now I need to scroll up to find `priceTemplate`, and replace `{0}` with `pricePerOunce` in my mind. A bit annoying. But hey, not the end of the world.

### complete flexibility

Another developer see this, and has a brilliant idea: what if we make this template string fully configurable? Put it in a configuration file, or in the database, so we can change from 

"The current price is {0} per ounce."
to 

"The price is currently {0} per ounce." 

without re-compiling! Magic!

Excellent idea, says everybody, let's do it. After hours of architectural design, coding, reviewing, testing and deployment, we put this in production:

```csharp
public class Foo 
{
    private string priceTemplate;

    public Foo(IConfig config) 
    {
        this.priceTemplate = config.ReadByKey("TemplateOfPrice");
    }

    // in a remote method
    void Bar()
    {
        Decimal pricePerOunce = 17.36m;
        String s = String.Format(priceTemplate, pricePerOunce);
        // ...
    }
}
```

Complete flexibility! Premium engineering. Except... now to understand  `priceTemplate`, we'll have to dig into configuration.

Suppose we decided to put another slot in the template string, say, you guessed it, the subject of the price, in the original way, this would be easy to do:

```csharp
String.Format("The current price for {0} is {1} per ounce.", item, pricePerOunce)
```

Not as easy with the **Complete Flexibility** design, in fact, we have to change both application code and configuration. Now suppose the configuration lives in a different place than the application, for example, in a database, we now have a race condition.

Consider again the previous version of `Bar()`

```csharp
void Bar()
{
    Decimal pricePerOunce = 17.36m;
    String s = String.Format(priceTemplate, pricePerOunce);
    // ...
}
```

And the updated version to account for the new slot **item**.

```csharp
void Bar()
{
    String item = "Quux";
    Decimal pricePerOunce = 17.36m;
    String s = String.Format(priceTemplate, item, pricePerOunce);
    // ...
}
```

What exactly is the race condition? Well, simply put, it's impossible to guarantee that the database and the application are updated at the **exact** same time (remember the assumption that they live in different places).

* if the string template is updated in database first, the application will be using "The current price for {0} is {1} per ounce." with old code, which does not account for **item**, leading to an incorrectly formatted string.

* if application code is updated first, now `String.Format` will provide both `item` and `pricePerOunce`, but the string template is still old, requiring only `pricePerOunce`, leading to a `FormatException`.

See the race condition there? After all, distributing `String.Format` and the **string template** turns out to be a pretty bad idea.

## alternatives

### string interpolation

### type checked format

## summary

Some magic strings are not really magical - they have assumptions / requirements that the callers must satisfy. Examples are,

* string templates assumes that callers know exactly how many and what types of arguments to pass in
* Regular Expression patterns assumes the runtime will use certain Regular Expression engines