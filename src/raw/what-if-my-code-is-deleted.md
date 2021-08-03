One argument for writing unit tests to have more (if not 100%) code coverage is, "what if somebody deletes this piece of code by accident?"

Between accidental deleting of code and it getting into production, there should be something to stop it from happening, and here people would argue, unit testing is THE way to go.

This is obviously a very loose statement - unit testing is not the only defense against accidental code deletion, possibly not the most efficient either, most of the time.

## all languages are not created equal

I think it's fair to say with a typical dynamically-typed languages, one would more easily suffer from accidental code deletion, for relatively less (if not completely free of) compile-time checking. Errors might only appear when the program is executed, god forbid, in production. In practice, such accidents can be mitigated - but not fully prevented - with static analysis tools such as linting.

For example, in plain JavaScript,

```JavaScript
function greeting(name, language) {
    return format(name, language.toUpperCase());
}

// in a far away place

greeting('Hoy!', 'English');
```

Now assuming the argument `English` is deleted accidentally, resulting in `greeting('Hoy!')`. This is syntactically correct and may elude a routine code review and make its way to production. Ahoy! We have a runtime exception when `greeting` is called.

One answer to such unfortunate accidents is to bring the checking forward, so defects are found before production. One way to do that is to write automated tests to execute `greeting('Hoy')` to find the error sooner. 
The same goes for code refactoring. For lack of better alternatives, one can easily see how unit testing can become an imperative part of the engineering workflow here. Indeed, unit testing enjoys more popularity in dynamic languages users, and rightfully so.

In a statically typed language, for example C#, this is rarely a problem. A method with type `void Greeting(string name, string language)` can not be called without fully supplying both parameters, thanks to type checking by the compiler. One can still write a unit test to verify this is the case, but it wouldn't be too far fetched to call such tets unnecessary.

## all programs are not typed equal 

Static type checking does not rule out the possibility of accidental code deletion. In general, statements are easily scraped, expressions, much harder.

Below is poor defense against accidental code deletion,

```CSharp
void Greeting(string name)
{
    Console.WriteLine($"Hello {name}"); // not safe: delete this and no problem from the compiler
}
```

This is good defense,

```CSharp
string MakeGreeting(string name)
{
    return $"Hello {name}"; // safe: not possible to delete this without a compiler error
}
```

This example might look too obvious, but the difference between statements and expressions can be far-reaching: expression-oriented code does not only stand well against code deletion, it is also intrinsically easier to compose and therefore reason with.

Let's look at another example.

```CSharp
Response Handle()
{
    var request = new SomeRequest { ... };
    EnrichRequest(request);     // not safe 
    ValidateRequest(request);   // not safe
    return SendRequest(request);
}
```

Again, statements aren't too helpful here. However, if a conversion to expressions is carried out as below, we would have a different story.

```CSharp
Response Handle()
{
    var request = new SomeRequest { ... };
    FullRequest fullRequest = EnrichRequest(request);     // safe 
    ValidRequest validRequest = ValidateRequest(fullRequest);   // safe
    return SendRequest(validRequest);
}
```

With a few descriptive types to restrict pre- and post-conditions, this is obviously safe from accidental deletion. There is a hidden fact here - expressions are easily chained together, so the above method can be turned into a one-liner.

```CSharp
Response Handle()
{
    return SendRequest(ValidateRequest(EnrichRequest(new SomeRequest { ... })));
}
```

It's clear from the chained expression that none of middle steps can be removed, or we risk breaking the chain.

The sharp-eyed reader would have noticed - there is no error handling. That indeed is a big problem for composition. In a language like C#, it's possible to maintain such pleasant style by allowing something fishy - throwing exceptions anywhere in the code. A more potent solution is available in the likes of Haskell, where we can write code as below, thanks to the "do" notation for monads.

```Haskell
handle :: Either Error Response
handle = do
    let request = SomeRequest { ... }
    full    <- enrich request
    valid   <- validate full
    return $ send valid
```

## all code is not typed equal

A good type system can be a very strong, yet economical defense against code deletion.

For example, to create an object of getters from an object of primitive values in TypeScript, this is a valid solution,

```TypeScript
type Values = { foo: string, bar: number };
const vals: Values = { 
    foo: 'hello', 
    bar: 88 
};

function toGetters(v: Values) {
    return {
        foo: () => v.foo,   // not safe
        bar: () => v.bar    // not safe
    };
}
```

However, `toGetters` can not defend against accidental deletion as there are no constraints for "there must be fields foo and bar, both of type X".

Full defense is available below,

```TypeScript
type Getters = { [K in keyof Values]: () => Values[K] };
function toGettersSafe(v: Values): Getters {
    return {
        foo: () => v.foo,   // safe
        bar: () => v.bar    // safe
    };
}
```

Deleting any of the "// safe" lines will now result in a compiler error, as the `Getters` type ensures both "foo" and "bar" MUST be present in the return value.

## all code is not bound equal

What if I tell you it's possible to delete a whole function without any warning? No kidding.

### with an IoC container
If an IoC container uses a binding convention that binds implementations to interfaces by names, such as `RuleFactory` to `IRuleFactory`, then typically `RuleFactory` can be deleted without any compilation errors. Only at runtime would we get an exception that no implementation can be found for `IRuleFactory`. Another kind of popular errors is when there are (unintended) multiple implementations for one interfaces. 

Manual binding is not off the hook either. Typically a binding can be deleted without any errors before runtime, such as `container.Bind<IRuleFactory>().To<RuleFactory>()`.

### redux
In a typical `redux` architecture, by design, there is no way to guarantee all actions to have corresponding reducers, it's no surprise any reducers can be accidentally wiped out without the compiler ever knowing.

This is a sign for bigger problems. It's no coincidence people are finding `redux` harder and harder to keep up and maintain. An architecture that's so low in coupling loses out on cohesion, in another words, ease of understanding.

### other forms of late binding

It's not uncommon to see the use of so called "in-process messaging" architecture. Instead of calling a method directly for data, with such architecture, one first sends out a "query" for some sort of "queue", then immediately expects a handler to be invoked that sends the data back.

Imagine there is some sort of binding for the "query" to the "handler" to achieve some allegedly good "loose coupling". The problem is, one can now accidentally delete the binding, and no warning will be given until code is executed.

By now you would have seen the pattern - smart late-binding for the sake of "loose coupling" is not always our best friend. In fact, anything that is so smart to elude the scrutiny of the compiler kind of defeats the purpose of using a statically checked language, and should be watched out for, if not actively rejected.

## in summary

My preference is clear here: use expressions over statements; bind early, not late; use static over dynamic typing when there is a choice; and try to add stronger type constraints whenever possible.

These are done not just for bias or aesthetics (it is essential, make no mistake), but also for good economics. We can get away with writing fewer tests, which is also code and need to be maintained. But more deeply, such alternatives are usually better defense against not just accidental code deletion, but also bad code and illegal state.

After all, unit testing is not the only, let alone the best way to ensure correctness. It is a good last resort, albeit a bit heavy-handed, and should be reserved for cases of true complexity.

And yes, I do mean it - IoC containers and in-process pub-sub / messaging are all bad ideas. Superstition in "low coupling" without seriously considering "high cohesion" completely misses the point and is a recipe for sure disaster. This does not just apply on code level, but also architecture - yes I am looking at you "microservices".
