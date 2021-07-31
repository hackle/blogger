One argument for writing unit tests to have more (if not 100%) code coverage is, "what if somebody deletes this piece of code by accident?"

Between deleting of code and it getting into production, there should be something to stop it from happening, and here people would argue, unit testing is THE way to go.

This is obviously a very loose statement - unit testing is not the only way to defend against accidental code deletion, it's also not the most efficient, most of the time.

## all languages are not created equal

I think it's fair to say with a typical dynamically-typed languages, one would more easily suffer from accidental code deletion, for relatively less (if not completely free of) compile-time checking. Errors might only appear when the program is executed, god forbid, in production. (In practice, such accidents are mitigated (maybe not fully prevented) usually with static analysis tools such as linting).

For example, in JavaScript

```JavaScript
function greeting(name, language) { ... }

// in a far away place

greeting('Hoy!', 'English');
```

Let's say one argument is deleted accidentally, resulting in `greeting('Hoy!')`. This is syntactically correct and may elude a routine code review and make its way to production, resulting in runtime exceptions or unexpected application behaviour.

One answer to such unfortunate accidents is to bring the checking forward, so defects are found before production. One way to do that is to write automated tests to execute `greeting('Hoy')` to find the error sooner. 
The same goes for code refactoring as well. For lack of better alternatives, one can easily see how unit testing can become an imperative part of the engineering workflow here, and it's no secret that unit testing enjoys more popularity in dynamic languages users.

In a statically typed language, for example C#, this is rarely a problem. A method with type `void Greeting(string name, string language)` can not be called without fully specifying both parameters, thanks to type checking by the compiler.

## all programs are not typed equal 

Static type checking does not rule out the possibility of accidental code deletion. In general, statements are easily scraped, expressions, much harder.

This is poor defense,

```CSharp
void Greeting(string name)
{
    Console.WriteLine($"Hello {name}"); // delete this and no problem from the compiler
}
```

This is good defense,

```CSharp
string MakeGreeting(string name)
{
    return $"Hello {name}"; // not possible to delete this without a compiler error
}
```

This might look obvious, but the difference between statements and expressions can be far-reaching: expression-driven code does not only stand well against code deletion, it is also intrinsically easier to compose and therefore reason with.

## all code is not written equal

## all code is not bound equal

What if I tell you it's possible to delete a whole function without any warning? No kidding.

### with an IoC container
If an IoC container uses a binding convention so that implementations are bound to interfaces by names, such as `RuleFactory` to `IRuleFactory`, then typically `RuleFactory` can be deleted without any compilation errors.

### redux
In a typical `redux` architecture, by design, there is no way to guarantee all actions to have corresponding reducers, it's no surprise any reducers can be accidentally wiped out without the compiler ever knowing (note redux is used not just for web front-end, which can be checked by TypeScript these days).

### other forms of late binding

It's not uncommon to see the use of so called "in-process messaging" architecture. Instead of calling a method directly for data, with such architecture, one first sends out a "query" for some sort of "queue", then immediately expects a handler to be invoked that sends the data back.

Imagine there is some sort of binding for the "query" to the "handler" to achieve some allegedly good "loose coupling". The problem is, one can now accidentally delete the binding, and no warning will be given until code is executed.

By now you would have seen the pattern - smart late-binding for the sake of "loose coupling" is not always our best friend. In fact, anything that is so smart to elude the scrutiny of the compiler kind of defeats the purpose of using a statically checked language, and should be watched out for, if not actively rejected. Yes, I do mean it, IoC containers are bad.