Dependency hell, a typical example of accidental complexity in software engineering, has annoyingly and unfortunately become some sort of "advanced problem". Solutions mostly focus on the consumers' side, varying from overly-specific, convoluted to hacky. A truly depressing state of affairs. 

However, if we look further up the chain at when a library (or package) is designed and created, quite a lot of this annoyance can be solved trivially, and the solution is only so glaringly available and laughably simple. You have guessed, more often than not, "function" is the answer, again!

# an example

Say I made a lovely server side application A, which uses library B and library C. Along comes the point of interest: A, B and C all use a library D, but in different versions.

- A uses D v3.0.0
- B uses D v2.0.0
- C uses D v1.0.0

If your package manager is not NPM who allows transitive dependencies to be hidden behind the direct dependency, but something like .NET that only allows a single version of D in the output, congratulations, you are in dependency hell.

# what's the REAL dependency

The question to ask, is what is the REAL dependency? Or, what's the KEY functionality used by A, B and C?

More often than not, the answer is simpler than expected: if D is used so much, it is possibly some sort of cross-cutting concern. Think about logging, error handling, HTTP call, or as an example, serialisation, which can be boiled down to a simple function.

```CSharp
// no generics here as that on a lone Func value is actually hard
Func<object, string> Serialize;
```

Knowing that, the authors of library B and C can remove the hard dependency on library D as a serialiser, instead they can ask their consumers to pass in a serialize function of their choice. Here is what library B can look like,

```CSharp
class LibBConfig 
{ 
    // why not a method? read on!
    Func<object, string> Serialize;
}

class LibB 
{
    public LibB(LibBConfig config)
    {
        this._config = config;
    }

    void SendHttpRequest()
    {
        var body = this._config.Serialize(dto);
        // ... sends request with body via HTTP
    }
}
```

Now application A uses library B as below,

```CSharp
var libD = new LibD();
var libBConfig = new LibBConfig { 
    Serialize = obj => libD.Serialize(obj)  // or more directly: Serialize = libD.Serialize
};
var libB = new LibB(libBConfig);
libB.SendHttpRequest();
```

Same goes for library C. Let me do a copy-paste.

```CSharp
var libCConfig = new LibCConfig { 
    Serialize = obj => libD.Serialize(obj)
};
var libC = new LibC(libBConfig);
libC.Hurray();
```

What has happened here? Surprisingly, `LibD` is only ever used in application A; library B and C each receives `Serialize` through its OWN configuration types, and are both free of direct dependency on library D. Dependency hell is no more!

This is it, this is the trick! Pass dependencies in as function values.

# is it first class?

What may underlie the difference between using a library as a dependency, and passing in function values, is libraries are not first-class in programming. One cannot write an `if/else` statement to decide if library D should be downloaded for the compilation of library B; it's all done quite unambiguously with a package manager. For example, either we run `nuget install LibD` or not; there is no other way.

On the other hand, Functions as values are first-class, which means they can be passed around and manipulated much like other types of values. In other examples, we can swap out any implementation of `Serialize`, as long as it suits the type required by library B or library C. So application A is free to choose whatever serialiser library there is.

# interface? No thanks

True believers of Object-Oriented programming will demand an `ILibBConfig` here as below. 

```CSharp
interface ILibBConfig
{
    string Serialize(object o);
}
```

So application A can implement it with a class `LibBConfig`, to be passed in for `new LibB(instance of LibBConfig)`. This accomplishes the same effect, and may look more "proper", but if we zoom in a little, there is an important difference between interfaces and function values (this is tautology - functions are usually values these days). 

An interface like `ILibBConfig` is closed (especially in languages with a nominal type system). It must be implemented; once a implementation is done, it's hard to swap out one `Serialize` with another; indeed, one can use "design patterns" such as strategies or decorators, but they are fixes for the problem of closedness, and are closed themselves.

On the other hand, functions are less closed. Let's take another look at `LibBConfig`.

```CSharp
class LibBConfig 
{ 
    Func<object, string> Serialize;
}
```

It simply wraps around `Serialize`, a field of type `Func<object, string>`. ANY function of this type can be assigned to `Serialize`, no matter what class or interface it lives in. That's why we could configure to use library D for library B and library C; in another word, `Serialize` is OPEN. If library B and library C both require `Func<object, string>`, there is no need to implement two interfaces; we simply pass along `LibD.Serialize`.

Are interfaces first-class? From the angle of "implementing" an interface, usually, a class cannot conditionally implement an interface. So NO is most likely the answer.

# recommendations

If you are authoring a library, either for public or private use, think about library / package dependencies.

Firstly, there should be rigour in deciding what should be considered a dependency? Is it foundational to the functionality? Or is it non-essential, but something the library **uses** and can be swapped out for something else?

Understanding once a dependency is introduced, the constraints can be carried on to the users, and can further constrain their options and make their lives hard.

Think twice before you require your users,

- to use a specific version of another library
- (not nearly as bad) to implement an interface to satisfy requirements
- worst of all, to require the use of opinionated dependency injector / IoC container to wire up the interfaces and implementations in order to use the library

Instead,

- keep the contracts open by using plain data types
- allow customisation by requiring function values of specific types
- if ever necessary, make an more opinionated but separately distributed variant of the library that integrates with specific dependencies, so users can bring it in at their own risk

In simple words: use functions, keep it open.