Managing transitive dependency, a typical example of accidental complexity in software packaging, has annoyingly and unfortunately become quite advanced knowledge. Solutions I could find vary from overly-specific, convoluted to hacky. A truly depressing state of affairs. 

Quite a lot of this annoyance can be solved quite trivially, and the solution is only so glaringly available and laughably simple, all it takes is slightly more foresight in making libraries or packages... you have guessed, more often than not, "function" is the answer, again!

# an example

Say I made a lovely server side application A, which uses library B and library C. Along comes the point of interest: A, B and C all use a library D, but in different versions.

- A uses D v3.0.0
- B uses D v2.0.0
- C uses D v1.0.0

If your package manager is not NPM who allows transitive dependencies to be hidden behind the direct dependency, but something like .Net that only allows a single version of D in the output, congratulations, you are in dependency hell.

# what's the REAL dependency

The question to ask, is what is the real dependency? What's the functionality being used from A, B and C?

More often than not, the answer is simpler than expected: if D is used so much, it is possibly some sort of cross-cutting concern. Think about logging, error handling, HTTP call, or as an example, serialisation, which can be boiled down to a simple function.

```CSharp
class Serialiser {
    Func<object, string> Serialize;
}
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
        // ... sends body via HTTP
    }
}
```

Now application A uses library B as,

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

What has happened here? Surprisingly, `LibD` is only ever used in application A; library B and C each requires `Serialize` through their OWN configuration types, and are both free of library D. Dependency hell is no more!

This is it, this is the trick. Pass dependencies in as function values.

You can stop here. What follows are just useless thoughts.

# is it first class?

What may underlie the difference between using a library as a dependency, and passing in function values, is libraries are not first-class in programming. One cannot write an `if/else` statement to decide if library B will depend on library D or not; it's all done quite unambiguously with a package manager. For example, either we run `nuget install LibD` or not; there is no other way.

On the other hand, Functions as values are first-class, which means they can be passed around and manipulated much like other types of values. In other example, we can swap out any implementation of `Serialize`, as long as it suits the type required by library B or library C.


# interface? No thanks

True users of Object-Oriented languages will demand an `ILibBConfig` here as below. 

```CSharp
interface ILibBConfig
{
    string Serialize(object o);
}
```

So application A can implement it with a class `LibBConfig`, to be passed in for `new LibB(instance of LibBConfig)`. This accomplishes the same effect, and may look more "proper", but if we look closely, there is an important difference between interfaces and function values. 

An interface like `ILibBConfig` is closed (especially in languages with a nominal type system). It must be implemented; once a implementation is done, it's hard to swap out one `Serialize` with another; indeed, one can use "design patterns" such as decorators, but they are fixes for the problem of closedness.

Let's take another look at `LibBConfig`.

```CSharp
class LibBConfig 
{ 
    Func<object, string> Serialize;
}
```

It simply wraps around `Serialize`, a field of type `Func<object, string>`. ANY function of this type can be assigned to `Serialize`, no matter what class or interface it lives in, that's how we managed to configure to use library D for library B and library C; in another word, `Serialize` is OPEN. If library B and library C both require `Func<object, string>`, there is no need to implement two interface; we simply pass along `LibD.Serialize`.

Here is the questions, are interfaces first-class? Can a class conditionally implement an interface? Usually not. So NO is most likely the answer.

# recommendations

If you are authoring a library, either for public or private use, think about dependencies.

Firstly, there should be rigour in deciding what should be considered a dependency for a library? Is it foundational to the functionality? Or is it something the library **uses** and can be swapped out for something else?

Understanding once a dependency is introduced, the constraints can be carried on to the users too, and can further constrain their options or make their lives hard.

Think twice before you require your users,

- to use a specific version of a dependency
- to implement a interface to satisfy requirements
- worst of all, to require the use of opinionated dependency injector / IoC container to wire up the interfaces and implementations in order to use the library

Instead,

- keep the contracts open by using plain data types
- allow customisation by requiring function values of specific types
- if ever necessary, make an more opinionated but separate variant of the library that integrates with specific dependencies.

In simple words: use functions, keep it open.