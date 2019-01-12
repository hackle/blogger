We explore the difference between pattern matching and passing in functions as arguments, and see how it makes a lot of sense in other languages such as C#, so we might be able to get away from null checks.

## exhaustive pattern matching

> I hate pattern matching... just pass in functions!

Paraphrased, Erik Meijer said this many times (definitely in [this great series](https://channel9.msdn.com/Series/C9-Lectures-Erik-Meijer-Functional-Programming-Fundamentals/Lecture-Series-Erik-Meijer-Functional-Programming-Fundamentals-Chapter-1) although I cannot place where exactly) to my utter perplexion - what's wrong with pattern matching? Especially when compilers for languages like `Haskell` and `Idris` can enforce exhaustive matching (or totality)?

```haskell
printMaybe :: Maybe String -> String
printMaybe Nothing = "Nothing"
```

now try to load this up in `GHCI` with the `-W` option to turn on warnings.

```haskell
*Main> :set -W
*Main> :l pattern-matching.hs 
[1 of 1] Compiling Main             ( pattern-matching.hs, interpreted )

pattern-matching.hs:2:1: warning: [-Wincomplete-patterns]
    Pattern match(es) are non-exhaustive
    In an equation for 'printMaybe': Patterns not matched: (Just _)
|
2 | printMaybe Nothing = "Nothing"
| ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
Ok, one module loaded.
```

To avoid this warning, we have to add pattern matching for `Just String`:

```haskell
printMaybe :: Maybe String -> String
printMaybe Nothing = "Nothing"
printMaybe (Just s) = s
```

To consume this function, suppose we have another function `makeMaybe`.

```haskell
makeMaybe : String -> Maybe String
makeMaybe s = if s == "Nothing" 
                    then Nothing 
                    else Just s
```

To hook it up to `printMaybe`, we simply do `printMaybe $ makeMaybe "some string"`.

Nothing wrong with this right? In fact I think it's just beautiful. Still, let's see the other way of doing things.

## passing in functions

What does it look like if we don't pattern match, and just pass in functions? First we need a function `useString`:

```haskell
useString :: String -> (String -> a) -> a -> a
useString str onJust onNothing =
    if str == "Nothing"
        then onNothing
        else onJust str
```

And it's used as
```haskell
*Main> useString "Nothing" id "None"
"None"
*Main> useString "Something" id "None"
"Something"
```

But notice now the result type is open, so it's also possible to do
```haskell
*Main> useString "Something" length 0
9
*Main> useString "Nothing" length 0
0
```

Do you see the difference?

* by using continuation passing style aka `CPS`, we gain more flexibility - the function can now return any type
* strangely, there is no need for the `Maybe` type now

I don't know if this is a strong enough selling point in abandoning pattern matching for `CPS`, but before we draw any conclusions, let's see how this works in a different language e.g. C#.

## how pattern matching works in `C#`

I consider all reference types in `C#` as `Maybe` type. For example, `String` is actually a union of `String` and `null`. So the action of null check is roughly comparable to pattern matching. For example, we want to reverse a string.

```csharp
static String ReverseString(string str)
{
    if (str == null)
        return null;
    else
        return new String(str.Reverse().ToArray());
}
```

See the pattern (pun intended) there? Sure we `C#` programmers do null checks a dozen times a day, at least! Or we'll get the dreaded `NullReferenceException`.

The question is - if this is the equivalence of pattern matching in `C#`, then what's it like to pass in functions? As you've guessed, first there is `UseString`.

```csharp	
static T UseString<T>(string str, Func<String, T> onValid, T onNull)
{
    if (str == null)
        return onNull;
    else
        return onValid(str);
}
```
`onValid` acts only on valid string, and `onNull` is returned if `str` is null. Straightforward.

Then to reverse a string using this function.

```csharp
static String ReverseStringCps(string str)
{
    return UseString(str, s => new String(s.Reverse().ToArray()), null);
}
```

Do you see how `ReverseStringCps` does no null check? It becomes even more obvious when, for example, we now need to get the length of the reversed string, with `ReverseString`:

```csharp
static int GetLength(string str)
{
    var reversed = ReverseString(str);
    if (reversed == null)
        return 0;
    return reversed.Length;
}
```

Arrhhh... we need to null check again? But with the `CPS` version:

```csharp
static int GetLengthCps(string str)
{
    return UseString(ReverseStringCps(str), s => s.Length, 0);
}
```

No more null check! We just need to pass a function (and a default value `0`) to `UseString`, because there is guarantee from `UseString` that the `onValid` function will always act on a valid string, and in the case that the string is null, `onNull` will be returned. 

## summary
Very simple trick inspired by Erik Meijer's "hatred" towards pattern matching. In languages with support for exhaustive pattern matching, this may only be a choice of programming style, as either way we can get strong confidence. However, in languages without such support like C#, this pattern can be very handy as it saves us the pain of null checks, or worse, the pain of forgetting to do so.

There are options to make this pattern more generic so we don't need to make `UseCustomer`, `UseProduct` `UseXYZ` etc etc.

It's also possible to make this more fluent, such as with extension methods.

Of course one can naturally runs into the problem of `void` not being a real type - but I'll leave all these to you to explore. 

Have fun hacking!