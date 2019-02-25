It's a well-known claim that static typing can help reduce amount of unit tests required. Let's see what this means with a few examples in different languages.

Disclaimer: this is in no way trying to fuel any debate over related subjects. If you are indeed in, or about to get into such a debate, read [What to know before debating type systems](http://blogs.perl.org/users/ovid/2010/08/what-to-know-before-debating-type-systems.html).

## example

We will try to write a simple function, `isValidPassword` that validates a given password satisfies two conditions:

- that it's indeed a string
- and its length is between 4 to 8 chars

For each attempt, we'll see how many unit tests are (roughly) required to validate its correctness.

## JavaScript

JavaScript gets tonnes of gibes for its type system, some unfairly for its being dynamic, others well-deserved (such as `[] + [] = ""`) for weird behaviors (or in another words, behaviors that require lengthy essays to explains). We use it here for its popularity and being dynamic.

```javascript
function isValidPassword(password) {
    if (typeof password !== "string") 
        throw "Password must be a string";
    
    return password.length >= 4 && password.length <= 8;
}
```

Being dynamically typed, the argument `password` can be of any type, be it number, boolean or Object. Thanks to the `typeof` operator, it's possible to filter out incorrect types. This does mean more work for unit testing, for which I'd throw in the following test cases.

1. positive cases
    
    1.1. a 4 chars string

    1.2. a 8 chars string

2. negative cases
    
    2.1. null
    
    2.2. undefined
    
    2.3. a number with 4 to 8 digits
    
    2.4. a boolean
    
    2.5. an object
    
    2.6. an empty string
    
    2.7. a 3 char string
    
    2.8. a 9 char string

That's **10** test cases. We'll use this as an inventory and refer to the test cases by their numbers.

## TypeScript

The TypeScript version is not much different except type annotation:

```typescript
function isValidPassword(password: string): boolean {
    if (null == password) 
        return false;
    
    return password.length >= 4 && password.length <= 8;
}
```

Already this helps a lot, maybe not so obvious in the function itself, but if we look at what test cases are needed for unit testing, we can see the following are not necessary now:
    
2.3. a number with 4 to 8 digits

2.4. a boolean

2.5. an object

Now we need **7** test cases, simply because the TypeScript compiler will prevent callers to pass in wrong types of data. For example, `isValidPassword(123)` gives us error:  `[ts] Argument of type '123' is not assignable to parameter of type 'string'. [2345]`.

(Of course it's possible to bypass type-checking with type coercion such as `isValidPassword(123 as any)` which we won't cover here).

## Haskell

This example hardly does any justice to the power of Haskell's type system, but here comes the Haskell version of `isValidPassword`.

```haskell
isValidPassword :: String -> Bool
isValidPassword password = l >= 4 && l <= 8
                 where l = length password
```

Succinct isn't it?! 

It's impossible to pass in anything other than a valid `String`. In Haskell `null` is not a thing any more. To account for values that are optional, the `Maybe` type is used which can be `Just a` or `Nothing`. To consume a value of this type, both cases must be checked.

Now we don't need these test cases:

2.1. null

2.2. undefined

**5** test cases left.

(You'll be surprised that `undefined` is actually a thing in Haskell - but it's neither passed around often, nor would it appear accidentally).

## Idris

With very similar syntax, the Haskell version can be easily converted to Idris by replacing `::` with `:`. That way though, we'll miss out on the power of dependent types that Idris is best known for. To utilise this power, behold the Idris version, 

```idris
data ValidPassword : (min: Nat) -> (max: Nat) -> Type where
  MkPass: (xs: String) ->
          { auto p: length xs >= min && length xs <= max = True } ->
          ValidPassword min max
```

You would probably find it mind-boggling this is not a function, only a type definition, but it has conditions set in its parameter in the form of a proof `{auto p: ...}`, stating that to successfully construct a value of type `ValidPassword`, the length of the password `xs` must be within range of `min` to `max`. 

Below is an example of making a `ValidPassword` between 4 to 8 characters,

```idris
validPassword : ValidPassword 4 8
validPassword = MkPass "abcd"
```

If we were to make a password with 3 characters only, say `MkPass "abc"`, the code won't compile.

```idris
When checking right hand side of validPassword with expected type
        ValidPassword 4 8

When checking argument p to constructor Main.MkPass:
        Can't find a value of type 
                False = True
```

The error message is basically saying that the required proof is not satisfied, so a `ValidPassword` cannot be constructed. A similar error goes for a password longer than 8 characters.

Now look at the list of test cases, it's obvious that **none** of them are required - if the code compiles, it must have satisfied all test cases.

Be warned though - such power does not come for free. If we are to construct a `ValidPassword` from a dynamic value (such as one from user input at runtime), we'll need to decide whether such value satisfy our proof or not. See [here](https://github.com/hackle/idris/blob/master/range.idr) for a full example that accepts user input.

## summary

Static type systems can move the work of validating correctness of code from runtime to the compiler, therefore reduce the number of unit testing required. In the case of powerful languages like Idris, when enough proofs are given, unit testing can even become unnecessary. As Bartosz Milewski puts it, [Testing is a poor substitute for proof](https://bartoszmilewski.com/2014/11/24/types-and-functions/).

However, proofs do not come for free and can be non-trivial to provide. So what language to use is never a black / white situation. 

If correctness is not the top priority and we just want to get things done, it can be more pragmatic and productive to go with a dynamically typed language. On the other hand, if correctness is essential, we would gain stronger confidence with more powerful type systems. 

Most of us write programs that fall in the middle, therefore the choice is usually not only technical. However, if everything else being equal, I personally prefer to write code with stronger static typing over exhaustive unit testing for 100% code coverage - which I dread, but know others celebrate with different perspectives.