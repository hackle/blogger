It's may seem obvious where unit tests should live, until we start looking at the alternatives - and there come the pleasant surprises.

For succinctness' sake, let's use "code" for "code under test".

# separate code and test

Having spent a lot of time on C#, I used to take it for granted that all tests, including unit tests, deserve their own separate project.

This can look completely natural but sooner or later there will be awkwardness. Typically, 

- the folder/file structure of the unit test project will mirror those projects being tested, if a file is moved/created in the source projects, so must it be in the test project
- usually new dependencies in the source project will also come into the test project, and the dependencies must be kept in sync
- for special tests (whether they are "unit tests" can be debated) that rely on the configurations of the "current" project being the startup/entry point, such configurations must also be available in the test project

These symptoms are all some form of duplication. As the size of the projects grows, so do the symptoms and the burden of maintenance. 

# test alongside code 
When I worked on a front-end (AngularJS) codebase, it was pleasant to find unit tests (specs) are placed alongside code under test. This greatly increases the visibility and significance of tests in a codebase. These questions are answered trivially,

- are there tests for `foo.js` == is there a `foo.spec.js` right next to it?
- should I take unit testing seriously? Looks like I must - they are right in my face so they must be first-class!
- is it easy to move both code and tests? Sure, they live right next to each other.

Later I found it's also conventional to have a `__test__` folder within each folder to house tests. Such seemingly innocuous naming in effect pushes tests a step towards obscurity, and it's a form of separation so it can suffer from some of the symptoms as with separate projects.

P/S it's great that front-end codebase don't overdo the projects shenanigans. 

# REPL
An honourary mention goes to the use of any REPL, as it's in my opinion one of the best techniques to get feedback about code. 

The ability to try out a piece of code by copy-pasting into (if not sending directly to) the REPL, and to see immediately if it works or not, without completing the whole program, it's massively undervalued.

This lends naturally to compositional problem-solving, and it's been a game-changer for my process and productivity.

Blessed be any light-weight syntax that enables functions, expressions and composition thereof, and scripting and functional languages. Not so much those object oriented - creating a class in the REPL? Too much ceremony, too much time.

The (waning?) dominance of OO languages means many of us are missing out in this department.

# doctest

Using a REPL is great, and naturally we would want to save the code snippets as reference for our later selves, and what better place to put it than right alongside the code under test, as comments?

And wouldn't it be DREAM if we can then execute such (code as) comments directly? This is exactly the idea of Python's [doctest](https://docs.python.org/3/library/doctest.html). Example from this link.

```python
"""
Below is a test case for the function, factorial()!

>>> factorial(5)
120
"""

def factorial(n):

    import math
    # calculates factorial for n
    return result
```

[Wikipedia](https://en.wikipedia.org/wiki/Doctest#Literate_programming_and_doctests) also lists out doctest implementation in other languages. For example, Rustlang features [full support](https://doc.rust-lang.org/1.7.0/book/testing.html#documentation-tests) plus a great summary,

> Nothing is better than documentation with examples. Nothing is worse than examples that don't actually work...

But Rustlang supports yet another alternative that's equally exciting...

# Rust!

Consider this idiomatic code snippet from the [documentation of Rust](https://doc.rust-lang.org/1.7.0/book/testing.html#the-tests-module),

```rust
pub fn add_two(a: i32) -> i32 {
    a + 2
}

#[cfg(test)]
mod tests {
    use super::add_two;

    #[test]
    fn it_works() {
        assert_eq!(4, add_two(2));
    }
}
```

Code and test live harmoniously in the same file. In no small surprise, and to the irk of testing doctrinaires, [private functions](https://doc.rust-lang.org/rust-by-example/testing/unit_testing.html) are eligible too. Well played Rust.

# C# again

Those of us using C# need not feel left behind here. Doctest may not be a thing in .Net (yet), but existing testing libraries such as NUnit already make it possible to inline tests right next (or above) any code.

A great example from [this](https://stackoverflow.com/a/25076422/4687081) stackoverflow reply by [Andrej Adamenko](https://stackoverflow.com/users/3421814/andrej-adamenko).

```CSharp
public static class Ext
{
     [TestCase(1.1, Result = 1)]
     [TestCase(0.9, Result = 1)]
     public static int ToRoundedInt(this double d)
     {
         return (int) Math.Round(d);
     }
}
```

This really brings all the goodies together - code and test as one, and maintenance becomes a joy. Why look elsewhere?!

There are a few restrictions - for good reasons.

- the method under test must be static
- expose all input as parameters, and all output as return value. Implicit input (as from constructors via popular "dependency injection") or output (side effects) won't play too nicely with this style. 

In another word, it's only possible with pure functions - which are the best type of code.

But we need not restrict ourselves here. Putting tests (either inlined or as separate test suites) alongside classes under test affords us many extra benefits on top of those above,

- test shares the same config (consider IoC, App / web config) as code because they live in the same assembly
- no need to duplicate and manually keep dependencies in sync across multiple projects 
- needing to run diagnostics (such as to check compatibility of running environments)? Ship your tests! Either to the server or to the library users. 

One would argue the size of the package may increase with all tests being included. Sure thing if a few extra MBs really make a difference, as it may be for drivers, embedded or some systems software. But is it really the case for web, desktop or even mobile applications?

We get all these benefits by bringing existing code and test closer, without introducing new libraries or dependencies. Why not?