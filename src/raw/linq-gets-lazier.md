Hardly the best way to, but it's during an unplanned live demo that I found out Linq got even lazier.

I was running my go-to example of Linq's laziness.

```CSharp
Console.WriteLine(
    Enumerable.Range(0, 100)
        .Select(x => {
            Console.WriteLine($"Fetching {x}");

            return x;
        })
        .Skip(10)
        .First()
);
```

While someone with the `for-loop` state of mind may think "Fetching X" will be printed 100 times, thanks to the lazy nature of Linq, the actual number would be 11 - as the other 89 times of `Console.WriteLine` was made unnecessary by `Skip(10)` and `First()`. [Try it out on dotnetfiddle](https://dotnetfiddle.net/I37R4P).

11 was what I expected, but I was wrong! Because Linq got even better, or lazier. 

To my bewilderment, using .Net 5.0, "Fetching X" was printed only ONCE. You heard me, not 100 times, not 11 times, but ONCE. [Try it out](https://dotnetfiddle.net/OuPQxF).

While this did demonstrate good laziness (it's a good thing!), it was "too good to be true", and I was quite taken aback and seriously thought something was wrong. 

After recovering from the shock, I thought I'd test it with another example that's equally lazy, maybe more extreme, and most definitely dangerous, typically presented in Haskell.

```Haskell
main = 
    let noSmallerThan5 = \x -> if x < 5 then (error "too small") else x
        from11 = drop 10 $ fmap noSmallerThan5 [0..100]
    in putStrLn $ show (from11 !! 0)
```

This outputs `10`, although the program has to iterate over 10 elements, of which the first 5, if evaluated, will result in an `error` (equivalent of `Exception` if I may) "too small". But that doesn't happen, as these elements are made unnecessary by `drop 10`. Lazy. Can't get lazier than that.

Such dangerous stunts have not been possible in Linq before, at least not [with .NET 4.7.2](https://dotnetfiddle.net/LUIcvj)

```CSharp
public static void Main()
{
    var numbers = 
        Enumerable.Range(0, 1000)
            .Select(x => {
                if (x < 5) throw new Exception("Bom");

                return x;
            });

    Console.WriteLine(numbers.Skip(10).First());
}
```

This gives us an error,

> Run-time exception (line 11): Bom
> Stack Trace:
> [System.Exception: Bom]
   at Program.<Main>b__0(Int32 x) :line 11
   at System.Linq.Enumerable.WhereSelectEnumerableIterator`2.MoveNext()
   at System.Linq.Enumerable.<SkipIterator>d__31`1.MoveNext()
   at System.Linq.Enumerable.First[TSource](IEnumerable`1 source)
   at Program.Main() :line 16

However, [with .NET 5](https://dotnetfiddle.net/d9kFor), the exact same code outputs `10`, with no `Exception` thrown, just like the Haskell example. It's truly lazy, amazing!

I must admit at one point I got quite carried away, and went off trying it on different instances of `IEnumerable`, only to be frustrated, and found out this does not work for just any `IEnumerable`, not even the classic `yield return`, as is [the previous implementation of `Enumerable.Range`](https://github.com/microsoft/referencesource/blob/5697c29004a34d80acdaf5742d7e699022c64ecd/System.Core/System/Linq/Enumerable.cs#L1271).

```CSharp
static IEnumerable<int> RangeIterator(int start, int count) {
    for (int i = 0; i < count; i++) yield return start + i;
}
```

Turns out the magic only comes with the new implementation of `Enumerable.Range`, which may not be as elegant, but is definitely clever and sensible.

Here is how it works. 

For a regular `IEnumerable`, calling `Skip(10)` requires [iterating over](https://github.com/dotnet/corefx/blob/7711b939317ae5cb3ffa4f19a39119876aafd30e/src/System.Linq/src/System/Linq/Skip.SizeOpt.cs#L18) 10 elements and the evaluating whatever expressions that have been built so far.

However, for `Range(start, end)`, that's not really necessary. `Skip(10)` can be simply evaluated as `Range(start + 10, end)`, which makes it a true "skip", as all 10 elements and their expressions are excluded completely.

Note this is possible because `Range` keeps tab of the index and count of elements, which is not always the case for any `IEnumerable`. But it does sound sensible for `Array` or `List` doesn't it? Turns out that is [certainly the case](https://dotnetfiddle.net/Mik1Mi).

As for implementation, this would require specialised `Skip` and `Select`, who are themselves both `IEnumerable`. Thus we find [`RangeIterator`](https://github.com/dotnet/corefx/blob/7711b939317ae5cb3ffa4f19a39119876aafd30e/src/System.Linq/src/System/Linq/Range.cs#L31) and [`SelectRangeIterator`](https://github.com/dotnet/runtime/blob/57bfe474518ab5b7cfe6bf7424a79ce3af9d6657/src/libraries/System.Linq/src/System/Linq/Select.SpeedOpt.cs#L165). You'd be able to find the [specialisations for `Array` and `List`](https://github.com/dotnet/corefx/blob/7711b939317ae5cb3ffa4f19a39119876aafd30e/src/System.Linq/src/System/Linq/Partition.SpeedOpt.cs#L102) in the same repository.

More specifically, the `Skip` trick is done [here](https://github.com/dotnet/corefx/blob/7711b939317ae5cb3ffa4f19a39119876aafd30e/src/System.Linq/src/System/Linq/Range.SpeedOpt.cs#L51) in the name of "speed". Fair play!

```CSharp
public IPartition<int> Skip(int count)
{
    if (count >= _end - _start)
    {
        return EmptyPartition<int>.Instance;
    }

    return new RangeIterator(_start + count, _end - _start - count);
}
```

Worth mentioning the `Range` [pull request](https://github.com/dotnet/corefx/pull/37410) dates back to May 2019, and preceding optimisation for `List` has been around for 6 years, so this has all been old news. This reporter feels out of the loop.