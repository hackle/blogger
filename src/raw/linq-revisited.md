## Infinity

We start with infinity - usually this means the evaluation is lazy. 

```C#
    var infinity = Infinity.Of(index => index); // the index can be useful 
    // 0, 1, 2, 3 ...

    infinity.Take(1000);
    // 1000 elements
```

Or a more practical use case: get the very first number greater than 5. Note it's perfect safe to use ``First()`` because there is guaranteed to be a value - because the sequence is infinite!

```C#
var firstValid = Infinity.Of(_ => Console.ReadLine()).First(val => Convert.ToInt32(val) > 5);
```

While it may seems a bit vague to some, it's actually quite easy to read. I like how expressive this is.

Here is the implementation - if you can get rid of the mutation in a simpler way, please do let me know!

```C#
public static class Infinity
{
    public static IEnumerable<T> Of<T>(Func<int, T> f)
    {
        var i = 0;
        while (true) 
        {
            yield return f(i++);
        }
    }
}
```