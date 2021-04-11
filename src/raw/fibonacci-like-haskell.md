You would have heard in `Haskell` people write [fibonacci functions](...link) instead of "hello world". It's always a good challenge to try to understand how some of them work. One of my favourite is this

```Haskell
fibs = [ 0, 1 ] ++ zipWith (+) fibs (tail fibs)
```

Is it possible to make this magic in other languages? Well I can tell you it's not easy in JavaScript, but it would seem C# does lend itself well to this, with a few tricks.

```C#

```
