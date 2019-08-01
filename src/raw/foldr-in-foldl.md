In one of his talks, Erik Meijer revealed one of his interview questions was to ask the (poor) applicant to define `foldl` in terms of `foldr`.

I am glad that I was never in such an interview as it took me quite a while to figure this out (with a fair bit of googling).

Called `reduce` in Javascript, or `aggregate` in Linq, in my opinion `fold` is one of the most useful concepts in day-to-day programming (and definitely not just for functional style programming). Getting a better understanding of `fold` certainly won't hurt.

## `foldl` vs `foldr`

Given the below type, one can come up with two different implementations.

```Idris
myFold : (f: elem -> acc -> acc) -> (init: acc) -> (xs: List elem) -> acc
```

First implementation - note `init` is used for the very first element `x`.

```Idris
myFold1 : (f: elem -> acc -> acc) -> (init: acc) -> (xs: List elem) -> acc
myFold1 f init [] = init
myFold1 f init (x :: xs) = myFold1 f (f x init) xs
```

Second implementation - `init` is passed along and used only at the very end of the recursion.

```idris
myFold2 : (f: elem -> acc -> acc) -> (init: acc) -> (xs: List elem) -> acc
myFold2 f init [] = init
myFold2 f init (x :: xs) = f x (myFold2 f init xs)
```

Let's see them in action with a trival example (one thing that I have learned from `Haskell` and `Idris`, is that trivial examples can be the most important (or confounding) ones).

```Idris
*myFoldl> myFold1 (::) [] [1..3]
[3, 2, 1] : List Integer
*myFoldl> myFold2 (::) [] [1..3]
[1, 2, 3] : List Integer
```

So `myFold1` will reverse a list as the first element `1` will be prepended to `init` - `[]`, then `2` and `3`. On the contrary, `myFold2` loyally reconstructs the list.

```idris
*myFoldl> myFold1 (::) [] [1..3] == 3::(2::(1::[]))
True : Bool
*myFoldl> myFold2 (::) [] [1..3] == 1::(2::(3::[]))
True : Bool
```

Turns out `myFold1` is basically `foldl` and `myFold2` is `foldr`.

```idris
*myFoldl> foldl (flip (::)) [] [1..3]
[3, 2, 1] : List Integer
*myFoldl> foldr (::) [] [1..3]
[1, 2, 3] : List Integer
```

Or simply as below thanks to structural equality.

```idris
*myFoldl> myFold1 (::) [] [1..3] == foldl (flip (::)) [] [1..3]
True : Bool
*myFoldl> myFold2 (::) [] [1..3] == foldr (::) [] [1..3]
True : Bool
```

The sharp-eyed would have noticed that the official type of `foldl` is not exactly the same as `myFoldl`.

```Idris
foldl : Foldable t => (acc -> elem -> acc) -> acc -> t elem -> acc
```

`acc` appears in front of `elem` - this is to suggest that the accumulation goes from left to right, hence fold **left**.

## `foldl` in terms of `foldr`, first go

Looking at the types (ignore the `Foldable` bit for now):

```idris
foldl : Foldable t => (acc -> elem -> acc) -> acc -> t elem -> acc
foldr : Foldable t => (elem -> acc -> acc) -> acc -> t elem -> acc
```

Hold on - there is but one difference, the order of the parameters to the function `(acc -> elem -> acc)`! It cannot get easier than this, you say, we need only swap the parameters when they are bound! `flip` does exactly that.

```idris
myFoldl f acc xs = foldr (flip f) acc xs
```

Let see if it works:

```idris
*myFoldl> myFoldl (flip (::))  [] [1..3]
[1, 2, 3] : List Integer
```

If you happen to be type-superstitious, you are in for a let-down here. No cigar! Although `foldr` is happy with the types of the arguments, but just by tweaking the order of some parameters, we are not going to magically turn it into `foldl`.

Of course we can also just do:

```idris
foldLeftCheat : (acc -> elem -> acc) -> acc -> List elem -> acc
foldLeftCheat f init xs = foldr (flip f) init (reverse xs)
```

And

```idris
*myFoldl> foldLeftCheat (flip (::)) [] [1..3]
[3, 2, 1] : List Integer
```

Which works for our example, but there is a reason I named this `cheat` - the interface `Foldable` in the type of `foldl`. If we look at the type of `foldl` again,

```Idris
foldl : Foldable t => (acc -> elem -> acc) -> acc -> t elem -> acc
```

It actually does not mention `List` at all, instead it used an interface called `Foldable` that's defined [here](https://github.com/idris-lang/Idris-dev/blob/master/libs/prelude/Prelude/Foldable.idr).

So while `reverse` works for `List` (which is an instance of `Foldable`), it may not work for other instances, for example `Maybe` which is also an instance of `Foldable`.

```idris
foldl (\acc, elem =>  "It's " ++ acc ++ " and " ++ elem) "Tom" (Just "Jerry")
"It's Tom and Jerry" : String
```

If we change the type of `foldLeftCheat` to use `Foldable`, we'll get an error from `Idris`.

```idris
foldLeftCheat : Foldable t => (acc -> elem -> acc) -> acc -> t elem -> acc
foldLeftCheat f init xs = foldr (flip f) init (reverse xs)

-- error:
When checking right hand side of foldLeftCheat with expected type
        acc

Can't disambiguate name: Prelude.List.reverse, Prelude.Strings.reverse
```

So our proper goal is to **define a `foldl` in terms of `foldr` that's applicable to all instances of `Foldable`**.

With that said, we'll still use our trivial example for `List` as it's easier to understand. However we must abstain from using anything that's specific to `List`, so we'll be able to switch to a proper implementation for 'Foldable'.

## with continuation

For `foldl` to come out of a `foldr`, we need to somehow transform `1::(2::(3::[]))` to `3::(2::(1::[]))`.

look at the implementation of `myFold2` / `foldr` again:

```idris
myFold2 : (f: elem -> acc -> acc) -> (init: acc) -> (xs: List elem) -> acc
myFold2 f init [] = init
myFold2 f init (x :: xs) = f x (myFold2 f init xs)
```

These we cannot change:

* `f` which accumulates `elem` and `acc` (starting in the form of `init`), it remains unchanged throughout
* the order of how each element in the `List` is processed - from left to right.

The only thing we can change is `acc`, which is transformed at each iteration as result of `f x acc`.

If we expand `myFold2 f init [1..3]` we get

```Idris
f 1 (f 2 (f 3 init))
```

Here is the genius (though not mine) bit: imagine we have a way to process `1` and `2` first, and pass the result `acc` as an argument to the iteration for `3`.

```idris
f 1 (f 2 (\result_acc2 => f 3 result_acc2))
```

That would require us to pass the result of the current iteration to the next iteration, so we'd end up with

```idris
f 1 (\result_acc1 => f 2 ??what to do with result_acc1?? (\result_acc2 => f 3 result_acc2))
```

What do we do with `result_acc1`? apparently we can just follow the pattern and do `f 2 result_acc1` which gives us a new `acc`

```idris
f 1 (\result_acc1 => let acc2 = f 2 result_acc1 in (\result_acc2 => f 3 result_acc2))
```

Now we see `(\result_acc2 => f 3 result_acc2)` takes an `acc` which we just have handy, let's pass it in

```idris
f 1 (\result_acc1 => let acc2 = f 2 result_acc1 in (\result_acc2 => f 3 result_acc2) acc2)
```

Now we need this done for `1` and here is what we get afterwards.

```idris
\result_acc0 => let acc1 = f 1 result_acc0 in
  (\result_acc1 => let acc2 = f 2 result_acc1 in
    (\result_acc2 => f 3 result_acc2) acc2
  ) acc1
```

let's see if this would work, in the REPL

```idris
*myFoldl> let f = (::) in \result_acc0 => let acc1 = f 1 result_acc0 in (\result_acc1 => let acc2 = f 2 result_acc1 in  (\result_acc2 => f 3 result_acc2) acc2 ) acc1
\result_acc0 => 3 :: 2 :: 1 :: result_acc0 : List Integer -> List Integer
```

Strange - now we have turned the whole expression into a function with type `List Integer -> List Integer` - as a result of requiring `result_acc*` for each iteration.

That aside, if we get a function back, what better to do other than applying it? And we only need to pass in `result_acc0` which can be, you guessed it, the long-missed `init` - `[]`!

```idris
*myFoldl> let f = (::) in (\result_acc0 => let acc1 = f 1 result_acc0 in (\result_acc1 => let acc2 = f 2 result_acc1 in  (\result_acc2 => f 3 result_acc2) acc2 ) acc1) []
[3, 2, 1] : List Integer
```

Aha! That's exactly how we wanted it! Now, let's see what just happened,

* we broke the implementation up to 2 parts,
* first to build a function with `foldr`,
* then secondly, to apply that function to `init`
* using lambda, the current iteration can act on the result of the previous iteration, therefore we reversed the order of processing! Let that sink in.

We need to move on as we cannot keep this tedious implementation as it's hard-coded for list `[1,2,3]`. We have figured out the algorithm, let's see how it looks with a couple of type holes.

```idris
myFoldl : (acc -> elem -> acc) -> acc -> List elem -> acc
myFoldl g initial xs = let foldFunc = foldr ?foldOneElem ?initFunc xs in foldFunc initial

-- foldOneElem
acc : Type
elem : Type
g : acc -> elem -> acc
initial : acc
xs : List elem
letty : Type
t : Type -> Type
elem1 : Type
--------------------------------------
foldOneElem : elem -> (acc -> acc) -> acc -> acc
initFunc : acc -> acc
```

We find that the type of `initFunc` and result of the `foldr` expression is now `acc -> acc`, in the case of `[1,2,3]`, it will be `List Int -> List Int`, as is seen above.
`acc -> acc` is also the type of the second parameter and result type of `foldOneElem` if we see it as `elem -> (acc -> acc) -> (acc -> acc)`.

If we are to identify the parts of `foldr` in our algorithm

```Idris
\result_acc1 => let acc_ = f 2 result_acc1 in
  (\result_acc2 => f 3 result_acc2) acc_
```

* the whole expression is `foldOneElem`, of whose parameters,
* `elem` is `2`
* `acc -> acc` is `(\result_acc2 => f 3 result_acc2)`
* the next `acc` is `acc_` (I've renamed it to differentiate)
* `f` is `g`

With these observations, we can lift `foldOneElem` and fill in the holes.

```idris
foldOneElem : (g : acc -> elem -> acc) -> (elem: elem) -> (prev: acc -> acc) -> ((cur: acc) -> acc)
foldOneElem g elem prev cur = prev (g cur elem)
```

Now we need to fill in `initFunc` whose type is `acc -> acc`. But where is that in our algorithm?

Well it turns out`(\result_acc2 => f 3 result_acc2)` should also be written as below so it's **really** consistent with the pattern for `result_acc0` and `result_acc1`.

```Idris
(\result_acc2 => let acc3 = f 3 result_acc2 in initFunc acc3)
```

Remember `initFunc` is used for the base case - for `List`, is `[]`. Apparently in our case, it's trivial and we need only `id`.

So here is our `myFoldl` in its full glory:

```Idris
foldOneElem : (g : acc -> elem -> acc) -> (elem: elem) -> (prev: acc -> acc) -> (cur: acc -> acc)
foldOneElem g elem prev cur = prev (g cur elem)

myFoldl : (acc -> elem -> acc) -> acc -> List elem -> acc
myFoldl g initial xs = let foldFunc = foldr (foldOneElem g) id xs in foldFunc initial
```

If we are to try it out:

```idris
*myFoldl> myFoldl (flip (::)) [] [1..3]
[3, 2, 1] : List Integer
```

## for `Foldable`

Remember the promise to make our `myFoldl` compatible to the `Foldable` interface? Since we didn't use anything `List` specific in our implememtaion, we need only change the type of the function without changing its implementation.

```Idris
myFoldl : Foldable t => (acc -> elem -> acc) -> acc -> t elem -> acc
```

And now it works for `List` and `Maybe` alike (and should for other instances of `Foldable`)

```Idris
*myFoldl> myFoldl (flip (::)) [] [1..3]
[3, 2, 1] : List Integer
*myFoldl> :t foldrfoldl (\acc, elem =>  "It's " ++ acc ++ " and " ++ elem) "Tom" (Just "Jerry")
No such variable foldrfoldl
*myFoldl> myFoldl (\acc, elem =>  "It's " ++ acc ++ " and " ++ elem) "Tom" (Just "Jerry")
"It's Tom and Jerry" : String
```

## summary
This turns out to be a pretty lengthy post, but honestly, defining `foldl` in terms of `foldr` is no easy matter to me! This post more or less captures the journey of me understanding (not finding as it's not my idea) the solution.

To reverse the order of processing, we resorted to nothing else - functions. I later understood that this is yet another case for `continuation`. But of course! How else would we get late binding? In all honesty though, it didn't come to be until I found the post on [ScTurtle's Pool](https://scturtle.me/posts/2016-01-27-foldl.html).

The default implementation for `foldl` in `Idris` can be found [here](https://github.com/idris-lang/Idris-dev/blob/master/libs/prelude/Prelude/Foldable.idr). It's a bit cryptic in ~~dreaded~~ point-free style.

## further reading
* [Organizing Functional Code for Parallel Execution; or, foldl and foldr Considered Slightly Harmful](https://vimeo.com/6624203)
