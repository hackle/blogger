## indexFin and Fin

In [Fin](/fin) we've seen a version of type-safe indexFin (which is basically a copy of the ``index`` function defined in ``Data.Vect``) which once compiles, guarantees to return a valid element from a vector.

```Idris
import Data.Fin
import Data.Vect

indexFin : {n: Nat} -> Fin n -> Vect n a -> a
indexFin FZ (x :: xs) = x
indexFin (FS s) (x :: xs) = indexFin s xs
```

Which is simply called as such:

```Idris
*fin> indexFin 3 [1,2,3,4]
4 : Integer
*fin> indexFin 0 [1,2,3,4]
1 : Integer
*fin> indexFin 3 [1,2,3]
(input):1:10:When checking argument prf to function Data.Fin.fromInteger:
        When using 3 as a literal for a Fin 3
                3 is not strictly less than 3
```

Out of sheer curiosity - can we have an alternative to this?

Suppose I can prove an index argument is less than the length of the vector, I should be able to get the same type-safety?

## The type for ``elementAt``

```Idris
elementAt : (idx: Nat) -> Vect (S n) a -> { auto prf: n `GTE` idx } -> a
```
A few interesting things:

* the length of the vector is ``S n`` rather than simply ``n``, which is an Idris way of saying **this vect must not be empty**
* we pass in an auto proof **n `GTE` idx** which means ``idx`` must be within bound.

(Read more about [auto proof here](http://docs.idris-lang.org/en/latest/tutorial/miscellany.html))

I don't know about you but I feel pretty impressed (by Idris and myself) how expressive the types alone can be!

## the code

Let's now get to my favorite part - coding with Idris!

Naturally, Ctrl+Alt+A to add a definition (with Atom), so we have:

```Idris
elementAt : (idx: Nat) -> Vect (S n) a -> { auto prf: n `GTE` idx } -> a
elementAt idx xs = ?elementAt_rhs
```

Well, don't think twice, just case split, and case split again.

```idris
elementAt : (idx: Nat) -> Vect (S n) a -> { auto prf: n `GTE` idx } -> a
elementAt Z (x :: xs) = ?elementAt_rhs_2
elementAt (S k) (x :: xs) = ?elementAt_rhs_3
```

Next we'll do a proof search on the hole ``elementAt_rhs_2``, as expected, ``x`` is returned, which makes sense - if the ``idx`` is ``0``, then we should return the very first element in the list. So far so good!

```Idris
elementAt : (idx: Nat) -> Vect (S n) a -> { auto prf: n `GTE` idx } -> a
elementAt Z (x :: xs) = x
elementAt (S k) (x :: xs) = ?elementAt_rhs_3
```

Now if we also do a proof search on ``elementAt_rhs_3``, we get ``x`` as well - which is in the correct type, but won't make much sense for our function.

When in doubt, type search. Idris tells us:

```Idris
a : Type
k : Nat
x : a
n : Nat
xs : Vect n a
prf : LTE (S k) n
--------------------------------------
elementAt_rhs_3 : a
```

There is a proof ``LTE (S k) n`` that tells us ``S k`` is less than or equal to ``n``. Apparently this is just another form of ```n `GTE` idx``` (``S k`` is one form of ``idx``). Informative, but not very helpful.

To jump the gun here, the only way I see how - is just to return ``elementAt k xs``. Seems reasonable right? Both the index and the size of the vector decrease by one, in another word, we are just using good-old recursion. So we get:

```Idris
elementAt : (idx: Nat) -> Vect (S n) a -> { auto prf: n `GTE` idx } -> a
elementAt Z (x :: xs) = x
elementAt (S k) (x :: xs) = let x1 = elementAt k xs in ?elementAt_rhs_3
```

Try type check, no - the compiler does not like it...

```Idris
When checking right hand side of elementAt with expected type
        a

When checking argument prf to Main.elementAt:
        Can't find a value of type
                LTE k n
```
This is telling us we need to prove that ``k`` is less than or equal to ``n``. But we already have ``GTE n (S k)``?

While I think surely this makes perfect sense, Idris does not understand that if ``idx <= n + 1`` then ``idx - 1 <= n ``. We have to prove it to Idris.

A bit lost, and honestly I don't know what to do except bringing the ``auto prf`` proof to scope. But the compiler still gives the same error.

```Idris

elementAt : (idx: Nat) -> Vect (S n) a -> { auto prf: n `GTE` idx } -> a
elementAt Z (x :: xs) = x
elementAt (S k) (x :: xs) {prf} = let x1 = elementAt k xs in ?elementAt_rhs_3
```

Still lost, the only thing left to do, is to case split on ``prf``, before that though, I have to revert the previous step, now I get:

```Idris
elementAt : (idx: Nat) -> Vect (S n) a -> { auto prf: n `GTE` idx } -> a
elementAt Z (x :: xs) = x
elementAt (S k) (x :: xs) {prf = (LTESucc y)} = ?elementAt_rhs_1
```

Now a type search on the hole returns:

```idris
a : Type
k : Nat
x : a
right : Nat
xs : Vect (S right) a
y : LTE k right
--------------------------------------
elementAt_rhs_1 : a
```

A few things to note here

* the only way the ``prf`` can be constructed, is ``LTESucc y``, which by the name of it, means it's a successive definition over another proof ``y``
* and the type of ``y`` is ``LTE k right``, and ``S right`` is the length of ``xs``. Previously we needed to prove ``LTE k n``, and ``n`` is the length of ``xs`` too. That means we've found our proof now!?

```Idris
elementAt : (idx: Nat) -> Vect (S n) a -> { auto prf: n `GTE` idx } -> a
elementAt Z (x :: xs) = x
elementAt (S k) (x :: xs) {prf = (LTESucc y)} = elementAt k xs
```

Ah, now it compiles! Let's see if it really works?

```Idris
*elementAt> elementAt 0 [1,2,3]
1 : Integer
*elementAt> elementAt 1 [1,2,3]
2 : Integer
*elementAt> elementAt 2 [1,2,3]
3 : Integer
*elementAt> elementAt 3 [1,2,3]
(input):1:1-19:When checking argument prf to function Main.elementAt:
        Can't find a value of type
                LTE 3 2
```

Yes, it does!

## Summary
The Idris compiler is so powerful that if we express our problems well enough in types, it can guide us to a solution by giving us very useful information every step of the way.

## Further reading
For ``List``, there is also an ``index`` function defined as such:
```Idris
index : (n : Nat) ->
                    (xs : List a) ->
                    {auto ok : InBounds n xs} -> a
```
Note there is an auto proof ``ok``? What's different is that a ``Vect`` has a length in its type, but a ``List`` does not, so how does ``List`` do it? And can the same proof be used for ``Vect``? Why or why not?
