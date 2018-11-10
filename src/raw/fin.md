Sorry to disappoint - `Fin` here is not French and does not mean I am done bloggine (yet) :)

`Fin` is a type in Idris that represents
``Numbers strictly less than some bound.  The name comes from "finite sets".`` according to the source code [here](https://github.com/idris-lang/Idris-dev/blob/master/libs/base/Data/Fin.idr) (you might not want to go there yet if you are new to `Fin` or Idris, just like me).

Let's look at its definition.

```idris
data Fin : (n : Nat) -> Type where
    FZ : Fin (S k)
    FS : Fin k -> Fin (S k)
```

You'll find ``Fin`` has two constructors, and ``FS`` is defined recursively in terms of ``FZ``. ``FZ`` is kind of special, its argument must be in the form of ``S k``, which means it can never be 0 as 0 cannot be represented as such. This is Idris' way of saying the argument to ``FZ`` must be greater than 0.

Clever, but nothing fancy, except...

## The first shock
The acute observers will soon find, although this type has a parameter in ``(n : Nat)``, neither of its constructors takes any parameter. In another word, we cannot construct a value for this type! (Edit: actually we can, by specifying the value of the implicit parameter, such as ``FZ {k=0}``)

Try this in the Idris REPL:

```Idris
Idris> :module Data.Fin
*Data/Fin> FZ
(input):Can't infer argument k to FZ
*Data/Fin> FS
(input):Can't infer argument k to FS
*fin> FS FZ
(input):Can't infer argument k to S, Can't infer argument k to FZ
```

Because there no way we can construct it, we cannot give the type parameter (yet). What is going on here? How this is useful at all?

## An example

Let's try to understand it with a typical example.

```Idris
import Data.Fin
import Data.Vect

indexFin : {n: Nat} -> Fin n -> Vect n a -> a
indexFin FZ (x :: xs) = x
indexFin (FS s) (x :: xs) = indexFin s xs
```

Given a ``Vect`` of length n, the ``indexFin`` will not take any invalid index that's equal or greater than n, which saves us the blush of index-out-of-range errors. It is called as such:

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

Note how Idris converts numerics to ``Fin`` values, which begs the question, what are 3, 0, 3 in ``Fin``?

## A sliver of light

Still with the REPL, let's try this:
```Idris
*fin> the (Fin 4) 3
FS (FS FZ) : Fin 4
```

Thanks to the ``the`` keyword, we can specify that ``3`` is a value of type ``Fin 4``!

## Let's try break it

Notice above there is an error message ``When using 3 as a literal for a Fin 3, 3 is not strictly less than 3``, which of course tells us that 3 cannot be Fin 3, as confirmed with this:

```Idris
When using 3 as a literal for a Fin 3
        3 is not strictly less than 3
```

Well, if 3 has to be less than a number, sure we are not short of options?

```idris
*fin> the (Fin 4) 3
FS (FS (FS FZ)) : Fin 4
*fin> the (Fin 5) 3
FS (FS (FS FZ)) : Fin 5
*fin> the (Fin 6) 3
FS (FS (FS FZ)) : Fin 6
```

OMG... what is going on here? so 3 can be Fin 4, Fin 5 or Fin 6???

Well that's right! The trick is with the type parameter to ``Fin`` which we never used to construct any ``Fin`` value - it's left for interpretation.

Take the above example, try to figure out the type of ``FZ`` in each example?

take ``the (Fin 4) 3`` as an example. The REPL interprets it as ``FS (FS (FS FZ)) : Fin 4``, if we reverse-engineer a bit, ``FZ`` would have to be ``Fin 1``, so ``FS FZ`` is of type ``Fin 2`` and so on.

Now your turn to try ``Fin 5`` and ``Fin 6`` - and you'll find ``FZ`` is interpreted as ``Fin 2`` and ``Fin 3`` respectively.

Getting closer...

## Finite set
Next let's count how many values can we construct for a certain ``Fin`` type.

From what limited I have learnt from Idris, is to always try the base case first. so

```idris
*Data\Fin> the (Fin 0) FZ
(input):1:1-14:When checking argument value to function Prelude.Basics.the:
        Type mismatch between
                Fin (S k) (Type of FZ)
        and
                Fin 0 (Expected type)

        Specifically:
                Type mismatch between
                        S k
                and
                        0
```

Well, if ``FZ`` cannot work, then ``FS FZ`` won't either. It seems ``Fin 0`` can have no values. Which makes good sense in the context of our function ``indexFin`` - an empty ``Vect`` has no value, thus cannot return anything for any index.

(Hold on - don't you want to try ``indexFin`` with an empty Vect?)

Let's try ``Fin 1``:
```Idris
*Data\Fin> the (Fin 1) FZ
FZ : Fin 1
*Data\Fin> the (Fin 1) (FS FZ)
(input):1:14-18:When checking an application of constructor Data.Fin.FS:
        Type mismatch between
                Fin (S k) (Type of FZ)
        and
                Fin 0 (Expected type)

        Specifically:
                Type mismatch between
                        S k
                and
                        0
```
So ``Fin 1`` can have 1 value only, which is ``FZ``, and ``Fin 2``, 2 values, ``FZ`` and ``FS FZ``. ``Fin 3``, 3. It's all making sense now isnt' it?

## Summary
``Fin`` is a clever type that by leaving its type parameter to interpretation, limit the number of values that a certain type can have. There is a good name to call these allowed values: **inhabitants**.

Well I didn't think it would take me so long to explain ``Fin``! But rest assured, it took me much longer to grok the whole thing and reach my WOW moment.

## Further reading
See [Fin](/index-fin-alternative) for an alternative version of  the ``indexFin`` function.
