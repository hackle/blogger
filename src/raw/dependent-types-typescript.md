(Update: more proper dependent types have become possible with new language features in TypeScript, please see [this later post](dependent-types-in-typescript-seriously)).

"A dependent type is a type whose definition depends on a value", according to Wikipedia. Unlike most statically-typed programming languages where types and values live in different places, with dependent types, they may appear together. For example, in `Idris`, there is a function that appends a Vector to another, with a type signature as below,

```
append : (xs : Vect m a) -> (ys : Vect n a) -> Vect (m + n) a
```

Whereas `m` and `n` are values that specify the lengths of the Vectors. As they are values, it's only natural to treat them as such, so the result `Vect` would have length `m+n`.

You will be surprised, but it's possible to get a taste of dependent types in TypeScript, just from a *slightly different* direction. 

[**Literal types**](https://www.typescriptlang.org/docs/handbook/advanced-types.html#string-literal-types), come to think of it, are basically **values**. And with TypeScript's support for conditional types, we can (almost) manage a little type calculation. 

Let's get to it!

First we have a union type comprises two literals, and a function `toggle`.

```TypeScript
type OnOff = 'on' | 'off';

function toggle(st: OnOff): OnOff {
    return st === 'on' ? 'off' : 'on';
}
```

This is all very well but nothing is stopping us from returning 'on' for 'on' and 'off' for 'off', therefore voiding the promise of `toggle`. Is there a way to express "toggle" on type level so 'on' is guaranteed to be 'off' after toggling, and vice versa?

This calls for some conditional typing. Introduce `Toggle` (as a type).

```TypeScript
type Toggle<T extends OnOff> = T extends 'on' ? 'off' : 'on';
```

Why, it's only a good-old ternary expression, albeit on type level. See how both 'on' and 'off', innocent string values, live comfortably in type declarations?

Pretty, you say, but does it work?

```TypeScript
const state1: OnOff = 'on';
const state2: Toggle<typeof state1> = 'off'; 
const state3: Toggle<typeof state1> = 'on'; // error: Type '"on"' is not assignable to type '"off"'.ts(2322)
```

By assigning type `Toggle<typeof state1>`, the compiler forces `state2` to have value "off", and rejects `state3` for violating its type!

However, I am out of luck when implementing `toggle`:

```TypeScript
function toggle<T extends OnOff>(st: T): Toggle<typeof st> {
    return st == 'on' ? 'off' : 'on';
}

// error: Type '"on" | "off"' is not assignable to type 'Toggle<T>'.
//  Type '"on"' is not assignable to type 'Toggle<T>'.ts(2322)
```

The problem, I guess, is the compiler cannot massage `'on' | 'off'` back to `Toggle<typeof st>`, although clearly they are equal when we compare their eventual constituents.

Despair not, as is usually the case, we can cheat with `as any`,

```TypeScript
function toggle<T extends 'on' | 'off'>(st: T): Toggle<T> {
    return st == 'on' ? 'off' : 'on' as any;
}

const state4 = toggle('on');
```

And if we inspect the type of `state4`, it will be `"off"`.

If you find this interesting, maybe you'd also like to read about [this issue on GitHub](https://github.com/microsoft/TypeScript/issues/33014).

P/S I won't recommend trying something like the `append` example, just yet. (update: But we will, [right here](dependent-types-typescript-seriously))