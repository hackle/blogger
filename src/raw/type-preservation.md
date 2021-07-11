Given a string value `"foo"`, how do we force it to be of literal type `"foo"`, not the more generic `string`?

For better or worse, in programming, it's not uncommon for the same value to appear as different types. For example, `1` can be either `int` and `decimal` in C# (sometimes with a bit of extra annotation), or as it's "nominally typed", two interfaces of the exact same shape are usually treated as different types.

TypeScript has structural typing, which I guess suits the purpose of retrofitting types to the chaotic world of JavaScript. Being "structural" does not necessarily mean a value always has only one type. To the contrary, there are nuances to type annotations, especially with regards to literal types (which I consider a game changer). Understanding the nuances can help us preserve the right level of information to suit different purposes.

## literals, maps and tuples

TypeScript does a fairly good job of inferring types for values (technically functions are also values - here we talk about primitives, maps and tuples), most of the time it's so smooth we don't need to pay it any attention. There are however different treatments to type inference depending on how the code is written, that are only visible when you look closely. When you do though, what you see might not be what you expect. Although there are usually very good reasons behind how it's done.

For example, `let day = 'Saturday'` will result in `day` having a `string` type. No surprise there. 

But for `const day = 'Saturday'`, `day` has type `Saturday`. This can throw newcomers off, but it is quite reasonable - by `const` we promise to never change the value of `day`, so it might as well take the strictest type, in this case, the literal string `Saturday`. Worth noting, this is the strongest possible constraint for `day`.

It's less obvious with maps. `let map = { day: 'Saturday' }` results in type `{ day: string }`, but `const map = { day: 'Saturday' }` makes no difference. Why?! It's the quirk of value / reference differentiation. `map` is a reference and `const` enforcement is only shallow, which allows mutation of field value e.g. `map.day = 'Sunday'`, so it will be incorrect to restrict the type of `day` to only `Saturday`. Hope is not lost. To get the strictest typing we use `const map = { day: 'Saturday' } as const` to lock it down for `day`.

`as const` works for tuples too, which are easily confused with arrays. For example `const tup = [ true, 'love' ]` results in `(string | boolean)[]`, and `const tup2 = [ true, 'love' ] as const` gives `readonly [true, "love"]`.

## as function parameters

Does the above treatments apply to function parameters? For example, how do we require a function parameter to be of string literal type, but not the generic `string` type? 

Unfortunately, the answer is no. We won't have at our disposal the annotations such as `as const` for function parameters.

To illustrate the problem let's use this function: 

```TypeScript
declare function needLiteralStr(foo: string): typeof foo;

const l1 = needLiteralStr('foo'); 
// const l1: string
```

The goal is to make it so `const l1: "foo"`. The above code fails as `l1` is of type `string`, which is too generic.

To be clear, it's also not valid syntax to write `function needLiteralStr(foo: string as const)`.

"Evil" programmers would say, just write `function needLiteralStr(foo: 'foo')`! But it's too specific, and won't work for `needLiteralStr('bar')`.

The trick is to make the parameter generic but with a constraint in the form of `T extends string`.

```TypeScript
declare function needLiteralStr1<T extends string>(foo: T): T;

const l2 = needLiteralStr1('foo');
// l2: "foo"
```
 
This is like saying, "`foo` is of a specific type of `string`", and TypeScript will take the hint and infer the more specific type, in this case the literal `"foo"`.

Maps are a similar case. Consider the below example,

```TypeScript
declare function needLiteralMap(foo: Record<string, string>): typeof foo;

const m1 = needLiteralMap({ foo: 'foo', bar: 'bar' });

// const m1: Record<string, string>
```

You would have guessed, this is how to preserve the literal values of both keys and values.

```TypeScript
declare function needLiteralMap2<
    T extends string, 
    V extends string, 
    M extends Record<T, V>
>(foo: M): M;

const m2 = needLiteralMap2({ foo: 'foo', bar: 'bar' });

// const m2: { foo: "foo"; bar: "bar"; }
```

What if `bar` is a `number`? We then need `V extends string | number`. One will be tempted to go `V extends any`, but that won't work as `extends any` seems too generic to be helpful, and we'll end up with the default inference.

Tuples seem really tricky - how we do express "Give us a tuple with unknown number of elements, whose types we also don't know yet?"

```TypeScript
declare function needTup(foo: [??])
```

It would look like we are stuck here, but `extends` come to our rescue yet again. `T extends any[]` allows inferring types of each elements of a tuple.

```TypeScript

declare function needTup<T extends any[]>(...foo: T): T;

const t1 = needTup(true, 'love');

// const t1: [boolean, string]
```

However it doesn't look possible to infer each element as their respective literal value - something to be figured out yet?

These innocuous techniques will set the stage for very powerful handling of types such as pattern matching and calculation, which are typical of dependent types. Something we'll cover (again) later.