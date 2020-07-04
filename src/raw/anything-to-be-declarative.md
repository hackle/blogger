I'd do anything to make code more declarative, or go to great lengths before giving up.

It's not always possible with most languages, libraries or programming models. Mutating values can be built-in and very hard to avoid. However, there are patterns I resort to that help make up a declarative "appearance". In the examples below, I'll use `TypeScript` as it's my current daily poison.

Reading buffer is a recent task of this kind. How `Buffer` works kind of indicates we need some kind of mutation (barring possible workarounds),

```Typescript
const buffer = Buffer.from([1, 2, 3, 4]);
let index = 0;
const number1 = buffer.readInt8(index);
++index;
const number2 = buffer.readInt16BE(index);
index += 2;
const number3 = buffer.readInt8(index++);
```

Yuck! It irks me! I want to get the mutations out of the way, or at least, "tucked away"...

## The tuck-away

```Typescript
const [ number1 ] = [ buffer.readInt8(index), ++index ];
const [ number2 ] = [ buffer.readInt16BE(index), index += 2 ];
const [ number3 ] = [ buffer.readInt8(index), ++index ];
```

Whew! This is much more intention-revealing! Mutating / moving the index is important but noisy, the focus should be on the result numbers, and let the noise fade into the background.

You'll see it's made possible by array destructuring, and the mutation is left out from the left side, or, "tucked away". Only fair - mutation is a "side" effect after all!

(Less obviously, this won't work if array destructuring is lazy - but most languages aren't.)

I must warn you against getting carried away with this pattern, as a counter-example, one might be tempted to "consolidate" the above code into:

```TypeScript
const [ number1, , number2, , number3 ] = [ 
    buffer.readInt8(index), ++index,
    buffer.readInt16BE(index), index += 2,
    buffer.readInt8(index), ++index 
    ];
```

While lower in character count, the intention of the code is obscured, the noise is back. Gotta know where to draw the line!

The "tuck-away" gets me by pretty well, but it's no defense against constructing / initialising objects.

```Typescript
const buffer = Buffer.from([1, 2, 3, 4]);
let index = 0;
let lovelyObject = {};
[ lovelyObject.number1 ] = [ buffer.readInt8(index), ++index ];
[ lovelyObject.number2 ] = [ buffer.readInt16BE(index), index += 2 ];
[ lovelyObject.number3 ] = [ buffer.readInt8(index); ++index ];
```

This is not bad, but I'd really like to be able to write,

```TypeScript
const lovelyObject = {
    number1: ...,
    number2: ...,
    number3: ...,
}
```

And the workaround is easy.

## The take-one
 
```Typescript
let lovelyObject = {
    number1: [ buffer.readInt8(index), ++index ][0],
    number2: [ buffer.readInt16BE(index), index += 2 ][0],
    number3: [ buffer.readInt8(index); ++index ][0]
};
```

Not the worst thing ever. If I am to nitpick, this still looks slightly obscure, as the intention of only using the first element (at index `0`) is only revealed at the end of each line. As programmers do, we can extract a function to help with readability,

```Typescript
let lovelyObject = {
    number1: _1(buffer.readInt8(index), ++index),
    number2: _1(buffer.readInt16BE(index), index += 2),
    number3: _1(buffer.readInt8(index); ++index)
};
```

Whereas `_1`, short for "take-one", is defined as

```TypeScript
function _1<T>(v: T, ...vs: []any): T {
    return v;
}
```

# the happily hoisted

I was breathless when I first saw how `where` is used in this Haskell code snippet on [https://www.haskell.org/](https://www.haskell.org/)

```haskell
primes = filterPrime [2..]
  where filterPrime (p:xs) =
          p : filterPrime [x | x <- xs, x `mod` p /= 0]
```

See how `filterPrime` is declared AFTER it's used? (Take nothing away from the other reasons to be breathless about this snippet)

Turns out, JavaScript has always got this right through hoisting. So it's possible to write

```TypeScript
function foo(x: number) {
    return bar(x);

    function bar(y: number) {
        return y * 2;
    }
}
```

I have reservations about hoisting of variables, but don't mind that of functions at all!