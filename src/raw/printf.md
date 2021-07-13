With template literal types in TypeScript, we can make our own mini language, such as a strongly typed version of `printf`, that can be called as,

```TypeScript
printf('I am %s, %d years old, born in %D', arg1, arg2, arg3);
```

First thing first, let's agree on the terms: `%s, %d, %D` are called specifiers; the string with the specifiers is called the "format" or "template", to avoid confusion with the TypeScript type "template literal" let's call it "format". 

# Constructing, destructuring symmetry, and pattern matching

Ideally we want symmetric constructing and destructuring, or, something can be destructured the same way it's constructed.

This is actually true most of the time for values, for example with maps `const { foo, bar } = { foo: 'foo', bar: 1 }`, or arrays / tuples `const [foo, bar] = ['foo', 'bar']`.

It should come as a pleasant surprise that such destructuring works equally well on type level. The trick is to use type inference (`extends + infer`) to destructure or pattern match on a type.

```TypeScript
type FooType<T> = T extends { foo: infer F } ? F : never;

// must have number type
const fieldFoo: FooType<{ foo: number }> = 123;
```

# Pattern matching on literals

Less well-known is with template literal types, with which we can construct a literal type in the similar way as string interpolation.

```TypeScript
type Mixed = `${number} ${boolean} ${string}`;

// must follow the format
const mixed: Mixed = "1 true friend";

// Type '"one TRUE friend"' is not assignable to type '`${number} false ${string}` | `${number} true ${string}`'.ts(2322)
const mixed1: Mixed = "one TRUE friend";
```

It's also possible to pattern match on a string literal to recover the components. Here is how it goes.

```TypeScript
type LeadingNumber<T extends string> = 
    T extends `${infer N}${infer _}`
    ? N extends `${number}`
        ? N
        : never
    : never;

const leadingNumber: LeadingNumber<'1 true love'> = '1';

// Type '"2"' is not assignable to type '"1"'.ts(2322)
const leadingNumber2: LeadingNumber<'1 true love'> = '2';
```

Two things to note here,

- it's not completely symmetric as we can only recover `'1'` as a string, not as a number. I haven't figured out type-level coercion yet, if that's possible?

- pattern matching is lazy, proof being `` T extends `${infer N}${infer _}` `` alone is not enough to infer the leading number unless combined with the following `` N extends `${number}` ``.

# Recursive parsing

Throw in recursion we can do something quite interesting already. Here is a super strongly-typed `Split` type.

```TypeScript
// D for delimiter
type Split<D extends string, T extends string> =
    T extends `${infer Head}${D}${infer Tail}`
    ? [Head, ...Split<D, Tail>]
    : [T];

const parts: Split<',', 'you,me,we'> = ['you', 'me', 'we'];

// Type '"yo"' is not assignable to type '"you"'.ts(2322)
const parts2: Split<',', 'you,me,we'> = ['yo', 'me', 'we'];
```

# printf

Now we are ready for the strongly-typed `printf`.

To start with the easy parts let's first declare 
the specifiers to be supported. This is done with a map type - that maps a specifier to a type that we expect the corresponding value to have, when `printf` is called.

```TypeScript
type Specifiers = {
    's': string,
    'd': number,
    'b': boolean,
    'D': Date
};

type Spec = keyof Specifiers;
```

And using a similar technique to infer the specifier. Note it only works if we help the pattern matching by specifying the `%` symbol - my guess is otherwise it's hard to decide where to stop to infer `K` as it's sandwiched by two other  inferences with no constraints.

```TypeScript
type Values<T extends string> = 
    T extends `${infer _}%${infer K}${infer Rest}`
    ? K extends Spec
        ? [ Specifiers[K], ...Values<Rest> ]
        : Values<Rest>
    : [];
```

The above type also ignores unsupported patterns. This is how it works.

```TypeScript
declare function printf<T extends string>(format: T, ...values: Values<T>): string;

const r = printf('this is a %s and it is %d %wyears old, right?%b %D %i %f', 'Hackle', 20, true, new Date());
```

Next I'll restrict the return type to follow the format, other than being a plain `string`. Coming soon.