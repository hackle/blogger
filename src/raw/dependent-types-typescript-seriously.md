With dependent types, we can calculate types based on values. An introductory example is `concat`, with which the result vector should have the combined lengths of both parameters. `m` and `n` are natural numbers that can be added.

```idris
concat : Vect m a -> Vect n a -> Vect (m + n) a
```

It does imply that values must be available on type level for the calculation, as `m` and `n` above. This rules out most programming languages already, especially those mainstream. The glaring exception is, you would have guessed, TypeScript.

We will look at the enabling features first. If you are quite well versed, feel free to skip to the **Variadic map** example.

## Values in / as types
It's quite common to see values used in types for day-to-day TypeScript code, for example:

```TypeScript
type WeekendDay = 'Saturday' | 'Sunday';
```

More shockingly, blatant use of values as types.

```TypeScript
type Foo = { bar: 'baz' };
type Hom = [ 1, 2, 3 ];
```

It's also routine to infer types from values. 

```TypeScript
const foo = { bar: 'baz' };
type Foo = typeof foo;

const hom = [ 1, 2, 3 ] as const;
type Hom = typeof hom;
```

## Equality
A good starting point for any calculation is equality. It may seem impossible without `===` on type level, but there is `extends`.

```TypeScript
type TypeEqual<T, U> = 
    T extends U
    ? U extends T
        ? true
        : false
    : false;

const n: number = 1;
const bothNumbers: TypeEqual<number, typeof n> = true;
const areEqual: TypeEqual<1, 1> = true;
const notEqual: TypeEqual<1, 2> = false;
// const notEqual: TypeEqual<number, string> = true; // won't compile
```

This roughly works as: if `A >= B && B >= A` then `A == B`.

## Pattern matching

It's pretty standard to use `extends` + `infer` for pattern matching on type level. 

The below example extracts the type of a `foo` field considering it can be nested differently.

```TypeScript
type ExtractFoo<T> = 
    T extends { foo: infer U }
    ? U
    : T extends { bar: { foo: infer U } }
        ? U
        : never;

const withFoo1 = { foo: new Date() };
type Foo1 = ExtractFoo<typeof withFoo1>;    // Date

const withFoo2 = { bar: { foo: true } };
type Foo2 = ExtractFoo<typeof withFoo2>;    // boolean
```

## Recursion

The sharp-eyed reader would have noticed the much coveted opportunity for recursion in the above example, which turns out to be simple on type level too.

```TypeScript
type ExtractFooRec<T> = 
    T extends { foo: infer U }
    ? U
    : T extends { bar: infer T1 }
        ? ExtractFooRec<T1>         // recursion!
        : never;

type FooRec1 = ExtractFooRec<typeof withFoo1>;    // Date

type FooRec2 = ExtractFooRec<typeof withFoo2>;    // boolean
```

## Tuples

Pattern matching tuples is essential for this post. The below example removes the `readonly` modifier (which is possible too!) from a tuple type.

```TypeScript
type NoReadOnly<T> =
    T extends readonly [...infer U]
    ? U
    : T;

const tuple = [ 1, 'true', false ] as const;
type TupsReadonly = typeof tuple;       // readonly [1, "true", false]
type Tups = NoReadOnly<TupsReadonly>;   // [1, "true", false]
```

## Collapse, entropy

Tuples especially need explicit typing or be marked with `as const`, otherwise they collapse to arrays of union types. 

```TypeScript
const collapsed = [ 1, 'true', false ]; // (string | number | boolean)[]
```

Entropy also exists for type alias as TypeScript is structurally typed, not nominally.

```TypeScript
type TypeOfTrue<T> =
    T extends 'true'
    ? string
    : T extends true
        ? boolean
        : never;

type TrueDat = TypeOfTrue<'true'>;   // string, not TypeOfTrue<'true'> 
```

## Variadic functions
Compared to `as const`, it appears less obtuse to conserve types for functions with flexible number of parameters.

```TypeScript
declare function params<T extends any[]>(...params: T): T;
const ps = params(true, 'chocolate', 3);  // [boolean, string, number]
```

We will use either tuples or variadic functions for following examples. In most cases they should be inter-changeable.

Now we are ready for some dependent types!

## Variadic map

Let's follow with another variadic function, this time as parameter `mapper` to function `mapMany`. Unlike the typical map function `map : (mapper: a -> b) -> [a] -> [b]`, the number and types of following parameters depend on the parameters of the `mapper` function, for example, `mapMany: (mapper: a -> b -> c) -> [a] -> [b] -> [c]`.

We already know it's possible to type indefinite number of parameters with `mapper<T extends any[]>(...parameters: T)`, what's less well-known is to use `infer` to restore the type of each element of `T`. Reversed entropy!

```TypeScript
type ToArrays<T extends any[]> =
    T extends []
    ? []
    : T extends [infer T1, ...infer Ts]
        ? [T1[], ...ToArrays<Ts>]
        : never;

declare function mapMany<T extends any[], U>(mapper: (...ts: T) => U, ...ps: ToArrays<T>);

mapMany((a: string) => `${a}`, [ 'hello', 'world' ]);
mapMany((a: string, b: number) => `${a} ${b}`, [ 'hello', 'world' ], [ 2, 3 ]);
```

(There are suggestions the above example would be easier to understand if it's called `zipMany`).

## concat

Who would have guessed - `concat` is relatively trivial to type in TypeScript. In curried form below.

```TypeScript
declare function concat<T extends any[]>(...ts: T): <U extends any[]>(...us: U) =>[...T, ...U];

const merged = concat(1, 'true')('hero', new Date());   // [number, string, string, Date]
```

## reverse

We take advantage of pattern matching and type inference to inductively reconstruct a type for `reverse`.

```TypeScript
type Reverse<T extends any[]> =
    T extends [infer T1, ...infer Ts]
    ? [ ...Reverse<Ts>, T1 ]
    : T;

declare function reverse<T extends any[]>(...ts: T): Reverse<T>;

const isReversed = reverse(1, true, 'hero');    // [string, boolean, number]
```

## Remove element

Using a `readonly` tuple with literal values, it's possible to remove an element from a tuple on type level.

```TypeScript
type Remove<T extends any[], U> =
    T extends [infer T1, ...infer Ts]
    ? TypeEqual<T1, U> extends true
        ? Ts                        // a match is found, remove T1 / U
        : [T1, ...Remove<Ts, U>]     // keep looking
    : T;

declare function remove<T extends readonly any[], U>(ts: T, t: U): Remove<NoReadOnly<T>, U>;

const oneLess = remove([ 1, 2, 3 ] as const, 2 as const);   // [1, 3]
const unchanged = remove([ 1, 2, 3 ] as const, 4 as const); // [1, 2, 3]
```

## Peano number

While we can also define Peano numbers as tuples, below is the more traditional object form.

```TypeScript
type Nat = 0 | { suc: Nat };
```

Now let's utilise the clever trick of [self-referenced JSON](self-reference-json) to initialise some natural numbers. They will be handy later on.

```TypeScript
const nats = (() => ({
    0: 0,
    get 1 () { return { suc: nats[0] }; },
    get 2 () { return { suc: nats[1] }; },
    get 3 () { return { suc: nats[2] }; },
    get 4 () { return { suc: nats[3] }; }
} as const))();

type Nats = typeof nats;
```

Inductively we can add two natural numbers.

```TypeScript
type NatAdd<N1 extends Nat, N2 extends Nat> =
    N2 extends 0
    ? N1
    : N2 extends { suc: infer N3 }
        ? N3 extends Nat
            ? NatAdd<{ suc: N1 }, N3>
            : never
        : never;

const nat4: NatAdd<Nats[1], Nats[3]> = nats[4];
// const nat3: NatAdd<Nats[1], Nats[2]> = nats[4]; // does not compile
```

Subtraction is much more verbose, but the idea is similar.

```TypeScript
type NatMinus<N1 extends Nat, N2 extends Nat> =
    N2 extends 0
    ? N1
    : N1 extends { suc: infer N3 }
        ? N2 extends { suc: infer N4 }
            ? N3 extends Nat
                ? N4 extends Nat
                    ? NatMinus<N3, N4>
                    : never
                : never
            : never
        : never;

const nat1: NatMinus<Nats[4], Nats[3]> = nats[1];
// const nat2: NatMinus<Nats[3], Nats[1]> = nats[1]; // won't compile
```

## Embedding

It's also possible to calculate types from other types if there is a way to connect them. For example, the above `Nats` type connects / embeds *normal* numbers (as key) to `Nat` numbers (as value), so we can create a type that compares numbers.

```TypeScript
type GTE<T1 extends Nat, T2 extends Nat> =
    TypeEqual<T1, T2> extends true
    ? true
    : T2 extends 0
        ? true
        : T1 extends { suc: infer T3 }
            ? T2 extends { suc: infer T4 }
                ? T3 extends Nat
                    ? T4 extends Nat
                        ? GTE<T3, T4>
                        : never
                    : never
                : never
            : false;

type NumGTE<N1 extends keyof Nats, N2 extends keyof Nats> = GTE<Nats[N1], Nats[N2]>;

const isGTE: NumGTE<2, 1> = true;
const notGTE: NumGTE<1, 2> = false;
```

## Ordering

To bring a few things together, `IsOrdered` enforces on type level that an array of numbers must be in order, be it descending or ascending.

```TypeScript
type AnyGTE<T1, T2> =
    T1 extends keyof Nats
    ? T2 extends keyof Nats
        ? GTE<Nats[T1], Nats[T2]>
        : false
    : false;

type IsDesc<T extends any[]> =
    T extends []
    ? true
    : T extends [infer _]
        ? true
        : T extends [infer T1, infer T2, ...infer Ts]
            ? AnyGTE<T1, T2> extends true
                ? IsDesc<[T2, ...Ts]>
                : false
            : false;

type IsOrdered<T extends any[]> = 
    IsDesc<T> extends true
    ? true
    : IsDesc<Reverse<T>>;

const isAsc: IsOrdered<[ 1, 2, 3 ]> = true;
const isDesc: IsOrdered<[ 3, 2, 1 ]> = true;
const notOrdered: IsOrdered<[1, 3, 2]> = false;
```

And this is how we can use the types for `first`, a function that takes an array in order.

```TypeScript
type FirstOf<T extends any[]> = T extends [ infer T1, ...infer _ ] ? T1 : never;

declare function first<T extends readonly any[]>(ns: T): IsOrdered<NoReadOnly<T>> extends true ? FirstOf<NoReadOnly<T>> : never;

const m1 = first([ 1, 2, 3 ] as const);   // 1
const m3 = first([ 3, 2, 1 ] as const);   // 3
const m_ = first([ 3, 2, 3 ] as const);   // never
```

That's it for now. Let me know if you find more interesting examples!