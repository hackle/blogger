export {};

// values and types
type WeekendDay = 'Saturday' | 'Sunday';
type Foo = { bar: 'baz' };
type Hom = [ 1, 2, 3 ];

const foo = { bar: 'baz' };
type FooL = typeof foo;

const hom = [ 1, 2, 3 ] as const;
type HomL = typeof hom;



// equality
type TypeEqual<T, U> = 
    T extends U
    ? U extends T
        ? true
        : false
    : false;

const n: number = 1;
const bothNumbers: TypeEqual<number, typeof n> = true;
const areEqual: TypeEqual<1, 1> = true;
const notEqual1: TypeEqual<1, 2> = false;
const notEqual2: TypeEqual<number, string> = false; // won't compile





// pattern matching
type ExtractFoo<T> = 
    T extends { foo: infer U }
    ? U
    : T extends { bar: { foo: infer U } }
        ? U
        : never;

const withFoo1 = { foo: new Date() };
type Foo1 = ExtractFoo<typeof withFoo1>;    // Date

const withFoo2 = { bar: { foo: new Date() } };
type Foo2 = ExtractFoo<typeof withFoo2>;    // boolean


// pattern matching / tuples
type NoReadOnly<T> =
    T extends readonly [...infer U]
    ? U
    : T;

const tuple = [ 1, 'true', false ] as const;
type TupsReadonly = typeof tuple;       // readonly [1, "true", false]
type Tups = NoReadOnly<TupsReadonly>;   // [1, "true", false]




// recursion
type ExtractFooRec<T> = 
    T extends { foo: infer U }
    ? U
    : T extends { bar: infer T1 }
        ? ExtractFooRec<T1>         // recursion!
        : never;

type FooRec1 = ExtractFooRec<typeof withFoo1>;    // Date

type FooRec2 = ExtractFooRec<typeof withFoo2>;    // boolean





// entropy / collapse

const collapsed = [ 1, 'true', false ] as const; // (string | number | boolean)[]

type TypeOfTrue<T> =
    T extends 'true'
    ? string
    : T extends true
        ? boolean
        : never;

type TrueDat = TypeOfTrue<true>;   // string, not TypeOfTrue<'true'> 




// reversed entropy
// variadic
declare function params<T extends any[]>(...params: T): T;
const ps = params(true, 'chocolate', 3);  // [boolean, string, number]



// map many
type ToArrays<T extends any[]> =
    T extends []
    ? []
    : T extends [infer T1, ...infer Ts]
        ? [T1[], ...ToArrays<Ts>]
        : never;

declare function mapMany<T extends any[], U>(mapper: (...ts: T) => U, ...ps: ToArrays<T>);

mapMany((a: string) => `${a}`, [ 'hello', 'world' ]);
mapMany((a: string, b: number, c: boolean) => `${a} ${b}`, 
        [ 'hello', 'world' ], 
        [ 2, 3 ],
        [true, false]);






// Vect / concat
declare function concat<T extends any[]>(...ts: T): <U extends any[]>(...us: U) =>[...T, ...U];
const merged = concat(1, 'true')('hero', new Date(), 1 as const);   // [number, string, string, Date]





// Vect / reverse

type Reverse<T extends any[]> =
    T extends [infer T1, ...infer Ts]
    ? [ ...Reverse<Ts>, T1 ]
    : T;

declare function reverse<T extends any[]>(...ts: T): Reverse<T>;

const isReversed = reverse(1, true, 'hero');    // [string, boolean, number]





// Vect / remove element
type Remove<T extends any[], U> =
    T extends [infer T1, ...infer Ts]
    ? TypeEqual<T1, U> extends true
        ? Ts                        // a match is found, remove T1 / U
        : [T1, ...Remove<Ts, U>]     // keep looking
    : T;
declare function remove<T extends readonly any[], U>(ts: T, t: U): Remove<NoReadOnly<T>, U>;

const oneLess = remove([ 1, 2, 3 ] as const, 2 as const);   // [1, 3]
const unchanged = remove([ 1, 2, 3 ] as const, 4 as const); // [1, 2, 3]





// Peano / natural number

type Nat = 0 | { suc: Nat };

const nats = (() => ({
    0: 0,
    get 1 () { return { suc: nats[0] }; },
    get 2 () { return { suc: nats[1] }; },
    get 3 () { return { suc: nats[2] }; },
    get 4 () { return { suc: nats[3] }; },
    get 5 () { return { suc: nats[4] }; },
    get 6 () { return { suc: nats[5] }; },
    get 7 () { return { suc: nats[6] }; },
    get 8 () { return { suc: nats[7] }; },
    get 9 () { return { suc: nats[8] }; },
    get 10 () { return { suc: nats[9] }; },
} as const))();

const nat11 = nats[11];

type Nats = typeof nats;

declare function addNat<N1 extends Nat, N2 extends Nat>(n1: N1, n2: N2): NatAdd<N1, N2>; 

// addition

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

type NatMultiply<N1 extends Nat, N2> =
    N2 extends 0
    ? 0
    : N2 extends Nats[1]
        ? N1
        : N2 extends { suc: infer N3 }
            ? NatAdd<N1, NatMultiply<N1, N3>>
            : never;

const natM4: NatMultiply<Nats[2], Nats[2]> = nats[4];
const natM10: NatMultiply<Nats[2], Nats[5]> = nats[10];

// subtraction

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

// embedding

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

type NumGTE<N1 extends keyof Nats, N2 extends keyof Nats> =
    GTE<Nats[N1], Nats[N2]>;

const isGTE: NumGTE<2, 1> = true;
const notGTE: NumGTE<1, 2> = false;






// ordering
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




// first of an ordered array
type FirstOf<T extends any[]> = T extends [ infer T1, ...infer _ ] ? T1 : never;
declare function first<T extends readonly any[]>(ns: T): IsOrdered<NoReadOnly<T>> extends true ? FirstOf<NoReadOnly<T>> : never;

const m1 = first([ 1, 2, 3 ] as const);   // 1
const m3 = first([ 3, 2, 1 ] as const);   // 3
const m_ = first([ 3, 2, 3 ] as const);     // never

type ExtraName<T> =
    T extends `firstname: ${infer FirstName} lastname: ${infer LastName} age: ${infer Age}`
    ? Age extends number 
        ? { firstname: FirstName, lastname: LastName, age: Age }
        : never
    : never;

declare function extractName<T extends string>(raw: T): ExtraName<T>;

const fullName = extractName('firstname: Hackle lastname: Wayne age: 32');

type ToOptional<T> = {
    [K in keyof T]: undefined extends T[K] ? { K?: T[K] } : T[K]
};

type DuckTyped = { foo: 'foo', bar: 'bar' } extends { foo: 'foo' } ? true : false;
