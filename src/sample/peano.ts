type Nat = 0 | { s: Nat };

const nats = (() => ({
    0: 0,
    get 1 () { return { s: nats[0] }; },
    get 2 () { return { s: nats[1] }; },
    get 3 () { return { s: nats[2] }; },
    get 4 () { return { s: nats[3] }; }
} as const))();

type Nats = typeof nats;

type VectorOf<T, U> =  T extends 0 
                        ? [] 
                        : T extends { s: infer N }
                            ? [U, ...VectorOf<N, U>]
                            : never;

interface Vect<T extends Nat, U extends any> {
    length: T,
    items: MustBeOfLength<T, U>;
}

const v2: Vect<Nats[2], [string, number]> = {
    length: nats[2],
    items: ['a', 2]
}

type GTE<T extends Nat, U extends Nat> = 
    T extends U
    ? true
    : T extends { s: infer T1 }
        ? T1 extends Nat 
            ? GTE<T1, U>
            : false
        : false;

type GTE_<T, U> = 
    T extends Nat
    ? U extends Nat
        ? GTE<T, U>
        : false
    : false;

const gte: GTE<Nats[2], Nats[1]> = true;
const notGte: GTE<Nats[2], Nats[3]> = false;
const justEqual: GTE<Nats[2], Nats[2]> = true;

type And<T1 extends boolean, T2 extends boolean> =
    T1 extends true
    ? T2 extends true
        ? true
        : false
    : false;

type Ordered<T> = 
    T extends []
    ? true
    : T extends [infer _]
        ? true
        : T extends [infer T1, infer T2, ...infer Ts]
            ? And<GTE_<T1, T2>, Ordered<[T2, ...Ts]>>
            : false;

const isOrdered: Ordered<[Nats[3], Nats[2], Nats[1], Nats[0]]> = true;

type MapToNat<T extends number[]> =
    T extends []
    ? []
    : T extends [ infer T1, ...infer Ts ]
        ? T1 extends (keyof Nats)
            ? Ts extends number[]
                ? [Nats[T1], ...MapToNat<Ts>]
                : never
            : never
        : never;

type NoReadOnly<T> = T extends readonly [...infer Ts] ? Ts : T;

type OrderedNat<T extends number[]> = Ordered<MapToNat<T>>;

const nums = [3, 2, 1, 1] as const;
const isOrderedNat: OrderedNat<NoReadOnly<typeof nums>> = true;

const k: keyof Nats = 1;

type Add<M, N> = N extends 0
                    ? M
                    : N extends { s: infer N1 }
                        ? Add<{ s: M }, N1>
                        : never;

type Arr2 = VectorOf<Add<Nats['1'], Nats['2']>, number>;

// declare function addVector<M, N>(v1: M, v2: N): AddVector<M, N>;
declare function addVector<M, N, U>(v1: VectorOf<M, U>, v2: VectorOf<N, U>): VectorOf<Add<M, N>, U>;

// const result = addVector(asVect(123), asVect(456, 789, 1011));

type LengthOf<T> = T extends [] 
                    ? 0 
                    : T extends [infer _, ...infer Ts]
                        ? { s: LengthOf<Ts> }
                        : never;

type l2 = LengthOf<[1, 2, 3]>;

type AreEqual<T, U> = T extends U 
                        ? U extends T 
                            ? true
                            : false
                        : false;

type IsOfLength<U extends Nat, T> = AreEqual<U, LengthOf<T>>;

type IsEq = IsOfLength<Nats[2], [1, 2]>;

type MustBeOfLength<U extends Nat, T> = IsOfLength<U, T> extends true ? T : never;

type MustBe2 = MustBeOfLength<Nats[2], [1, 2]>;

// const rv1 = addVector([1, 2, 3], [1, 2]);

type AddVector<M, N> =
    M extends [...infer _]
    ? N extends []
        ? M
        : N extends [infer N1, ...infer NS]
            ? AddVector<[...M, N1], NS>
            : never
    : never;

function asVect<T extends any[]>(...args: T): VectorOf<ToNat<T>, FirstOf<T>> {
    return args as any;
}

type FirstOf<T> = T extends [infer T1, ...infer _] ? T1 : never;

type ToNat<T> =
    T extends [infer _, ...infer Ts]
    ? { s: ToNat<Ts> }
    : 0;