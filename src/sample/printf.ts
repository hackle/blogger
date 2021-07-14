type Specifiers = {
    's': string,
    'd': number,
    'b': boolean,
    'D': Date
};

type Spec = keyof Specifiers;

type Values<T extends string> = 
    T extends `${infer _}%${infer K}${infer Rest}`
    ? K extends Spec
        ? [ Specifiers[K], ...Values<Rest> ]
        : Values<`${K}${Rest}`>
    : [];

type Formatted<T extends string> = 
    T extends `${infer Head}%${infer K}${infer Tail}`
    ? K extends Spec 
        ? `${Head}${string}${Formatted<Tail>}`
        : `${Head}%${Formatted<`${K}${Tail}`>}`
    : T;

declare function printf<T extends string>(format: T, ...values: Values<T>): Formatted<T>;

const r = printf('this is a %%s and it is %d %wyears old, right?%b %D %i %f', 'Hackle', 20, true, new Date());

// const { foo, bar } = { foo: 'foo', bar: 1 };
const [foo, bar] = ['foo', 'bar'];

type FooType<T> = T extends { foo: infer F } ? F : never;

// must have number type
const fieldFoo: FooType<{ foo: number }> = 123;

type Mixed = `${number} ${boolean} ${string}`;

// must follow the format
const mixed: Mixed = "1 true friend";

// Type '"one TRUE friend"' is not assignable to type '`${number} false ${string}` | `${number} true ${string}`'.ts(2322)
// const mixed1: Mixed = "one TRUE friend";

type LeadingNumber<T extends string> = 
    T extends `${infer N}${infer _}`
    ? N extends `${number}`
        ? N
        : never
    : never;

const leadingNumber: LeadingNumber<'1 true love'> = '1';

// Type '"2"' is not assignable to type '"1"'.ts(2322)
// const leadingNumber2: LeadingNumber<'1 true love'> = '2';

// D for delimiter
type Split<D extends string, T extends string> =
    T extends `${infer Head}${D}${infer Tail}`
    ? [Head, ...Split<D, Tail>]
    : [T];

const parts: Split<',', 'you,me,we'> = ['you', 'me', 'we'];

// Type '"yo"' is not assignable to type '"you"'.ts(2322)
// const parts2: Split<',', 'you,me,we'> = ['yo', 'me', 'we'];