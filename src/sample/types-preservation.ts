let day1 = 'Saturday';
const day2 = 'Saturday';

let map1 = { day: 'Saturday' };
const map2 = { day: 'Saturday' };
const map3 = { day: 'Saturday' } as const;

const tup1 = [ true, 'love' ];
const tup2 = [ true, 'love' ] as const;

declare function needLiteralStr(foo: string): typeof foo;

const l1 = needLiteralStr('foo');

declare function needLiteralStr1<T extends string>(foo: T): T;

const l2 = needLiteralStr1('foo');

declare function needLiteralMap(foo: Record<string, string>): typeof foo;

const m1 = needLiteralMap({ foo: 'foo', bar: 'bar' });

// const m1: Record<string, string>

declare function needLiteralMap2<
    T extends string, 
    V extends string, 
    M extends Record<T, V>
>(foo: M): M;

const m2 = needLiteralMap2({ foo: 'foo', bar: 'bar' });

// const m2: { foo: "foo"; bar: "bar"; }

declare function needLiteralMap3<
    T extends string, 
    V extends string | number, 
    M extends Record<T, V>
>(foo: M): M;

const m3 = needLiteralMap3({ foo: 'foo', bar: 1 });

// const m2: { foo: "foo"; bar: "bar"; }

// declare function needTup(foo: [??])

declare function needTup<T extends any[]>(...foo: T): T;

const t1 = needTup(true, 'love');

// const t1: [boolean, string]
