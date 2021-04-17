declare function mapMany<T extends any[], U>(fn: (...ps: T) => U, ...ts: Distribute<T>): U[];

type Distribute<T> = T extends [u: infer U, ...v: infer V] ? [U[], ...Distribute<V>] : [];

var x: Distribute<[a: boolean, b: number]>;

mapMany((a: string, b: number, c: Date) => true, ['a', 'b', 'c'], [ 1, 2, 3], [new Date()]);

