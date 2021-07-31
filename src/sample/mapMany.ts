declare function mapMany<T extends any[], U>(fn: (...ps: T) => U, ...ts: Distribute<T>): U[];
type AB = 'a' | 'b' | 'c' | 'd';
type BC = 'a' | 'b' | 'e';

type Exc<T, U> = T extends U ? never : T;
type X = Exc<AB, BC>;

type Arr<T> = T extends boolean ? boolean[] : T extends infer B ? B[] : never;
type As = Arr<boolean | string | number>;
const as: As = [true, false];

type Distribute<T> = T extends [u: infer U, ...v: infer V] ? [...Distribute<V>, U[]] : [];

var x: Distribute<[a: boolean, b: number]>;

mapMany((a: string, b: number, c: Date) => true, [new Date()], [ 1, 2, 3], ['a', 'b', 'c']);

// alternatively

declare function mapManyArray<T extends any[], U>(fn: (...ps: T) => U, ...ts: DistributeArray<T>): U[];

type Stream<T> = T[];
type DistributeArray<T> = T extends [u: infer U, ...v: infer V] 
                        ? U extends Array<infer W> ? [Stream<W>, ...DistributeArray<V>] : DistributeArray<V>
                        : [];

var x: Distribute<[a: boolean, b: number]>;

mapManyArray((a: string[], b: number[], c: Date[]) => true, ['a', 'b', 'c'], [ 1, 2, 3], [new Date()]);
