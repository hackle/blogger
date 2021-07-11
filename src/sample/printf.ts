declare function printf<T extends string>(format: string, ...values: Values<T>): Formatted<T>;

type Values<T extends string> = 