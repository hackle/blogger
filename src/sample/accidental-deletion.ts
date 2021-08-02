type Values = { foo: string, bar: number };
const vals: Values = { 
    foo: 'hello', 
    bar: 88 
};

function toGetters(v: Values) {
    return {
        foo: () => v.foo,   // not safe
        bar: () => v.bar    // not safe
    };
}

type Getters = { [K in keyof Values]: () => Values[K] };
function toGettersSafe(v: Values): Getters {
    return {
        foo: () => v.foo,   // safe
        bar: () => v.bar    // safe
    };
}