Once in a while, we'll come across the need for a collection with different types (that are not known at the time of creation), and it usually presents challenges to using types safely and efficiently. Let's look at a few popular examples.

## ActionReducerMap

This comes from `@ngrx/store` (whether it's enjoyable or not is a different issue) - `ActionReducerMap`, an attempt to bring Actions and reducer functions together.

`Action`s can be different types, lets say `ActionA`, `ActionB` and `ActionC`, all of them extend something like an `ActionBase`.

Reducers are just functions of signature `(st: State, action: ActionXx) => State`.

So it happens that we have an aggregate objects in the form of

```typescript
type StateRoot = { key1: State1; key2: State2; ... };
```

And the reducers map this structure as follows:

```typescript
{
    key1: (st: State1, action: Actions1) => State1,
    key2: (st: State2, action: Actions2) => State2,
    ...
}
```

And one way to do it is with something like this,

```typescript
type ActionReducerMap<T, V extends Action = Action> = {
  [p in keyof T]: (state: T[p], action: V) => T
};
```

This is neat in that it uses `keyof T` to use the structure of `T` as well as types of its values (through `T[p]`). There are a couple of problems with this,
- 


