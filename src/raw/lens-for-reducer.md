Once having got used to the idea, it's not hard to see that any `lens` implementation can help reduce boilerplate in transformation of complex data structure, for example, in so called "reducer" functions. Let's see how we can utilise something simple as `tsMiniLens` to arrive at a minimalistic but type-safe replacement for the usual reducers, in `TypeScript`.

Our goal is to have a fluent interface to build reducers with type-safety as well as type inference.

```typescript
const reducer = new LensReducer<State>()
    .register(UpdateBirthdayAction, lStateTo.birthday)
    .register(UpdateNameAction, lStateTo.name)
    .toReducer();
```

Let's start with a *usual* way of setting up a reducer.

```TypeScript
class UpdateBirthdayAction {
    payload: Date;
}

class UpdateNameAction {
    payload: string;
}

type State = {
  name: string,
  birthday: Date
}

function reducer(state: State, action: Action): State {
    if (action instanceof UpdateBirthdayAction)
        return { ...state, birthday: action.payload };
    
    if (action instanceof UpdateNameAction)
        return { ...state, name: action.payload };
    
    return state;
}

```

It's good to be aware that implementations vary, there are conventions with much more ceremony, such as union `Action` types, enum as tags for `Action` etc. I picked the above style for its lack of ceremony (largely due to representing `Action` as class and use of `instanceof`) as well as distraction from the topic at hand.

I can't say I am a big fan of the whole `redux` fetish but if I have to use it then I wouldn't mind writing a `reducer` as the one above.

There is but one small annoyance, the way the `state` value is being updated is quite tedious, with code blocks like `{ ...state, birthday: action.payload }` or `{ ...state, name: action.payload }`. This can get pretty messy if the data structure at hand is complex.

Worry not - `lens` excels at eliminating such repetitions. First, let's define a few lenses for `State`. (You may want to click [here](lens-typescript) for a quick recap of `tsMiniLens`.)

```typescript
const lStateTo = {
    name: lensFrom<State>().to('name'),
    birthday: lensFrom<State>().to('birthday')
};
```

**lFooTo.bar** is a bit of hungarian notation (that you don't need to follow) I use to wrap lenses for an object; it simply means "lens from Foo to bar". 

With the above we can do `lStateTo.name.view({ name: 'foo', birthday: ... })` or `lStateTo.name.set(state, 'Jack')`. Meanwhile, the `reducer` function becomes:

```typescript
function reducer(state: State, action: ActionUnion): State {
    if (action instanceof UpdateBirthdayAction)
        return lStateTo.birthday.set(state, action.payload);
    
    if (action instanceof UpdateNameAction)
        return lStateTo.name.set(state, action.payload);
    
    return state;
}
```

This may not seem much difference as `State` is simple - but we are on the right track.

The repetition in `reducer` is more pronounced now - with every `instanceof` it tries to recover the type of `action` so the `payload` can be accessed by following code.
(Read about [instanceof type guard](https://www.typescriptlang.org/docs/handbook/advanced-types.html#instanceof-type-guards))

Such repetition is something we can abstract over. One option is to use something like a **builder pattern** to build up a reducer with one `Action` type at a time.

Now the idea is in place, we need only add a few helper types to get underway.

```typescript
interface IAction<TPayload> { payload?: TPayload }

type TypeOf<T> = {
    new(...args): T;
}

type Reducer<TState, TAction> = (st: TState, action: TAction) => TState;
```

And here comes the `LensReducer` with which we can register `Action`s with corresponding lenses, and make a `reducer` function.

```typescript
class LensReducer<TState> {
    private reducerFns: Reducer<TState, any>[] = [];

    register<TPayload>(ty: TypeOf<IAction<TPayload>>, lens: Lens<TState, TPayload>): LensReducer<TState> {
        this.reducerFns.push(
            (st: TState, action: IAction<TPayload>) => 
                action instanceof ty ? lens.set(st, action.payload) : st);

        return this;
    }

    toReducer(): Reducer<TState, IAction<any>> {
        return (state: TState, action: IAction<any>) =>
            this.reducerFns.reduce((st, fn) => fn(st, action), state);
    }
}
```

We want the method `ReducerBuilder.register()` to take type of `payload` of an `Action` as type parameter. This type restricts the first parameter of `register` to an `Action` with it as payload, and the second parameter to a `lens` that focuses on a `string` value of a `State` structure. 

For example: `register<string>(UpdateNameAction, lStateTo.name)`. By specifying `string`, it's required that `UpdateNameAction` must have a `payload` of `string`; and `lStateTo.name` must be a lens that manipulates a `string` field within a structure of type `State` (which is encoded in the type of `ReducerBuilder`). All this should follow by just specifying a type parameter.

It is used as follows

```typescript
// and the Action classes need to implement IAction
class UpdateBirthdayAction implements IAction<Date> {
    constructor(public payload: Date){};
}

class UpdateNameAction implements IAction<string> {
    constructor(public payload: string){};
}

const reducer = new LensReducer<State>()
                    .register<string>(UpdateNameAction, lStateTo.name)
                    .register<Date>(UpdateBirthdayAction, lStateTo.Birthday)
                    .toReducer();

const newState = reducer(oldState, new UpdateNameAction('George'));
```

Type-safety is maintained. The trick is to only make strongly-type methods public, and the weakly-typed `reducerFns: Reducer<TState, any>[]` private. The public method ensures that all input is in good shape; the weakly-typed field unifies `Action`s of different types.

This is trick I've found useful when dealing with similar scenarios - when there is a need to unify an ever-growing list of sub-types, encapsulation could work better than abusing union types.

But encapsulation is an OOP concept isn't it? In fact, keeping a weakly-typed list is no easy feat in functional languages like Haskell. This technique is only possible with cross-paradigm languages. 

And make no mistake: there are definitely plenty of OOP here: the builder pattern that encapsulates the weakly-typed functions, with a fluent interface. While it would have been possible to use functional alternatives, I feel OOP concepts works much better and are more practical.