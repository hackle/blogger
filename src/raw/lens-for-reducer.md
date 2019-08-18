Once having got used to the idea, it's not hard to see that any `lens` implementation can help reduce boilerplate in dealing with complex data structure, for example, with so-called "reducer" functions in Redux. Let's see how we can utilise something as simple as `tsMiniLens` to build a minimalistic replacement for the usual reducers. 

This replacement would look as follows:

```typescript
const reducer = new LensReducer<State>()
    .register(UpdateBirthdayAction, lStateTo.birthday)
    .register(UpdateNameAction, lStateTo.name)
    .toReducer();
```

Let's start with a "usual" way of setting up a reducer.

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

I say "usual" because implementations vary, there are different styles with much more ceremony, such as union `Action` types, enum as tags for `Action` etc. I picked the above for its lack of ceremony (largely due to representing `Action` as class and use of `instanceof`) or distraction from the topic at hand.

While I can't say I am a big fan of the whole Redux fetish, if I have to use it then I wouldn't mind writing a `reducer` as the one above.

There is but one small annoyance, the way the `state` value is being updated is quite tedious, with code blocks like `{ ...state, birthday: action.payload }` or `{ ...state, name: action.payload }`. It can get pretty messy if the data structure at hand is complex.

Worry not - `lens` excels at eliminating such repetitions. First, let's define a few lenses for `State`. (You may want to click [here](lens-typescript) for a quick recap of `tsMiniLens`.)

```typescript
const lStateTo = {
    name: lensFrom<State>().to('name'),
    birthday: lensFrom<State>().to('birthday')
};
```

**lFooTo.bar** is a bit of hungarian notation (that you don't need to follow) I use to wrap lenses for an object; it simply means "lens from Foo to bar". 

With the above we can do `lStateTo.name.view(state)` or `lStateTo.name.set(state, 'Jack')`. Meanwhile, the `reducer` function becomes:

```typescript
function reducer(state: State, action: ActionUnion): State {
    if (action instanceof UpdateBirthdayAction)
        return lStateTo.birthday.set(state, action.payload);
    
    if (action instanceof UpdateNameAction)
        return lStateTo.name.set(state, action.payload);
    
    return state;
}
```

If this seems not much difference it is only because the structure of `State` is simple - but we are on the right track.

Notably, the repetition in `reducer` is more pronounced now - with every `instanceof` it tries to recover the type of `action` so the `payload` can be accessed by following code.
(Read about [instanceof type guard](https://www.typescriptlang.org/docs/handbook/advanced-types.html#instanceof-type-guards))

Such repetition is something we can abstract over. One option is to use something like a **builder pattern** to build up a reducer with one `Action` type at a time.

With the idea in place, we need only add a few helper types to get underway.

```typescript
interface IAction<TPayload> { payload?: TPayload }

type TypeOf<T> = {
    new(...args): T;
}

type Reducer<TState, TAction> = (st: TState, action: TAction) => TState;
```

Worth noting that `TypeOf<T>` represents a constructor - it kind of works like a `where T : class` constraint in `C#` to generalise over classes, and it's usually seen when a type is passed along, for example, `foo<T>(ctor: TypeOf<T>)` can be invoked as `foo<UpdateBirthdayAction>(UpdateBirthdayAction)` or simply `foo(UpdateBirthdayAction)`. Remember the type parameters are erased at compile time by `TypeScript`, this allows us to pass the "type" along as value and it remains present at runtime.

At last we are ready for `LensReducer` with which we can register `Action`s with corresponding lenses, and make a `reducer` function.

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

All the type annotations and lambdas can be a bit overwhelming but let's take a deep breath and dive in:

1. Method `ReducerBuilder.register()` takes a type parameter that is an `Action`'s *payload*, for example, `TPayload`. 
2. This type parameter in turn restricts `ty`, the first parameter of `register()`, to an `Action` with `TPayload` as payload, 
3. it also restricts the second parameter `len`, to type `Lens<State, string>` that focuses on a `string` field within a `State` data structure. 

For example: `register<string>(UpdateNameAction, lStateTo.name)`. By specifying `string`, it's required that

* `UpdateNameAction` must have a `payload` of `string`
* `lStateTo.name` must be a lens that manipulates a `string` field within a data structure of type `State` (which is encoded in the type of `ReducerBuilder`). 

Kind of nice that so much follows by specifying `State` and `string` as type parameters to `register()`, isn't it?

In fact, to invoke `register()` we don't need to specify `TPayload` at all, as it will be inferred from `IAction`. Let's see it in action (pun is coincidental). First of all the `Action` classes now need to implement IAction<T>.

```typescript
class UpdateBirthdayAction implements IAction<Date> {
    constructor(public payload: Date){};
}

class UpdateNameAction implements IAction<string> {
    constructor(public payload: string){};
}
```

And behold - this is how we get our good-old `reducer` back.

```typescript
const reducer = new LensReducer<State>()
                    .register(UpdateNameAction, lStateTo.name)
                    .register(UpdateBirthdayAction, lStateTo.Birthday)
                    .toReducer();
```

Type safety, what we value, is maintained, how? Well, by making strongly-type methods public, and the weakly-typed `reducerFns: Reducer<TState, any>[]` private. The public method ensures that all input is in good and valid shape; the weakly-typed field unifies `Action`s of different types.

This is a trick I've found useful when dealing with similar scenarios - when there is a need to unify an ever-growing list of sub-types, encapsulation could work better than abusing union types.

But encapsulation is an OOP concept isn't it? Very true. On the other hand, keeping a weakly-typed list is no easy feat in functional languages like Haskell.

And make no mistake: there are definitely plenty of OOP here: the builder pattern that encapsulates the weakly-typed functions, with a fluent interface. While it would have been possible to use functional alternatives, I feel OOP concepts work much better and are more practical. Nothing wrong combining paradigms - in fact it's only reasonable that we do when it makes sense.

You may want to improve on this solution for more flexibility - for example, could `register` also take a `Reduce` function as its second parameter? This will change its type to `register<TPayload>(ty: TypeOf<IAction<TPayload>>, lensOrReducer: Lens<TState, TPayload> | Reducer<TState, TPayload>)` and we can write `register(UpdateNameAction, (st, name) => ...)`. If you've come this far, sure you'll enjoy this little exercise.