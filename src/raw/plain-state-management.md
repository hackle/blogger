# Redux the pet peeve

For a long time I have been uncertain about the state-of-art of state management, particularly in client-side applications, and particularly regarding the popularity of `Redux` style architecture, for its 

* verbosity that results in code bloat
* unnecessary decoupling of logic that results in challenges to understanding what is going on

(With that said I've been very impressed with the awareness and prevalence of immutability and functional-style programming that such architectures were integral in creating).

When raised, historically my concerns were mostly brushed aside if not mocked, with arguments of varying level of rationality. However, to my great delight, lately I have been hearing similar thoughts from developers from different places and with various background. Thoughtworks even put `Redux` [back to "trial"](https://www.thoughtworks.com/radar/languages-and-frameworks/redux), not without a honourary mentioning of `Saga` which to be honest has been one of my pet peeves. The description is also spot-on. Well done Thoughtworks!

# the problem: managing side-effects

There are various emerging alternatives to `Redux/Saga`, some promising and others simply trying to abstract over or reorganise the same building blocks. (Just to be clear, being an advocate for functional programming, I am not particularly fond of any framework that uses an imperative/mutation-based state transition. It's backwards!)

To the very core, a lot of such frameworks / architectures are about managing side effect, which has been an age-old challenge and warranted many solutions or attempts. 

One of the problems with side effect is if managed poorly, it's disastrous for testing, especially integration testing. Dependency injection and mocking is a popular, albeit easily abused solution.

IoC containers / Dependency injectors does this by separating interfaces from concrete implementations where side-effects (think DB repository, API facade), which are kept in a container that has registration / injection capacities.

The famous [`Elm` architecture](https://guide.elm-lang.org/architecture/) does it differently and marked a milestone and passed [inspirations to the likes of `Redux/Saga`](https://redux.js.org/understanding/history-and-design/prior-art#elm). `Saga` help keeping side-effect in its own space/level (in the form of generator functions), making it easier to achieve purity for the components. However, any language without the benefits of a type system as sound as that of `Elm` can come across as somewhat lacking or less intuitive / idiomatic. For example, using `Action`s with unique `type` identifiers and different types of `payload` is no match to natively supported union types.

Coincidentally, I also came to learn that in `haskell` there have been a outburst of effect systems (which I never used in anger). The main takeaway is encoding side-effects as data types for the application domain to consume, and only interpret the result on the top level. Mind you, `Saga` kind of implements this data-interpretation idea, as generator functions need to be interpreted by the framework.

# plain and simple: the idea

This is a long-winded way of saying the idea of state management can be quite simple, even without any framework/library. Inspired by more conservative architecture of `haskell` applications, or in general, immutability-based functional programming, below is a high-level description of implementing this with `React`.

* elevate all side-effect operations (the "operations") in the form of functions, likely aggregated as an object, to the top level of the application. This object can be called `stateManager`, `effectFactory`, or more specifically `IOFactory` or anything to the content of your heart. 

* pass both the state and the `stateManager` all the way down to components that need state or, state manipulation that now becomes simple function calls, e.g. `stateManager.searchImages(keyword)` which might make an API call and use its response to create an updated state, which then triggers watching components to be updated.

Yes, it is that simple. The acute reader will scream "prop drilling!" at "pass ... all the way down". Luckily, the `React` folks have just the right fix for us, in the form of [`Context`](https://reactjs.org/docs/context.html). So the above statement can be revised as 
> "use a `Context` to pass the state and the operations to needy components".
(Mind you, this is not much different than how `Redux` does it).

# an implementation

It's quite straightforward to implement and [this one is by me](https://github.com/hackle/Lensta). A few key points below.

* how to pass the state and the operations from the top level. [example index.tsx](https://github.com/hackle/Lensta/blob/master/src/index.tsx)

```TypeScript
<StateProvider initialState={initialState} appIOFactory={appIOFactory}>
    <App />
  </StateProvider>
```

* how to connect a component. Note how `React.memo` is used to avoid unnecessary re-render. [example component](https://github.com/hackle/Lensta/blob/df6065adc2b05d7c5581028d98bfe27ddca401c5/src/SeparateIncrease.tsx)
```TypeScript
export default withState<OwnProps, StateProps>(
    React.memo(SeparateIncrease), 
    (state, appIO) => ({ increase: appIO.increase })
);
```

You'll see this implementation actually has a familiar syntax, that's because the idea is basically the same, but the implementation is arguably much, much simpler.

One thing that can be annoying, is updating a single field in a complex and deep state object. One might want to call back the tiny, verbose `reducer`s and all sorts of funny ways to combine them to a large reducer. However I have never been a fan of such convolution, for the boilerplate is easily made unnecessary with [lens](https://www.npmjs.com/package/tsminilens). A succinct side-effect operation as below. [full code](https://github.com/hackle/Lensta/blob/master/src/appIO.ts)

```TypeScript
// defining lenses
const lens = {
    counter: root.to('counter'),
    deep: root.to('a', 'very', 'deep', 'value')
};

// a side-effect function with API call
const increaseCounterAsync = async () => {
    const content = await fetch({
    'url': 'https://google.com',
    'mode': 'no-cors',
    } as Request);
    const text = await content.text();

    // look here! an updated state is set with a one-liner
    setState(prev => lens.counter.set(prev, text.length));
};
```

If you made it through to the end, you'll agree the idea should be fairly straightforward to implement in any language. All the merrier if you are lucky to find similar ways to avoid "prop drilling" as that of React's `Context`.