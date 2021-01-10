For a long time I have been uncertain about the state-of-art of state management, particularly in client-side applications, and particularly regarding the popularity of `redux` style architecture, for its 
* verbosity that results in code bloat
* unnecessary decoupling of logic that results in challenges to understanding what is going on

(With that said I've been very impressed with the awareness and prevalence of immutability and functional-style programming that such architectures were integral in creating).

When raised, historically my concerns were mostly brushed aside if not mocked at, with arguments of varying level of rationality. However, to my great delight, lately I have been hearing similar thoughts from developers from different places and with various background. Thoughtworks even put `redux` back to "trial", with a honourary mentioning of `sagas` which to be honest has been one of my pet peeves :)

There are various emerging alternatives to `redux/sagas`, some promising and others simply trying to abstract over or reorganise the building block. (Just to be clear, being an advocate for functional programming, I am not particularly fond of any framework that uses a mutation-based state transition. It's backwards!)

To the very core, a lot of such frameworks / architectures are about managing side effect, which has been an age-old challenge and warranted many solutions or attempts.
To name one, popular IoC containers / Dependency injectors is a form of managing side effects to a certain extent, by separating interfaces from implementations, and keeping concrete implementations in a container that has registration / injection capacities.

The famous [`Elm` architecture](https://guide.elm-lang.org/architecture/) marked a milestone and passed [inspirations to the likes of `Redux/Sagas`](https://redux.js.org/understanding/history-and-design/prior-art#elm). `Sagas` help keeping side-effect in its own space or level (in the form of generator functions), making it easier to achieve purity for the components.

Coincidentally, I also came to learn in `haskell` there have been a outburst of effect systems (which I never used in anger). The main takeaway is encoding side-effects as data types for the application domain to consume, and only interpret the result on the top level.

This is a long-winded way of saying the idea of state management can be quite simple, even without any framework/library. Inspired by more conservative architecture of `haskell` applications, or in general, immutability-based functional programming, below is a high-level description of implementing this with `React`.

* elevate all side-effect operations (the "operations") in the form of functions, likely aggregated as an object, to the top level of the application. This object can be called `stateManager`, `effectFactory`, or more specifically `IOFactory` or anything to the content of your heart. 

* pass both the state and the `stateManager` all the way down to components that need state or, state manipulation that now becomes simple function calls, e.g. `stateManager.searchImages(keyword)` which might make an API call and use its response to create an updated state, which then triggers watching components to be updated.

Yes, it is that simple. The acute reader will scream "prop drilling!" at "pass ... all the way down". Luckily, the `React` folks have just the right fix for us, in the form of [`Context`](https://reactjs.org/docs/context.html). So the above statement can be revised as 
> "use a `Context` to pass the state and the operations to needy components".
(Mind you, this is not much different than how `Redux` does it).

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

One thing that can be annoying, is updating a single field in a complex and deep state object. One might want to call back the tiny, verbose `reducer`s and all sorts of funny ways to combine them to a large reducer. However I have never been a fan of such convolution, for the boilerplate is easily made unnecessary with [lens](https://www.npmjs.com/package/tsminilens). A succinct side-effect operation as below to close this post. [full code](https://github.com/hackle/Lensta/blob/master/src/appIO.ts)

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