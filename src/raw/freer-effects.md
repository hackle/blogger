One obstacle of programming in Haskell, especially in production, is handling Effects. Effects are things that need special interpretation, usually in the form of monads, such as IO, State, Reader and Writer.

Effects is a big deal because types are made explicit in Haskell. When needing to mix effects, we easily end up with nested types like `IO (Reader(State String)) String`. This is traditionally handled with monad transformers, which requires getting into and out of each layer of monads. This is not the end of the world but does require repetitous code to enable each combination. For example, from the `mtl` library, a definition is found as [follows](https://hackage.haskell.org/package/mtl-2.2.2/docs/src/Control.Monad.State.Class.html#line-163),

```Haskell
instance MonadState s m => MonadState s (ReaderT r m) where
    get = lift get
    put = lift . put
    state = lift . state
```

But [conversely also](https://hackage.haskell.org/package/mtl-2.2.2/docs/src/Control.Monad.Reader.Class.html#line-161),

```Haskell
instance MonadReader r m => MonadReader r (Lazy.StateT s m) where
    ask   = lift ask
    local = Lazy.mapStateT . local
    reader = lift . reader
```

You get the idea. To be honest I never used this in anger but there will be need to nest more effects in a decently complex application, and the amount of boilerplate that follows. Composition is quite rigid, and if we use IO, it's not easily testable. So I understand some people will find this distasteful.

One of the alternatives to this was something called "Freer Monads", an idea by Oleg Kiselyov et al. And one popular implementation is the `Freer simple` package which I'll use for examples.

The big idea of `Freer Monads` is the effects are not evaluated immediately, but first encoded in an abstract syntax tree (which `Free Monad` is good at) that ensures purity. The syntax tree was only evaluated when effects are needed, for example when I/O happens. 

It also tackles the composition problem with some fancy type magic which I prefess no comprehension of, but we'll see it in action.

(For a good understanding you'll want to read this [excellent article](http://okmij.org/ftp/Computation/free-monad.html)).

OK, let's now see a few examples.

## pass environment with `Reader`

A pretty typical example (from my limited experience) is passing environment values (such as configuration, commandline arguments) downstreams as a `Reader`.

## add logging with `Writer`

## create an `Identity` effect

## where does it belong?

It would seem now effects are all pure and composable, sure we should be using it throughout our code? Not in my opinion. I would still try to keep my code in less fancy types, and only use the likes of `Freer` when absolutely necessary.

A good example is with logging. One would be tempted to put logging in any function. This would then turn a simple funciton like `string -> string` into something like `Eff [ Writer ] string -> Eff [ Writer ] string`, and make them inpenetratable to anyone who is not savvy to `Freer`, and possibly annoying even to thoese who are.

I can see the `Eff` type useful right below the `main` and I/O functions, but above the domain / business layer where everything is simple. Typically, it is used to integrate components on a relatively high level.