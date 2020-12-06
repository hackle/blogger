For a football application, there is `const int totalGameMinutes = 90`. Where do you put it? In a `Constant` file? Or in the `Game` file?

Let's say if you put it in the `Constant` file, it's placement by technical concern, where as `Game` is by functionality.

If we stick to one or the other, the effect of placement can turn out to be a pretty far-reaching.

Here is my recommendation: prefer placement by functionality whenever possible.

## volatile vs stable

Let's say, for any piece of code, there can be roughly two types of concerns.

- Technical concerns: interfaces, classes, enums, utilities, controller, views, models, constants, enums,  etc etc.
- Domain concern: the problem to solve, the subject at hand

The problem with **technical concerns** is they are subjective. Whether an interface, an enum or a const (not to mention space vs tab) is used is largely up to the programmer, which means more often than not, another programmer might not fully agree on some of the choices. 

For example, I don't use interfaces quite as much as most of my peers; and they might not like my pervasive use of functions.

**Functionality**, on the other hand, is much less subjective . Something like "display up-to-update scores of football games" will be more or less agreed upon before code is written, especially when the problem at hand (as well as the solution) is worked out beforehand, typically with the collaboration of domain experts.

So out of two options - implementation that is volatile, and functionality that is stable, it makes more sense to place and organise code by the latter, to mitigate volatility.

To better illustrate the differences, let's use an example and follow each path to see where they lead us.

## placement by technical concerns

Still for a football related application, if we organise code by technical concerns, below is a typical (but over-simplified) file structure. Assumed we use `C#`.

```
|- Constants
    |- Game.cs
    |- Substitution.cs
|- Interfaces
    |- Game
        |- IGame.cs
        |- IGameResult.cs
    |- Substitution
        |- ISubstitution.cs
        |- ISubRequest.cs
|- Implementations
    |- Game
        |- Game.cs
        |- GameResult.cs
    |- Substitution
        |- Substitution.cs
        |- SubRequest.cs
```

Some say grouping files under `Constants`, `Interfaces` and `Implementations` looks neat, but my very first observation is for any functionality, code is scattered around. To fully understand how `Substitution` works, I will have to jump in and out of 3 folders.

A closer look reveals the similar file names are repeated across 3 folders (in real "enterprise" code-bases there would be many more). Vigilance is required to keep them in sync. It's laborious manual work - IDEs normally don't help here. As the code-base becomes more and more complex, keeping such vigilance becomes more and more taxing; people can become lax over time, or they simply give up (I would).

Subsequently, to make changes to any functionality, the footprint is also spread out. Or if I want to delete a feature, or port `Substitution` to another code-base, I need to copy/remove a few files one by one.

### placement by functionality

How often do we change ALL `interfaces` or ALL `constants` at once? I bet not very often. 

Instead, when the rules of **substitution** change, we usually need to change something related, be it an `interface`, a `constant` or `implementation`. Naturally, it makes life easier for the programmer to keep these things together.

You must have guessed, it's just the cliche: **things change together belong together**.

This leads us to the below file structure.

```
|- Game
    |- IGame.cs
    |- IGameResult.cs
    |- Game.cs
    |- GameResult.cs
|- Substitution
    |- Substitution.cs
    |- ISubstitution.cs
    |- ISubRequest.cs
    |- SubRequest.cs
```

Things related by functionality mostly stay together - the structure is not just flatter, but also much cleaner and navigable. 

If I want to port anything `Game` related to another code-base, it's a matter of copy-pasting the containing folder. It doesn't get much easier.

You would have noticed there are no `constant` files any more, and you might not like the placement of the interfaces. Fairly so - please see the recommendations below.

## Recommendations

It's a simple idea and I know you got it by now. But let's see how far the list goes, just to make sure we don't leave any blind-spots. Below are my (opinionated) recommendations by example.

### constants
No `constant` file/class/module please. I keep them inline to where they are used - within a function, method or class. 

If a constant is reused, them elevate it to a more central location.

### interfaces/implementations folders
No `interfaces` folder please. An interface should be alongside its consumer if we stick to the mantra "consumers own interfaces". Or, if interfaces are used for dependency injection only, simply keep them alongside the implementation. 

Similarly, if an interface is shared across functionality, elevate it to a common location.

### interfaces module/projects
Having a dedicated project for interfaces is usually done for dependency injection. Alternatively I've seen `IoC` projects created for this purpose. It is also done when developers are stuck managing project dependencies. This is a sign that poor dependencies management is spoiling architecture. Don't go there.

### models / views / controllers folders
The frameworks are at fault here - way to lead people down a path of no return. Come on! It should be fairly straightforward to discover controllers without a specific / conventional folder name. The MVC pattern doesn't mean there should be M.V.C. folders literally. I feel the more modern front-end frameworks are getting this right.

### model / core project
Lumping all models to a `model`, `core` or `domain` project is also advised against. With the exception of `core` if it's indeed reused across functionality.

### test folder / project
Are we serious about **tests as documentation**? Or testing as first-class engineering concern? Then it makes sense to keep tests alongside functionality. I am under the impression the front-end frameworks are championing this with `foo.spec.js` files alongside `foo.js`. The conventional way of keeping separate **test** projects or folders does not help the cause here.

### actions / reducers
Typically, such folders appear for front-end projects with the use of `redux store`. Grouping `actions` and `reducers` can be necessary as action types are used for case split as with 
```
switch (action.type) { 
    case 'START_LOADING_IMAGES': ... update state
    ... cases for more action types
}
```
The recommendation here is to avoid a global one-for-all `actions` file, instead try to keep smaller `actions` files, each for specific functionality.
