Types are contracts. No contracts is worse than a vague contract. The TypeScript handbook has a [good example here](https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html#optional-parameters-in-callbacks).

> ❌ Don’t use optional parameters in callbacks unless you really mean it:

```TypeScript
/* WRONG */
interface Fetcher {
  getObject(done: (data: any, elapsedTime?: number) => void): void;
}
```

And the explanation,

> This has a very specific meaning: the done callback might be invoked with 1 argument or might be invoked with 2 arguments. The author probably intended to say that the callback might not care about the elapsedTime parameter, but there’s no need to make the parameter optional to accomplish this — it’s always legal to provide a callback that accepts fewer arguments.

A typical example is for front-end code, callbacks such as `onClick` can be wired up as (in React DOM) `<button onClick={ () => console.log("Hello!") }>` although the type of `onClick` is `(event: MouseEvent<T, globalThis.MouseEvent>) => void`. This is let go by TypeScript even with all "strict" compiler options turned on. 

Terrible! This never happens in a "proper" language like C#, one might say. An example of TypeScript - if not everything front-end - being sloppy!

Feigned frustration (if not also sarcasm) aside, there are actually solid reasons behind. Let's take it from the top with a harmless example.

But first thing first, let me introduce the parties involved, they are,

- the "Users" who will use my code,
- and I am the "Author" who writes code for the Users to use

# A text-book `greet`

As the Author I write this function,

```TypeScript
function greet(name: string): string {
    return `Hello ${name}`;
}
```

Seeing the type `greet(name: string): string`, the Users can happily call `console.log(greet('George'))`. No surprise.

# A real-life `greet`

Real-life Users may have (they always do) different requirements for making a greeting. Due to network latency (and maybe their excellent use of micro-services), building a greeting message can take indeterminate amount of time. As a friendly (and money-loving) Author I find that reasonable and am happy to oblige.

```TypeScript
function greetReal(name: string, whenReady: (message: string) => void): void {
    // much fuss to build a message
    const message = `Hello ${name}`;

    whenReady(message);
}
```

Seeing the new type `greetReal(name: string, whenReady: (message: string) => void): void`, the JavaScript-tested Users should have no problem calling it with `greetReal('George', message => console.log(message))`. Given the same `name`, they will get the same greeting printed on the `console`. The difference being, instead of returning the `message` directly, the new version uses a call-back to hand over the `message`.

Now we can look at the recommendation from the handbook - all it says is, as the **Author** of `greetReal`, I should know VERY WELL there is a `message` to give back, and it's confusing and therefore bad design to use an optional `message?`. 

In code it's possibly clearer than in word.

```TypeScript
function greetReal(name: string, whenReady: (message?: string) => void): void {
    const message = `Hello ${name}`;

    // by now I know for certain that I have a message, 
    // and it's not cool to say "I MAY have a message for you..."
    whenReady(message);
}
```

One will argue - certainly there are times the Author is unsure if there is a message or not, for example, a greeting cannot be made if a dependent service fails.

```
const greeting: string | undefined = getGreetingFromService();

// fails to get greeting
if (greeting == null) {
    whenReady();
}

const message = `${greeting} ${name}`;
whenReady(message);
```

This might be reasonable, but I would counter it should be better designed and handled with another call-back, such as 

```TypeScript
function greetReal(
    name: string, 
    whenReady: (message: string) => void, 
    whenFailed: () => void
): void
```

A good reminder of [the likes of `Promise`](http://definitelytyped.org/docs/es6-promises--es6-promises/classes/promise.html), which has `constructor(callback: (resolve: (result: R) => void, reject: (error: any) => void) => void): Promise`.

What about the original `greet`? If we try to express a failure case with the return type, it becomes `greet(name: string): string | undefined`. The Users can guess correctly that `undefined` indicates failure and handle it accordingly, however, I personally find the callback style expresses the intent of the Author more clearly.

While passing functions / call-backs is not everyone's cup of tea, it does pose the question of expressivity, which I tried to explain in a previous post [Don't pattern match, just continue!](dont-pattern-match-just-pass-function). (We'll agree case-splitting on `string | undefined` is a form of pattern matching).

# Leaving out `name`

Let's get back to the TypeScript handbook, which says

> it’s always legal to provide a callback that accepts fewer arguments

How is this possible? Let's put it in code. 

To recap, in order to call `greet(name: string): string` the Users must provide a `name`, which is required, as in `greet('George')`.

On the other hand, to call `greetReal(name: string, whenReady: (message: string) => void): void`, the Handbook says it's "always legal" to write `greetReal('George', () => console.log("..."))`. Why? 

One way to look at it, is through the scenario when `message` is not used in the call-back, then there should be no need to include it as a parameter. Below we have two calls of `greetReal` that are effectively the same.

```TypeScript
// message is not used at all
greetReal('George', (message: string) => console.log("I don't care"));

// then why not leave it out?
greetReal('George', () => console.log("I don't care"));
```

The `message` is thrown away, a bit wasteful, but no harm is done. 

You'll notice the same reasoning should apply to any language, however, some are more strict and require us to type out `(message: string)` even when it's never used.

# Contra-variance

Some readers must have noticed `message` is contra-variant to the call-back `(message: string) => void`, or it's in a *negative* position. This means to call `greetReal`, the Users can vary the type of `message` to be a more general type than `string`. 

As it's hard to find a good super type for `string`, let's change the example up a little to use the typical `Tiger` which is a sub-type of `Animal`.

```TypeScript
interface Animal { name: string };
interface Tiger extends Animal { kind: 'cat' };

function greetTiger(name: string, whenReady: (tiger: Tiger) => void): void {
    const tiger: Tiger = { name, kind: 'cat' };
    whenReady(tiger);
}
```

Being contra-variant means it's possible to write `greetTiger('Howler', (tiger: Animal) => console.log(tiger.name))`. This may look strange at first, but all we are doing here is calling a `Tiger` an `Animal`, no harm done.

How does this relate to `greetTiger('Howler', () => console.log('do not care'))`? 

Well, they can be related if we are willing to take a stretch: any parameter that's not used at all can be considered of type `IDontCare`, which is the super type of everything. As `IDontCare` is a super type of `Animal`, just like `Animal` is a super type of `Tiger`,  we are free to write `greetTiger('Howler', (tiger: IDontCare) => console.log('do not care'));`, which by definition is equivalent to `greetTiger('Howler', () => console.log('do not care'));`.

# Who calls?

Yet another angle is looking at who calls a function, the User or the Author?

When the Author writes `greet(name: string): void`, it's for the Users to call `greet` by providing `name`. Plain and simple.

Now the Author writes `function greetReal(name: string, whenReady: (message: string) => void): void`, again the Users call `greetReal` with a `name`, but they must also provide `whenReady`, which is a function that will be called by - you guessed it - the Author. It's the other way around!

Here is the deal: when PROVIDING values for parameters to call a function, it's always safe to provide values of MORE STRICT types, such as `Tiger` for `Animal`.

On the other hand, when RECEIVING values for parameters, it's always safe to be loose, as in `(tiger: Animal) => void` for `(tiger: Tiger) => void`. Note the Users are on the RECEIVING end, as the `tiger` value will be PROVIDED by the Author. Let that sink in, it's key!

This constraint may look like a minder bender, but it's actually key to the design of good software, and it's famously phrased as [Postel's law](https://en.wikipedia.org/wiki/Robustness_principle).

> be conservative in what you do, be liberal in what you accept from others

(Notice the similarity to "be STRICT in what you PROVIDE, be LOOSE in what you RECEIVE"?)

If you've made it this far, then it should come as no surprise this law is also stated as,

> be contravariant in the input type and covariant in the output type

Although words as "input" and "output" can be a bit confusing when functions are involved.

And there is a good reason the "Users" are not called the "Callers", as when it comes to functions, the table can turn pretty quickly!

