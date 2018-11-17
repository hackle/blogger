Make no mistake - any form of testing is better than no testing at all. However, for professional software development, some forms of testing are more efficient than others for certain scenarios. I'll name some common practices in testing and share a few of my thoughts.

You'll find my view is heavily biased towards functional programming - the very unpredictable me :-)

## developers are not to be trusted for their code

Don't be upset by this - I am a developer and I don't trust myself for my own code. Like many other developers, I am an optimist when it comes to my code. But also like many others, I had to eat my own words time and time again as a result of over-confidence.

Unit testing is what saves me blushes as it forces me to reason with my code. Instead of relying on blind faith, I find myself asking "does it really work? have I prove it?". I check that's indeed the case, before the work is handed to the next person.

Experiences tell me that it's much cheaper to write a few unit tests before I call something "done" and hand it off, than having the work rejected later by Quality Assurance, product management or much worse end-users - in fact, the further it goes, the longer the delay, the worse the damage and the more expensive it becomes.

## testing everything through U.I. may be fine, but too expensive

First of all, there is nothing wrong with the practice of testing through user interface, the question is rather, when and when not?

For example if the task at hand is heavy in styling, then it's only natural to bring up the U.I. It's visual, and therefore most intuitive.

However if one is dealing with tasks rich in branching of logic, as complexity grows exponentially, testing through U.I. quickly becomes inefficient. One would need to navigate the whole application (if there is an application at all) to get to the code under test. The feedback cycle is typically long.

Unit testing offers a much shorter feedback cycle to validate the algorithms in our code. It's also much cheaper, therefore we can afford to write more of them.

At the right time we still need to test through U.I. as it's an integral part of any product, but only when we are working on the U.I. itself.

So my advice is if you are a professional developer, and are currently testing most of your code through U.I. then try to suppress that urge, and find another method with a shorter feedback cycle, for example, unit testing.

## it's more than a defence against tempering

A usual, but weak argument for unit testing, is that unit testing is necessary because "_what if somebody deleted this line of code?_" So, a test must be written to make sure this line of code stays.

This may frighten people into agreement and action, but if that's all there is to unit testing, why don't we simply keep a copy of the code, and compare the present code against the copy that we keep? Certainly that is more efficient and accurate than manually written and much different code?

A stronger reason for unit testing, or any form of testing really, is to validate that our code really does what we want it to do.

What better way to do that than by running the code with different inputs, and comparing the outputs against expectations? Such input, output and expectations, can be called ``specifications``.

Specifications matter a lot, and the stronger the better.

You may think by ``stronger`` I mean more stringent, and therefore more complex. That may be only half true. Stringent maybe, but complex mostly probably not. What's more likely is that the stronger a specification is, the simpler it will be.

A good example is ``idempotence`` - it's a simple concept, but also a very strong and very useful ``specification``. Unfortunately, I have not seen it usually validated. Do you have stringent testing against the PUT methods of your API endpoints? By running hundreds if not thousands of test cases against the endpoints under test to make sure it's ``idempotent``? (This can be beyond the scope of unit testing, but the idea holds).

## purity is key!

We'd be amiss if we talk about specification without mentioning purity, or ``pure function``, which is defined [here](https://en.wikipedia.org/wiki/Pure_function):

> In computer programming, a pure function is a function that has the following properties:
> Its return value is the same for the same arguments (no variation with local static variables, non-local variables, mutable reference arguments or input streams from I/O devices).
> Its evaluation has no side effects (no mutation of local static variables, non-local variables, mutable reference arguments or I/O streams).

If a function is pure, unit testing is as simple as giving input and expecting corresponding output, as the noises of I/O and mutation are kept out.

I/O can be either out of our control, or hard / tedious to set up. Mutation often makes our code less reasonable.

This does also remind me of some teaching or practices in the industry:

* I/O rich code cannot be easily unit tested - possibly because they are not pure
* singletons / stateful services are hard to unit test- not pure
* U.I. is hard to unit test - not pure either

Also recommended to read about
* [referential transparency](https://en.wikipedia.org/wiki/Referential_transparency)
* [equational reasoning](http://www.haskellforall.com/2013/12/equational-reasoning.html)

## how to write more pure functions? static method to the rescue!

I would consider this a given for any form of pure (or non-pure) functional programming - but believe me the benefit of pure functions hold for any programming languages as purity makes code more reasonable. Now you might be questioning, _BUT HOW?_

For example, I use C# a lot, which is object oriented and the concept of function is not even valid in idiomatic C# code - how do I write ``functions`` to start with? Let alone pure functions?

The answer is simple - just write ``static`` methods!

``static`` methods are basically functions as they cannot rely on the state of any instance, and the class name virtually becomes a namespace.

For static methods, mutating states becomes much harder (although still very much possible). More likely, they just take input and produce output, thus they are more likely than instance methods to be pure.

I/O and state mutation are still essential. But to make our code more reasonable, it's a good idea to build complex logic in a pure fashion, and bring them together when I/O is needed.

A good pattern is to integrate I/O only at high level, and keep code at this level as simple as possible, so there would be no need to unit test it, and not to unit test it! Remember such code would be hard to unit test anyway. However, now other forms of testing (end-to-end tests, for example) become necessary to validate the I/O does take place as expected.

One of my all-time favorite quotes by **C. A. R. (Tony) Hoare**:

> "There are two ways of constructing a software design: One way is to make it so simple that there are obviously no deficiencies, and the other way is to make it so complicated that there are no obvious deficiencies. The first method is far more difficult."

Once you go down this path (the ``first method``), soon you'll find less and less mocking is needed for your unit tests, as less and less interfaces are created, which inevitably means you have less and less code. Not surprisingly, you'll be using less and less Dependency Injection. While your code becomes easier to test and therefore more thoroughly validated.

Also read Mark Seemann's excellent post on [Dependency Rejection](http://blog.ploeh.dk/2017/02/02/dependency-rejection/)

## what does a good unit test look like?

If one has success at writing pure functions, then unit testing becomes fairly simple, and most probably data-driven. Simple because there is no longer much need to set up all sorts of mocks. Data-driven because the output of pure functions depend only on the input.

Now the main task of unit testing consists in finding meaningful and representative (if not exhaustive) sets of input and output.

## test first or later?

If tests are written first, they are more like specifications - we set forth the requirements before any production code is written, and then we write code to satisfy such specs.

I like this style very much as it forces me to think ahead and think in a structured way.

Indeed, a lot of software's problems come from jumping right into coding without a clear plan first.

With this said, it's not a bad thing to write tests later - after all, any form of testing is useful.

## randomness is not your enemy

We talked about finding meaningful and exhaustive sets of input and output, which is easier said than done. Despair not, there are extremely powerful tools that help us generating test cases for testing our code (again not just for unit testing), such as [AutoFixture](https://github.com/AutoFixture/AutoFixture) which was started by Mark Seemann and I had the honour to contribute to, and [QuickCheck](http://hackage.haskell.org/package/QuickCheck), and [FsCheck](https://fscheck.github.io/FsCheck/), a F# port of QuickCheck.

The difference being, AutoFixture will try to generate "good enough" test data, while QuickCheck / FsCheck will actually try to break a test by generating (often scarily) representative and edge-case test data.

To give an idea, for a `string` parameter, QuickQuick may start with `null`, empty string, whitespaces, line-breaks, etc.

An often-heard argument against use of such excellent tools, is that they make tests not deterministic.

If we consider _producing the same output for the same input_ as the essence of [determinism](https://en.wikipedia.org/wiki/Deterministic_algorithm), then using random / generated input does not break that, what's deterministic remains deterministic - on the contrary, such tools helps us to validate such characteristics by giving input that we would otherwise struggle to come up with.

Also if you are in the habit of testing, then be happy, don't be afraid, when you see a failed test. It means there is a chance to improve your code or your test, either way, your software benefits. Therefore embrace the such tools as AutoFixture and QuickCheck - they help us to fail fast, and fail loud.
