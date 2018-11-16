There must have been a million different opinions on unit testing, I won't be the least surprised if you have yours, and you should really think twice before you get a load of mine... and bear in mind that my opinion on unit testing has been an constantly evolving one. However, here it goes.

(I also notice this is getting way too long so it's possibly a good idea to fork this out to different posts. to-do.)

## testing through U.I. why not?

(Only) testing through U.I., a surprisingly popular practice - possibly also widely criticised for - and my thoughts? It's good, but not quite good enough.

### we should all be testing and any testing is good!

You can be one of the few privileged that work with dedicated testers, or be one of the many impoverished working with no testers and your boss is the product owner, quality assurance, marketing, sales, H.R. and accountant, there is one thing that applies to all of us in software and that is non-negotiable: we should all be testing! Either via refreshing the browser after every build, or debugging and pulling your hair hour for something mysteriously going wrong.

While you definitely shouldn't feel bad if you test everything through the browser or on real device or the simulator (in fact you should be proud that you are testing your product at all), be advised that that are times that testing through U.I. is not the most efficient way to validate your code / product.

### testing through U.I. when it makes sense

The times that testing through U.I. is absolutely necessary and is certainly good practice, is when the focus is on styling and user experience. There is no substation for putting yourself in the customers' shoes and going through the exact same journey as the customers.

### and when it is not enough

It would be terribly inefficient if one is working on tasks in business domain that is usually full of branching of logic. I trust that you know such concepts like [Cyclomatic complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity), you'd have learned that complexity of software grows exponentially. That is to say, every time you add only ``one`` innocent new condition to a domain with ``n`` existing conditions, chances are, you are adding extra ``n`` extra scenarios.

That is if you only change one component of your domain - if you change inter-dependent components, the complexity multiplies. Say one component has ``8`` scenarios that depend on another scenario with ``8`` scenarios, chances are that we are possibly dealing with ``64`` scenarios in total, now try test each scenario by refreshing your browser? (Believe me this is not wild imagination - I've dealt with domain ``much`` more complex than this).

Some typical problems with this approach can be:

* testing through U.I. can be unwieldy considering building the software, reload the application, and navigate to the specific functionality
* it gets tedious, time-consuming, and almost impossible to exhaust all possibilities as software becomes more and more complex
* it's not always reliable and deterministic as the environment can impact the process: connectivity, D.B. changes, user interaction, U.I glitches, anything can go wrong, etc. etc.

Enters unit testing - when the domain becomes complex in business logic, unit testing helps a lot in that

* done properly, it's deterministic and therefore offers high confidence
* it shorten the feedback cycle therefore it's more efficient
* it's much cheaper than manually testing therefore less time-consuming

## developers - not to be trusted :(

Don't be upset if you are a developer when you read this - I am also a developer and I completely stand by this statement.

Over-confidence is not a unusual symptom for a lot of developers, and when you hear a developer says something ``should`` work, or when they give an estimate, just smile and nod, or ask, "this is great! But how do we test this?"

I've been countless situations when I had absolute faith in my code without validating it, either through manual testing or unit testing, only to find out later that my presumptions were based on nothingness. What I wished I could have done, was to have verified my assumptions in a demonstrably way. What better ways to verify my code?

Manual testing, while it seems the obvious choice and the most intuitive way, suffers from being fragile and tedious to repeat.

Unit testing, on the other hand, is more formal and easier to repeat, and if done properly, serves as a persistent way to validate that code works as required.

> Unit testing forces developers to reason with their code

Blind faith rarely helps, hard cold unit tests serve us much better.

## it's more than a defence against tempering

A usual, but weak argument for unit testing, is that unit testing is necessary because "_what is somebody deleted this line of code?_"

Well, if that's all the purpose of unit testing, why don't we simply keep a copy of the code, and compare the present code against the copy that we keep? Certainly that is more efficient and accurate than manually written and much different code?

A stronger reason for unit testing, or any form of testing really, is to validate that our code really does what we want it to do. What best way to do that? By giving the code different inputs, and compare the outputs against expectations. One way to call these sets of input, output and expectations, is ``specification``.

Specifications matter a lot, and the stronger the better.

You may think by ``stronger`` I mean more stringent, and therefore more complex. That may be only half true. Stringent possibly, and complex mostly probably not. What's more likely, is that the stronger a specification is, the simpler it is.

A good example is ``idempotence`` - it's a simple concept, but I have not seen it usually validated. Do you have stringent testing against the PUT methods of your API endpoints? By ``stringent`` I mean, running hundreds if not thousands of test cases against the endpoints under test? (This might not be unit test now, but the idea holds).

## purity is key!

We'd be amiss if we talk about specification without mentioning purity, as purity, or ``pure function``, is defined [here](https://en.wikipedia.org/wiki/Pure_function):

> In computer programming, a pure function is a function that has the following properties:
> Its return value is the same for the same arguments (no variation with local static variables, non-local variables, mutable reference arguments or input streams from I/O devices).
> Its evaluation has no side effects (no mutation of local static variables, non-local variables, mutable reference arguments or I/O streams).

For pure functions, unit testing is as simple as giving different inputs and expecting corresponding output - just how we want it!

But mutation and statefulness dampens matters for us - does that ring a bell? Let me remind us some teaching or practices in the industry:

* I/O cannot be unit tested (possibly because they are not pure?)
* singletons / stateful services are hard to unit test (again possibly because they are not pure?)
* U.I. is hard to unit test

Also recommended to read about
* [referential transparency](https://en.wikipedia.org/wiki/Referential_transparency)
* [equational reasoning](http://www.haskellforall.com/2013/12/equational-reasoning.html)

## write more pure functions

I would consider this a given for any form of pure (or non-pure) functional programming - but in fact, even for any imperative languages this holds true. Purity makes code more reasonable. But you might be questioning, _HOW?_

How? If the language I use is object oriented, how do I write functions at all?

You might find this controversial, but I'd encourage you to write more ``static`` methods!

Why not? ``static`` methods are basically functions as they cannot rely on the state of any instance, and the class name virtually becomes a namespace; they cannot utilise such abused patterns as D.I., so mutating states becomes much harder (it's still very much possible of course), so most of the time, they are easier if they only rely on input to produce output, and therefore pure!

Read Mark Seemann's excellent post on [Dependency Rejection](http://blog.ploeh.dk/2017/02/02/dependency-rejection/)

About time we move away from D.I. and embrace purity!

## randomness is not your enemy

There are extremely powerful tools that help us generating test cases for testing our code (again not just for unit testing), such as [AutoFixture](https://github.com/AutoFixture/AutoFixture) which was started by Mark Seemann and I had the honour to contribute to, and [QuickCheck](http://hackage.haskell.org/package/QuickCheck), and [FsCheck](https://fscheck.github.io/FsCheck/), a F# port of QuickCheck.

An often-heard argument against use of such excellent tools, is that they make tests not deterministic.

Well if we consider _produce the same output for the same input_ as a characteristic of a pure function, then using random / generated input does not break that characteristic at all, on the contrary, it helps us to validate such characteristics.

The reason it upsets some of us could be that we are afraid that any failures cannot be reproduced and leaving us with mysterious errors - which of course is not a real problem as these tools give us details as what input is really given, and what kind of input results in failed tests.

If you are in the habit of testing, then be happy, don't be afraid, when you see a failed test. It means there is a chance to improve your code or your test, either way, your software benefits. Therefore embrace the above-mentioned tools - they help us to fail fast and fail loud.

## test driven design, really?

At one point in history, or should I say presently, there was / is belief that testing can drive good design. While this may seem true for some at a small scale, it may disappoint most of us most of the time.

I my opinion, there is no substation for architecture or vision, and if there is, unit testing is certainly not it. Any form or testing is but a way to validate that the architecture or design is correct or implemented properly, and nothing more.

While setting forth the specifications for a design in the forms of test cases, we are only demonstrating our best reasoning in our design, which might or might not be exhaustive. You'll have better luck with a language which has totality support, such as ``Idris``, but most probably not for lesser languages.

Quoted Dijkstra:

> Testing shows the presence, not the absence of bugs

Unit testing only goes as far as our best reasoning, and no further.

Its unquestionable value is in forcing us to ask ourselves - what if? how about? The value of testing lies right there - it forces us to think harder about our design, and by thinking harder, we usually end up making improvements.
