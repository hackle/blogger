There must have been a million different opinions on unit testing, I won't be the least surprised if you have yours, and you should really think twice before you get a load of mine... and bear in mind that my opinion on unit testing has been an constantly evolving one. However, here it goes.

## we should all be testing and any testing is good!

You can be one of the few privileged that work with dedicated testers, or be one of the many impoverished working with no testers and your boss is the product owner, quality assurance, marketing, sales, H.R. and accountant, there is one thing that applies to all of us in software and that is non-negotiable: we should all be testing! Either via refreshing the browser after every build, or debugging and pulling your hair hour for something mysteriously going wrong.

While you definitely shouldn't feel bad if you test everything through the browser of the simulator (in fact you should be proud that you are testing your product at all), be advised that that are times that testing through U.I. is not the most efficient way to validate your code / product.

## testing through U.I. when it makes sense

The times that testing through U.I. is absolutely necessary and is certainly good practice, is when the focus is on styling and user experience. There is no substation for putting yourself in the customers' shoes and going through the exact same journey that the customers are going through.

## and when it goes wrong

It would be terribly inefficient if one is working on tasks in business domain that is usually full of branching of logic. I trust that you know such concepts like [Cyclomatic complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity), you'd have learned that complexity of software grows exponentially. That is to say, every time you add only ``one`` innocent new condition to a domain with ``n`` existing conditions, chances are, you are adding extra ``n`` new scenarios.

That is if you only change one component of your domain - if you change inter-dependent components, the complexity multiplies. Say one component has ``8`` scenarios that depend on another scenario with ``8`` scenarios, chances are that we are possibly dealing with ``64`` scenarios in total, now try test each scenario by refreshing your browser? (Believe me this is not wild imagination - I've dealt with domain ``much`` more complex than this).

Some typical problems with this approach can be:

* testing through U.I. can be unwieldy considering building the software, reload the application, and navigate to the specific functionality
* it gets tedious, time-consuming, and almost impossible to exhaust all possibilities as software becomes more and more complex

## Quick feedback

So while validating your code through the browser / simulator is for the good purpose, it's hardly the most efficient thing to do. Why? Because it's awfully slow and therefore inefficient.

Believe it or not - in software or in anything else, fast feedback is important. We try everything to shorten the feedback loop. Good examples are user testing for U.I. design.
