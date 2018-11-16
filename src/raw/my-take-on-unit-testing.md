There must have been a million different opinions on unit testing, I won't be the least surprised if you have yours, and you should really think twice before you get a load of mine... and bear in mind that my opinion on unit testing has been an constantly evolving one. However, here it goes.

## Bottom line - we should all be testing

You can be one of the few privileged that work with dedicated testers, or be one of the many impoverished working with no testers and your boss is the product owner, quality assurance, marketing, sales, H.R. and accountant, there is one thing that applies to all of us in software and that is non-negotiable: we should all be testing! Either via refreshing the browser after every build, or debugging and pulling your hair hour for something mysteriously going wrong.

## and any testing is good!

Well don't feel bad - we all do that one time or the other, and do feel good, you've been testing and that's absolutely good! (in that we are trying to make sure that things do work!)

## it's a good way to validate your U.I.

While I totally like that developers validate their code by refreshing the browser after every build, I do feel bad for them for being so terribly inefficient! And there is almost always a better way than validating one's code by refreshing the browser.

This might have been absolutely valid if one is working on U.I. heavy tasks, such as adjusting styling for pixel-perfectness - at which time tools like Chrome DevTools would be extremely helpful.

## but it's a bad way to validate your code

It would be terribly inefficient if one is working on tasks in business domain that is usually full of branching of logic. I trust that you know such concepts like [Cyclomatic complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity), you'd have learned that complexity of software grows exponentially. That is to say, every time you add only ``one`` innocent new condition to a domain with ``n`` existing conditions, chances are, you are adding extra ``n`` new scenarios.

That is if you only change one component of your domain - if you change inter-dependent components, the complexity multiplies, and try test each one scenario by refreshing your browser?

## Quick feedback

So while validating your code through the browser / simulator is for the good purpose, it's hardly the most efficient thing to do. Why? Because it's awfully slow and therefore inefficient.

Believe it or not - in software or in anything else, fast feedback is important. We try everything to shorten the feedback loop. Good examples are user testing for U.I. design.
