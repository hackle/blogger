These are the slides for a talk I do as introduction to "pragmatic unit testing now!", but it's slowly becoming a thing of its own as I find myself doing this separately (without running a workshop) more and more.

![slide 1](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-01.jpg)

![slide 2](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-02.jpg)

![slide 3](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-03.jpg)

It would seem that as developers, we spend too much time on fluff, like every latest shiny frameworks, but not enough time on solving the real problems better.

I think we can use more focus on the essentials, not fluff. This is what I call "pragmatic" - the keyword for my workshop "pragmatic unit testing now!"

![slide 4](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-04.jpg)

Believe it or not, we all test! It's human nature.
We test drive when we buy a car. We test water temperature before stepping into the shower or a swimming pool. Buy a house, we get building inspection.
In software, when people say "We don't have time so we don't test", that's simply not true. These people may not have automated testing, but they will test alright - either by looking at the user interface, or manually try out their algorithm.

The difference is then, whether testing is automated, or manual; whether efficient, or wasteful; whether it suits a purpose, or defeats it.

Unit testing is usually considered automated, efficient and suitable. Although it's not always the case, more often than not, we consider it a practice that gives more benefits than grief.

![slide 5](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-05.jpg)

Unit testing, as a form of automated testing, is both underrated and overrated. That is fitting to developers because we gravitate to over-simplification or over-complication.

Underrated as in "It's a just a matter of spending time to do it. Sure if I spend enough time, I can write tests as good as Edsger Dijkstra (mind the irony). ALTHOUGH I am not writing any as I don't have time for my current work"

Overrated as in "I know so many advanced techniques and tricks, I can write unit tests better than anybody else"

My take on this though, is that there is no and should not be "advanced unit testing" pe se. There are bad practices, which ironically are called "good" practices, especially "perfect" practices, but in daily programming for most of us, there should be no rocket science in unit testing.

When these ideas are put plainly though, we know the answer is not in the extremes - that's where pragmatism comes in handy.

![pragmatism](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/slide-20-the-pragmatic-aproach.jpg)

Pragmatism also indicates the benefit justifies the effort. A good principle here is: less code is usually better than more. By usually I mean 99% of the time. Remember unit testing is also coding, so it's important that we keep it as slim as possible.

It's best that we focus our effort on the essential bits. And if what we work on happens to be trivial? We simply don't test it. We'll explore triviality more later.

Let's first see how we can write less tests? 

![slide 6](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-06.jpg)

If we resort to pragmatism, it's not hard to arrive at the conclusion that we don't always need unit testing. There are times unit testing is simply not necessary.

For example, if I am making a pamphlet website, should I unit test? Possibly not. I can test by looking at the UI directly. In fact, integrated dev tools in browsers would make it rather pleasant and efficient. Writing unit tests would be an overkill.

![slide 7](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-07.jpg)

It's well known that in dynamically typed languages, code coverage is a very big thing. Whereas in statically typed languages, not so much.

Of course "perfect" code is hard to write, what is very much possible, is self-validating code.

![slide 8](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-08.jpg)

This example is written in `Idris`, and believe it or not, it needs no unit testing, or rather, it is not possible to write a unit testing for it. Because if you try, any test cases that do not satisfy the constraints simply won't even compile. So any code we write must be valid.

Except when we deal with runtime values instead of compile-time values. In order to accept user input for a password, we need a way to *Decide* that the input value is valid or not. This however, is [not for the faint of heart](https://github.com/hackle/idris/blob/master/range.idr).

We can definitely write a unit testing for this, although in practice, it's usually very hard to get it wrong. The point is, is it worthwhile to do this for the software most of us build? Possibly not.

![slide 9](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-09.jpg)

We can also look at a "lesser" but no less brilliant type system, that of `TypeScript`, which I think strikes a pretty good balance, with some very very interesting features.

![slide 10](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-10.jpg)

Using good abstraction is another way out of unit testing - we can defer boiler-plates to battle-tested patterns or libraries. One good example is how `Lens` abstracts away navigating and updating data in light of immutability, null safety as well as type safety. See [TsMiniLens](https://github.com/hackle/TsMiniLens).

![slide 11](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-11.jpg)

![slide 13](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-13.jpg)

![slide 14](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-14.jpg)

So let's say if code is trivial, then no tests are necessary, can we take advantage of this, and say in order to write fewer tests, I intentionally keep my code trivial? Some people call it "reversing the arrows", or duality. Would that work? You would have noticed, I haven't been talking too much about unit testing!

Suppose we've made decisions on what language to use, what libraries to import, then more practically, we would be dealing with complexity of the domain, and this trick is possibly much more relevant than the others.

Good news is, this trick can turn out to be very helpful, and even better, there are just a couple of things we need to address before we can take full advantage of it.

![slide 16](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-16.jpg)

As an example, one of these issues, perhaps the worst of all, is I.O. and I.O. is a dog! It pollutes everything and makes things hard to unit test.

In a typical layered architecture, I.O. happens almost on all layers, from presentation, to business then to data layer, although sometimes in the form of interfaces to repository, or D.A.L., we write code that is very much aware of I.O.

![slide 17](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-17.jpg)

A pragmatic approach to this problem, is to segregate I.O. onto it's own layer. This layer will be responsible for things like accepting user input, marshalling and unmarshalling, reading and updating database. The key is, all I.O. stops here.

What about the business layer, or the domain? Well it makes domain decisions. An important thing to note is, these decisions are usually based on data and data alone. It needs data, but how we get the data is kind of irrelevant, and can be abstracted away.

![slide 18](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-18.jpg)

So how do we reach this state of **pragmatism** with unit testing? There are in fact just 3 steps and they are all very simple.

With that said, once we get to the third step, there is a chance that we will discover something unexpected. As is usually the case with a journey: we make a plan, but there will be surprises!

![slide 19](https://s3-ap-southeast-2.amazonaws.com/hacklewayne.com/img-19.jpg)

You can get in touch for a full workshop. Or follow this blog for a hopefully interesting read once in a while.