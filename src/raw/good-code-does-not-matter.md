"Good" code may not matter. Not always. At least not THAT much.

This is a scary comment so it certainly needs context - specific to the vast majority of BLOBAs (Boring line of business applications - I got it from Scott Wlaschin) we build on a daily basis.

Let me explain. Code is only useful when it serves a purpose as part of a software, which in turn is only useful when it serves an even bigger purpose. So with a bit of reasoning, it's easy to see that in the grand scheme of things, code might not be the only thing that matters.

To be fair, code solves its share of problems, but there are other problems not solvable with code. For example, team organization and workflow, or marketing and sales.

If code is not THAT important, then what is really important? I'll name a few subjects and see for each how much and when it matters, when it might not matter (that much), and how we can keep it relevant.

Before we delve in it's necessary to clarify that by "code" I mean source code. Sometimes we refer to code as solution, such as "code wins argument" - it clearly means **solution** wins argument. Another example of confusion is "code review" - it really should be called and practiced as solution review. **Code**-only review is bad practice. To be covered in a different post.

(You'll notice I am speaking from, and limited by personal experience in building software in the industry, not writing programme to prove theorems or to make a compiler, nor launching rockets or detecting alien beings, regarding which I am entitled to no say.)

## code style / convention

The argument for code style is consistency, and consistency makes code easier to read, as the claim goes. There is a certain amount of truth to it - but consistency in code style is often exaggerated out of proportion.

I am for consistency - as long as it does not get in the way. While code written in a consistent style is certainly pleasing, I don't find slight inconsistency in code style revolting either.

Obsession with code style sees developers having hour-long meeting on spaces vs tabs which is completely unproductive. Same goes for indentation, bracket placement, casing or even naming. Yes I said it - naming might not be as important as you think. If you struggle with naming something, just give it a super long name describing everything, and move on. 

Yes - just decide and move on - done is better than perfect.

All the lengthy arguments to regard code style as the sole reason for readability are very much exaggerated just the same. Programming is a creative trade, it's OK if people have different styles. Choice of spaces, tabs, indentation or naming do not have such a massive impact (as is often fervently alleged) on the readability of source code.

So if "good" code means code with good style only, then it really doesn't matter much. It's like taking good penmanship as good literature. They are related to some extent but one would be foolish to equate them. 

As programmers, we'll have to learn to move on from such superficial subjects. Here is a spoiler on my planned post on code review: try not to spend too much time reviewing and commenting on code style.

## algorithms

One can write code in beautiful style, but if the algorithm is incorrect, then the sexiness is for nothing.

Expressing algorithm correctly, clearly and concisely is truly essential to the trade, in fact this is how I would define real **good** code. A major part of software engineering is to design the algorithms, then implement and validate their correctness (such as with unit testing).

I am all for spending time on algorithm as this is where quality of software shines. However, I'd watch out for any form of obsession, which easily leads to premature optimization. This usually starts with developers thinking, "what if we get a thousand times more customers over night?" or "what if this becomes world's most popular software?" While this can be true for a few places in the world, it certainly won't be the case for the vast majority of us.

Optimization can lead to less readable code as the approaches are usually imperative, or less intuitive, and can also make testing more difficult. The biggest sin of all, of course, it's that it usually takes much more time to implement than the plain versions.

While optimization certainly has it's moments, most of the time it can be deferred until absolutely necessary. Even then, one can usually resort to architecture changes so simplicity can be preserved. A good mantra to stick to is "first make it work, then make it right, and, finally, make it fast." Most of the time, "make it fast **enough**" is enough (enough is enough?!). And if it's not slow? Leave it be and move on.

## tests, or proof

The most elegant algorithms, if cannot be proven correct, is as good as none. Between *average algorithm + good tests* and *excellent algorithm + bad tests*, my choice is usually for good tests. Without good tests maintainability is but an empty word.

Code coverage is the usual pitfall here. Yet another example of human weakness - we must reduce everything to a number so we can compare, celebrate or point the finger. 80% is certainly better than 79%, indisputable! Naturally, tests can extensively cover triviality but skimp through essential decision-making - if the end result is the same percentage, why not take the easy way out?

Why is that? Good tests are not easy to create, it takes guidance and practice. Following main-stream industry practices does not necessarily help here: layered architecture, dependency injection and mocking can make unit testing a major pain. Not to mention some of the questionable practices championed by popular frameworks - for example, Angular.

My take on testing (which makes up a big part of this blog) is that it should be first-class in code or architectural design; we should consciously separate complexity, segregate IO to make the software easy to verify and validate.

So here it goes: forget about code coverage. Find what parts are really important in the software, make them easy to test, test them thoroughly and move on.

## architecture

Say we have robust algorithms validated with solid tests, and expressed in simple code, what else could go wrong? Well, there is still the big word, architecture, that could fail us.

If the architecture is off, then nothing is really wasted, but everything could be made extra difficult.

Architecture could go wrong in many different ways. Granularity is the latest hot topic. A bloated monolithic solutions can give distributed teams much grief. Teams trip on each other's toes navigating a shared code base, the software breaks accidentally, easily leading to stress and a blame-ful culture; this also helps growing a heavy process that eventually makes doing any simple thing slow.

On the other hand, overly granular microservices can result in overhead in orchestration or communication. It's usually fun to start with this style, but as services aggregate, we will find that dependencies still exist, only now hidden than explicit, leading to expected failures and much harder trouble-shooting; growing duplication in data or functionality that are hard to keep track of; as time goes we struggle more and more to maintain the grip on the big picture.

And it's never just technical. Architecture usually needs to suit team structure, otherwise it becomes an endless source of conflicts, result in tension and low level of happiness.

There is also the so called god-architecture which I coined from the term "God object". A god architecture tries to do everything and leaves nothing to interpretation. With unquestionably the best intentions, god architecture rarely works. Symptoms? It usually takes much time to put together, and once it is, we will find it easily broken due to its rigidity; as it tries to do everything, it usually does nothing really well.

My advice? Build architectures in an [evolutionary way](https://www.thoughtworks.com/insights/blog/microservices-evolutionary-architecture). Most important characteristics for an evolutionary architecture: it should define (preferably physical vs logical) boundaries, is organized around business capabilities (vs technical aspects), enable experimentation (vs big bangs).

## there is more!

The list goes on! We can have a good architecture, but it won't help us much if there is a terrible process in place. Or team structure and communication. Or management, funding and profitability. So on and so forth. But I promised to keep it technical (or I'll be out of my depth anyway).

## summary

Hopefully I've made my point clear - software development is a systematic effort. It's never about one thing and one thing alone. We need to take care of subjects on different levels; when a weak link exists,  productivity will suffer.

This also reminds us obsession with any particular subject (or worse, a certain style of a certain subject), be it code style or architecture, without considering the big picture, can be counter-productive.

Consistency can be the source of evil too - any obsession with consistency is possibly missing the point. Even at architecture level, there is the option of making it evolutionary.

Of course most of this only applies to software as an engineering practice, which is an art of compromises at large. In academia, or to a lesser extent when we study a certain subject, it pays to have passion, be thorough, or even be obsessed.