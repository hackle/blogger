## a tale of two copies

The CAP theorem describes a phenomenon in distributed databases: out of these 3 guarantees: Consistency, Availability and Partition Tolerance, no more than 2 can be satisfied at the same time. 

If you find the [Wikipedia](https://en.wikipedia.org/wiki/CAP_theorem) definition a bit dense, here is an over-simplified example: presumed for reasons such as security, load balancing etc, all data of an application must be saved in 2 identical copies.

Now I want to save a piece of data called "foo" then read it back right away. Below is what CAP means.

* Partition Tolerance: if either copy fails I should still be able to save "foo" and read it back.
* Consistency: after "foo" is saved to one copy, it will be synced to the other, so both copies are kept identical.
* Availability: As soon as I save "foo" to one copy, I can read it back with no delay. Or, it's immediately available.

They all look like good things to have, so why don't we have all? Well, the CAP theorem reveals this is not possible, or there will be conflicts.

For example, we start with Consistency and Partitioning. Consistency demands "foo" is in sync between 2 copies, and Partitioning says one copy can fail. When one copies fails, the database will wait for the failing copy to come back to life to ensure consistency, and won't make the data available so I can't read "foo" back right away. In another word, availability is compromised.

Or let's start with Availability and Partitioning: I decide I really need to be able to read "foo" back (Availability), in disregard to a failing copy (Partitioning). After a while, the failing copy comes back to life, and immediately (before it syncs up with the working copy) someone tries to read "foo" from it, guess what, "foo" is not there! Consistency is broken.

We can also choose Availability and Consistency, then we need to eliminate syncing, only way to do so is to keep only one copy - Partitioning is gone.

## a demanding boss

If it's still not clear enough, worry not, I have another analogy called "a demanding boss".

Hux hires Nic and Tas to keep "business" (wink wink) secrets, each in a separate office. It's a well-paying job, except there are a few SIMPLE rules, says Hux:

1) "I want any secret to be kept by BOTH of you at all times",
2) "Anytime I call to ask for a secret I want an answer IMMEDIATELY",
3) "I am very busy, so I will tell a secret to only one of you, and you must share between yourselves"

(It goes without saying, everything is done over the office phones which are "scrambled" for security).

Alas, you say, these are but simple and reasonable requirements, Hux is a good boss! But little did you know, Nic and Tas are in for a bit of trouble.

For example, if Hux first shares a secret with Nic, Nic will try to call Tas right after, according to rule 3. But if Hux calls Tas even sooner, Tas wouldn't have known the secret yet. This breaks rule 1. (Note it doesn't break rule 2 as Tas can simply say "I don't know").

Now, Nic and Tas get clever - they won't answer the call (even if they can) from Hux until they are done sharing the secret. But this breaks rule 2 and leaves an anxious Hux waiting on the other end of the line.

Under much more risk, Nic and Tas can secretly combine their offices into one (and rewire the phones), so anytime Hux shares anything with Nat, Tas can overhear (like in the movies!) and vice versa. The problem is, Hux can't know anything about this arrangement - what's the point of hiring two people for the exact SAME job in the SAME office?!

You see, in order to keep their jobs, it's much better for Nic and Tas to reason with Hux and ask him to be more flexible about the rules, especially between rule 1 and rule 2.

(And I'll leave it to you to figure out what C, A and P each is in this analogy).

## software again

That's an exaggerated example, you may say, it won't ever happen in real life. I agree (thinking "How naive!"), but let's look at another example that's looser as an analogy, but more likely to take place.

This time, Hux is in charge of software development. As someone who values culture, business outcome and "quality", he makes these arrangements

1) people are broken up to small, autonomous teams, each team can work on their own projects separately
2) each team must move as FAST as possible to deliver software
3) however, consistency is important, so all code must be written following the same conventions across teams. 

(You would have heard, these conventions can include but are not limited to: indentation, curly bracket placement, naming, casing, design patterns, layered architecture, linting rules, dependency injection, framework, language, ect etc etc, you get the idea.)

Reasonable requirements again, but by now you must have figured out, poor Hux can't have them all, at least not simultaneously. If we consider arrangement 1 to be Partition Tolerance, then 2 is Availability and 3 Consistency, it's easy to forecast what problems can arise.

* If Hux wants to keep autonomous teams (Partitioning) and want the teams to move fast (Availability), then he must be flexible around the exhaustive coding conventions (Consistency), which takes time to enforce and will slow teams down. 
* If he really cares about the coding conventions (Consistency) over everything else, then he'll have to be more patient (Availability) when teams keep their coding styles in sync.
* Or he can put everyone in one big team (Partitioning). Now it's possible to coordinate coding conventions (Consistency) and move fast (Availability), but it will be hard (or messy) to work on many projects concurrently.

Of course it would be naive to think Hux would be stuck or torn between choices, in real life, people can work around such problems case by case. That though does not mean CAP cannot be used to make matters easier.

## make use of CAP

First, it is worth noting in modern software development, partition tolerance is usually not a choice - running a database or an application on a single instance is very much possible, but quite rare. Sames goes for large organisations with only one team. Therefore the choice is usually between Consistency or Availability.

Secondly, CAP states it's not possible to have all 3 guarantees **at the same time**. Systems can still have all 3 characteristics, such as with "eventual consistency"; but just not simultaneously. 

A good place to apply CAP is when choosing a database. It's essential to know that databases are made with CAP in mind. Typically, relational databases provide more consistency guarantee (and data integrity which is also a "consistency" concern), whereas non-relational databases typically offer more availability. This line is not always distinct - there are non-relational databases that can offer various forms of strong consistency. For example, CosmoDB offers different levels of consistency for the user's own choosing.

The choice of database is usually subject to the software product at hand, at which level CAP applies equally well. A banking system typically needs more consistency than an e-commerce website, for which availability must take precedence - people must be allowed to shop!

Even within one system, different components can differ in choice of CAP. Still the e-commerce website, the payment component must prioritise consistency, whereas the catalogue must always be available, even though at some point slightly out-of-date. 

There is a school of poor software architecture that arises from poor choice of CAP. Often people tend to reuse one "successful" architecture for different problems, without considering what choice of CAP is most fitting, resulting in wasted effort and pain of implementation. Admittedly, this goes beyond CAP, but intentional application of CAP can help align technological decisions with the nature of a product, and make life easier for many.

Speaking on looser terms, software teams can leverage CAP to inform organisational decisions. Using the previous example, if Hux makes up his mind to prioritise speed of development (as is fitting to the products being built), or availability, he needs to feel no regret in allowing inconsistencies in coding conventions. Indeed, a more inspirational way to communicate this is teams are trusted with the freedom of making up their own coding conventions. Sounds familiar?

Or, do you know Facebook once had a motto "move fast and break things"? Way to be inspirational, but guess where they stood with CAP?