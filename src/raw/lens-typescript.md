# What I want

An idiomatic record updater in TypeScript that is type-safe, and null-aware.

Given

```TypeScript
type Address = { street: string; region: string }
type Person = { name: string; address: address }
const person1: Person = { 
    name: 'Foo', 
    address: {
        street: 'Queen',
        region: 'CBD'
        }
    }

const person2: Person = { name: 'Bar', address: null }
```

And an updater as follows,

```TypeScript
const updater = Updater.for<Person>().withPath('address', 'street');
const updatedPerson = updater.set(person1, 'Waterloo');    // this sets street to waterloo
```

Satisfies the following assertions

```TypeScript
// assert
updatedPerson.address.street === 'Waterloo';
// not the same reference to address
updatedPerson.address !== person1.address;
// not the same reference to person
updatedPerson !== person1;

```

Also,

```TypeScript
// null aware - address is null, no exception
const updatedPerson2 = updater.set(person2, 'Walterloo');
// this is satisfied
updatedPerson2.address === null;

// type safety - will not compile, district is not a field  of address
const badUpdater = Updater.for<Person>().withPath('address', 'district'); 

```

## Background

When working with TypeScript, I liked the increase popularity of immutability and other usually considered "functional programming" concepts.

One consequence of immutability is the need for handy ways to update complex data structures. In Haskell, this is indisputably the domain of the almighty [Lens](https://hackage.haskell.org/package/lens).

Ports of lens do exists but in my opinion they mostly still appear esoteric, and usually not simple enough.


## Most types are Maybe types

TBC