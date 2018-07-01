## What I want

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
```

Satisfies the following assertions

```TypeScript   
// this sets street to waterloo
const updatedPerson = updater.set(person1, 'Waterloo'); 

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
## Implementation

The implementation is surprisingly straightforward.

```TypeScript
class Updater<T> {
    static for<T>() {
        // overloads needed for different number of parameters
        return { 
            withPath: <P1 extends keyof T, P2 extends keyof T[P1]>(p1: P1, p2: P2) => new Updater<T>([p1, p2])
        };
    }

    constructor(public fields: any[]) {
    }

    set(obj: T, val: any): T {
        const paired = this.fields
            .reduce((st, f) => (st == null || st.nested == null) ? null : { 
                    pairs: st.pairs.concat([ { Key: f, Value: st.nested }]), 
                    nested: st.nested[f]
                }, 
            { pairs: [], nested: obj });

        
            
        return paired == null ? 
            obj : 
            paired.pairs.reduceRight((st, pair) => ({ ...pair.Value, [pair.Key]: st }), val);
    }
}


```

## Todo

You'll notice this is far from exhaustively covering all possibilities. To name a few missing features, working with
* union types
* arrays
* passing in a lambda for the **over** function

I can see this will evolve into a more STAB like pattern if all the above are implemented. Hopefully they'd be covered when real need emerges in more serious forms.

## Background

When working with TypeScript, I liked the increase popularity of immutability and other usually considered "functional programming" concepts.

One consequence of immutability is the need for handy ways to update complex data structures. In Haskell, this is indisputably the domain of the almighty [Lens](https://hackage.haskell.org/package/lens).

Ports of lens do exists but in my opinion they mostly still appear esoteric, and usually not simple enough.