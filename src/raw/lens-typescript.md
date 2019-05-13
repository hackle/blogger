You would have heard of lens if you use or (like me) dabble in Haskell. Loosely called functional getters and setters, it is popularized by the mighty Edward Kmett with his famous [lens package](http://hackage.haskell.org/package/lens).

Ironically, the lens proper condones an imperative syntax, but the idea is unfortunately not really utilized enough for the Object Oriented world, to the dismay of many who have had a taste of the mojo and then have to go back to stateful assignments and unsafe navigations.

I for one have craved for a port of lens in TypeScript when working with redux in TypeScript: while immutability is great, spreading through nested data structures is not particularly fun.

Existing libraries such as Rambda does provide [lens-like functions](https://ramdajs.com/docs/#lens), which is great in itself; however its lack of type-safety and functional-syntax (which to be fair it's the purpose of Rambda) can be off-putting to those who are used to OO styled programming.

I then resolved to take matters to my own hands and took on to build something that's lens-like in spirit, but can appear not-scary-at-all, idiomatic to OO programmers.

TypeScript proved to be a bless - its beautiful type system helped me get type-safety and intellisense for this tiny library - [TsMiniLens](https://github.com/hackle/TsMiniLens).

Below is how it works.

![See it in action](https://github.com/hackle/TsMiniLens/raw/master/demo.gif)

## Use cases

### Given
```TypeScript
interface Address { city?: string; street: string };
interface Person { name?: string; address: Address };

const lensPerson2Street = lensFor<Person>().withPath('address', 'street'); // this is type safe, e.g. 'street1' wont't compile

// or since version 1.1.6
const lensPerson2Street = lensFrom<Person>().to('address', 'street');

```

### view() to navigate safely

We all know the dreaded null reference exception (Law of demeter applies)

```TypeScript
const street = person.address.street; // error if address is null!
```

with lens this never happens, in the following case, if address is null then view() returns null instead of erroring out

```TypeScript
const street = lensPerson2Street.view(person); // safe!
```

### set() or over() to update easily

If immutability is a concern, then updating a nested data structure can be tedious.
```TypeScript
const updatedPerson = {
    ...person,
    address: {
        ...person.address,
        street: 'new street'
    }
};
// imagine more nesting! :(
```

with ``set()`` this becomes a breeze. It does a CoW (Copy on Write) to support immutability.
```typescript
const personRelocated = lensPerson2Street.set(person, 'new street');
```
Note ``personRelocated`` is a different object than ``person``, or, ``person !== personRelocated``.

``over()`` is handy if we are to modify (but not replace) the current street,
```
const updatedPerson = lensPerson2Street.over(person, street => 'Level 2' + street);
```
Quiz: how to implement ``set()`` in terms of ``over()``?

### chain() and castIf()

It's also possible to chain lenses with ``lens1.chain(lens1)`` or more fluently, ``lens.then.withPath('level1', 'level2')``

``lens.castIf(typeGuard)`` supports navigating through union types safely with type guards. In lens / optics terms, this might be the equivalence of prism.

## yet to come

### insert

Unlike `set` that stops at any null value, `insert` will create objects along the way, and guarantee the value will be eventually present.

```typescript
lensPerson2Street.insert(null, 'Sale Street');

// { address: { street: 'Sale Street' } }
```

## Remember it's mini
Bear in mind it's mini indeed - there is absolutely no parity with lens proper in Haskell.