TL;DR if you are making a `foo.json` file then I can't help you! Otherwise, read on.

Wouldn't it be handy if we can write something like this?

```TypeScript
const person = {
    firstName: 'George',
    lastName: 'Costanza',
    fullName: `${person.firstName} ${person.lastName}`
};

console.log(JSON.stringify(person));
```

> error TS2448: Block-scoped variable 'person' used before its declaration.

It doesn't work - because when `person.firstName` is evaluated, `person` is not available yet.

Workarounds include declaring `firstName` and `lastName` separately before `person`, which is not as nice.

```TypeScript
const firstName = 'George';
const lastName = 'Costanza';

let person = {
    firstName,
    lastName,
    fullName: `${person.firstName} ${person.lastName}`
};
```

 or make `person` a variable (vs a const value) and initialise `fullName` as a second step. e.g.

```JavaScript
let person = {
    firstName: 'George',
    lastName: 'Costanza',
};

person.fullName = `${person.firstName} ${person.lastName}`;
```

Not the end of the world but it annoys me a lot as I really want `person` to be immutable.

There is also the option of making a `class Person`, a bit too heavy-handed for my taste.

Not all hope is lost. The trick is to make `fullName` a getter.

```TypeScript
const person = {
    firstName: 'George',
    lastName: 'Costanza',
    get fullName() { return `${person.firstName} ${person.lastName}`; }
};

console.log(JSON.stringify(person));
```

> {"firstName":"George","lastName":"Costanza","fullName":"George Costanza"}

It works because by the time the `fullName` getter is evaluated, `person` is now available. The benefit of lazy evaluation.

Be warned though, if you are already likening it to something like `this.firstName + this.lastName`.

```TypeScript
const suzanne = { ...person, firstName: 'Suzanne' };
console.log(JSON.stringify(suzanne));
```
> {"firstName":"Suzanne","lastName":"Costanza","fullName":"George Costanza"}

It makes sense - the getter still uses `person.firstName`, not `suzanne.firstName`. So `suzanne` will need its own `fullName` getter. No surprise, and definitely not enough reason to convert to `class`.