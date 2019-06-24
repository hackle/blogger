class Creature {}
class Animal extends Creature {}
class Dog extends Animal {}

type Func<T> = () => T;

let dogGetter : Func<Dog> = () => new Dog();
let animalGetter : Func<Animal> = () => new Animal();
let creatureGetter : Func<Creature> = () => new Creature();

animalGetter = dogGetter;
console.log(animalGetter());

animalGetter = creatureGetter;
console.log(animalGetter());