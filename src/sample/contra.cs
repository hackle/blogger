public class Creature {}
public class Animal : Creature {}
public class Dog : Animal {}

IEnumerable<Dog> dogs = Enumerable.Empty<Dog>();
IEnumerable<Creature> creatures = Enumerable.Empty<Creature>();
IEnumerable<Animal> animals = creatures;