
function greetReal(name: string, whenReady: (message: string) => void): void {
    const message = `Hello ${name}`;

    whenReady(message);
}

function greetFake(name: string, whenReady: () => void): void {
    whenReady();
}

greetReal('George', () => console.log('Hello'));
greetReal('George', (message: string) => console.log('Hello'));



greetFake('George', () => console.log('Hoy'));

type Greeting = typeof greetReal;
// const greet1: Greeting = greetFake;

function greet(name: string): string {
    return `Hello ${name}`;
}

interface Animal { name: string };
interface Tiger extends Animal { kind: 'cat' };

function greetTiger(name: string, whenReady: (tiger: Tiger) => void): void {
    const tiger: Tiger = { name, kind: 'cat' };
    whenReady(tiger);
}

greetTiger('Howler', (tiger: Animal) => console.log(tiger.name));

greetTiger('Howler', () => console.log('do not care'));