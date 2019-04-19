type Contains<A> = { a: A, o: any };

type Lens<S, T, A, B> = (f: (a: A) => Contains<B>) => (s: S) => Contains<T>;

function fmap<A, B>(f: (a: A) => B, c: Contains<A>): Contains<B> {
    return { ...c, a: f(c.a) };
}

type Address = { street: string, suburb: string };
type Person = { name: string, address: Address };

const lname : Lens<Person, Person, string, string> = 
    (f: (name: string) => Contains<string>) => 
        (p: Person) => fmap((n => <Person>{ ...p, name: n}), f(p.name));
    
const laddress : Lens<Person, Person, Address, Address> =
    (f: (address: Address) => Contains<Address>) =>
        (p: Person) => fmap((n => <Person>{ ...p, address: n }), f(p.address))

const lstreet : Lens<Address, Address, string, string> =
    (f: (street: string) => Contains<string>) =>
        (a: Address) => fmap((n => <Address>{ ...a, street: n }), f(a.street));

function composeLens<A, B, C>(
    l1: Lens<A, A, B, B>,
    l2: Lens<B, B, C, C>
): Lens<A, A, C, C> {
    return (f: (c: C) => Contains<C>) =>
        (a: A) => l1(b => l2(c => f(c))(b))(a);
}


const p2name = composeLens(laddress, lstreet);

const Identity = <A>(a: A) => <Contains<A>>{ a: a, o: null };
const Const = <A>(a: A) => <Contains<A>>{ a, o: a };
        
const person = { name: 'Hackle', address: { street: 'Sale', suburb: 'CBD' } };
const me = lname(Identity)(person).a;
const justName = lname(Const)(person).o;

console.log(p2name(Identity)(person).a)
console.log(p2name(Const)(person).o)