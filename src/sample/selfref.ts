const person = {
    firstName: 'George',
    lastName: 'Costanza',
    get fullName() { return `${person.firstName} ${person.lastName}`; }
};

console.log(JSON.stringify(person));

const suzanne = { ...person, firstName: 'Suzanne' };
console.log(JSON.stringify(suzanne));