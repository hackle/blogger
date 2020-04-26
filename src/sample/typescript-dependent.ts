type OnOff = 'on' | 'off';

// function toggle(st: OnOff): OnOff {
//     return st === 'on' ? 'off' : 'on';
// }

type Toggle<T extends OnOff> = T extends 'on' ? 'off' : 'on';

const state1: OnOff = 'on';
const state2: Toggle<typeof state1> = 'off';
const state3: Toggle<typeof state1> = 'on';

function toggle<T extends OnOff>(st: T): Toggle<T> {
    return st === 'on' ? 'off' : 'on' as any;
}

const state4 = toggle('on');


interface Functor<Ins, El1, El2> {
    map(fn: (el: El1) => El2): Ins;
}