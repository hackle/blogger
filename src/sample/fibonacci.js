// fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

function* cons(x, xs) {
    yield x;
    yield* xs;
}

function* zip(
    zipper, 
    xs, 
    ys
) {
    const [ iterX, iterY ] = [ xs, ys ].map(fn => fn());

    while (true) {
        const [ x, y ] = [ iterX, iterY ].map(i => i.next());
        if (x.done || y.done) break;

        yield zipper(x.value, y.value);
    }
}

function* tail(xs) {
    let skip = true;
    while (true) {
        const x = xs.next();
        if (x.done || skip) break;

        skip = true;
        yield x.value;
    }
}

function print(n, xs) {
    let i = 0;
    while (i < n) {
        let x = xs.next();
        if (x.done) break;
        
        console.log(x.value);

        i++;
    }
}

const fibs = cons(0, cons(1, zip((a, b) => a + b, () => fibs, () => tail(fibs))));

// print(5, cons(1, cons(2, function*() { yield 3; }())));
print(5, fibs);