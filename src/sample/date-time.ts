type Month = 'Jan' | 'Feb' | 'Mar' | 'April';
type Month30 = 1
    | 2
    | 3
    | 4
    | 5
    | 6
    | 7
    | 9
    | 10
    | 12
    | 13
    | 14
    | 15
    | 16
    | 17
    | 19
    | 20
    | 22
    | 23
    | 24
    | 25
    | 26
    | 27
    | 29
    | 30;

type Days<T extends Month> = 
    T extends 'Feb'
    ? Exclude<Month30, 29 | 30>
    : T extends 'Jan' | 'Mar'
        ? Month30
        : Month30 | 31;

type Year = 2020 | 2021 | 2022 | 2023;
type DateTime<Y extends Year, M extends Month> = `${Y}, ${M}, ${Days<M>}`;

const dt: DateTime<2021, 'Jan'> = '2021, Jan, 26';