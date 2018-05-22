class Point {
    x: number;
    y: number;
}
class Line {
    a: Point;
    b: Point;
}

function Length(l: Line) {
    return 10086;
}

let p1 = {x: 1, y: 1};
let p2 = {x: 1, y: 1, z: 1};

let l1 = {a: p1, b: p1};
let l2 = {a: p1, b: p1, c: "hello"};
let l3 = {a: p1, b: p2};

Length(l1); // exact
Length(l2); // subtype by width
Length(l3); // subtype by depth
