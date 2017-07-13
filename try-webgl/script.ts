const canvas = <HTMLCanvasElement>document.getElementById("canvas");
const ctx = <CanvasRenderingContext2D>canvas.getContext("2d");

type Point = [number, number];

class Circle {
    constructor(origin: Point, radius: number, color: string) {
        this.origin = origin;
        this.radius = radius;
        this.color = color;
    }
    path(): Path2D {
        const path = new Path2D();
        path.arc(this.origin[0], this.origin[1], this.radius, 0, Math.PI * 2)
        return path;
    }
    fill(ctx: CanvasRenderingContext2D): void {
        ctx.fillStyle = this.color;
        ctx.beginPath();
        ctx.arc(this.origin[0], this.origin[1], this.radius, 0, Math.PI * 2)
        ctx.fill();
    }
    stroke(ctx: CanvasRenderingContext2D): void {
        ctx.strokeStyle = this.color;
        ctx.lineWidth = 3;
        ctx.stroke(this.path());
    }
    public origin: Point;
    public radius: number;
    public color: string;
}

function distance(a: Point, b: Point): number {
    return Math.sqrt(Math.pow(a[0] - b[0], 2) + Math.pow(a[1] - b[1], 2));
}

class Rocker {
    constructor(origin: Point, radius: number, color: string) {
        this.circle = new Circle(origin, radius, color);
        this.dot = new Circle(<[number, number]>origin.slice(0), radius / 15, color);
    }
    draw(ctx: CanvasRenderingContext2D): void {
        this.circle.stroke(ctx);
        this.dot.fill(ctx);
    }
    resetDot(): void {
        this.dot.origin = <[number, number]>this.origin.slice(0);
    }
    moveDotToward(p: Point): void {
        const dist = distance(p, this.origin);
        const radius = this.circle.radius;
        if (dist <= radius) {
            this.dot.origin = p;
            return;
        }
        const [dx, dy] = [p[0] - this.origin[0], p[1] - this.origin[1]];
        this.dot.origin[0] = this.origin[0] + dx * radius / dist;
        this.dot.origin[1] = this.origin[1] + dy * radius / dist;
    }
    get origin(): Point {
        return this.circle.origin;
    }
    direction(): Point {
        const dx = this.dot.origin[0] - this.origin[0];
        const dy = this.dot.origin[1] - this.origin[1];
        const dist = distance(this.dot.origin, this.origin);
        return [dx / dist, dy / dist];
    }

    private circle: Circle;
    private dot: Circle;
}

class Ball {
    constructor(c: Circle, direction: Point, velocity: number) {
        this.circle = c;
        this.velocity = velocity;
        this.direction = direction;
    }
    move(maxw: number, maxh: number): void {
        console.log("direction: ", this.direction)
        const dx = this.direction[0] * this.velocity;
        const dy = this.direction[1] * this.velocity;
        const x = this.circle.origin[0];
        const y = this.circle.origin[1];
        const newX = dx + x < 0 || dx + x > maxw ? x : x + dx;
        const newY = dy + y < 0 || dy + y > maxh ? y : y + dy;
        this.circle.origin = [newX, newY];
        console.log([newX, newY], [maxw, maxh]);
    }
    draw(ctx: CanvasRenderingContext2D): void {
        this.circle.fill(ctx);
    }

    private circle: Circle;
    private velocity: number;
    public direction: Point;
}

function resetCanvasSize(): void {
    const width = window.innerWidth, height = window.innerHeight;
    [canvas.width, canvas.height] = [width, height];
}

function drawBackground(color: string): void {
    ctx.fillStyle = color;
    ctx.fillRect(0, 0, canvas.width, canvas.height);
}

function start() {
    resetCanvasSize();
    const rocker = new Rocker([canvas.height / 5, canvas.height * 4 / 5], canvas.height / 6, "white");
    const ball = new Ball(new Circle([canvas.width / 2, canvas.height / 2], 10, "yellow"), [1, 0], 5);
    console.log(canvas.width, canvas.height);
    document.ontouchmove = e => e.preventDefault();
    function repaint(): void {
        drawBackground("black");
        rocker.draw(ctx);
        ball.move(canvas.width, canvas.height);
        ball.draw(ctx);
    }
    canvas.onmouseup = ev => rocker.resetDot();
    canvas.onmousemove = (event: MouseEvent) => {
        if (! (event.buttons & 1))
            return;
        rocker.moveDotToward([event.x, event.y]);
        ball.direction = rocker.direction();
    }
    canvas.onmousedown = (event: MouseEvent) => {
        rocker.moveDotToward([event.x, event.y]);
        ball.direction = rocker.direction();
    }
    function drawScreen(time: number): void {
        repaint();
        requestAnimationFrame(drawScreen);
    }
    window.requestAnimationFrame(drawScreen);
}