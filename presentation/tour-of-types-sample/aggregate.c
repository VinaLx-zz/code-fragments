#include <math.h>

struct Point {
    double x;
    double y;
};

double Mod(const Point *p) {
    return sqrt(pow(p->x, 2) + pow(p->y, 2));
}

Point Normalize(const Point *p) {
    double m = Mod(p);
    Point result = {p->x / m, p->y / m};
    return result;
}
