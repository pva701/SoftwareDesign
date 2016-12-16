package draw;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 16.12.16
 */
public class Point {
    public double x, y;

    public Point(double x, double y) {
        this.x = x;
        this.y = y;
    }

    public Point minus(Point a) {
        return new Point(x - a.x, y - a.y);
    }

    public Point div(double k) {
        return new Point(x / k, y / k);
    }

    public double length() {
        return Math.sqrt(x * x + y * y);
    }

    public Point normalize() {
        double l = length();
        return new Point(x / l, y / l);
    }

    public Point multiply(double k) {
        return new Point(x * k, y * k);
    }

    public Point plus(Point a) {
        return new Point(x + a.x, y + a.y);
    }
}
