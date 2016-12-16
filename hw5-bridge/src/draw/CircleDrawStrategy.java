package draw;

import graph.Edge;

import java.util.Iterator;
import java.util.Random;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 16.12.16
 */
public class CircleDrawStrategy implements DrawStrategy {

    private int vRadius = 40;

    public int getVRadius() {
        return vRadius;
    }

    public void setVRadius(int vRadius) {
        this.vRadius = vRadius;
    }

    @Override
    public void drawGraph(DrawingApi drawingApi, String[] labels, Iterator<Edge> edgeIterator) {
        int w = drawingApi.getDrawingAreaWidth();
        int h = drawingApi.getDrawingAreaHeight();
        int side = Math.min(w, h) / 2;
        if (side <= 2 * vRadius) {
            throw new IllegalStateException("Area is too small");
        }
        //Random rnd = new Random(1);
        int n = labels.length;
        Point[] centers = new Point[n];
        for (int i = 0; i < n; ++i) {
            double angle = i * 2 * Math.PI / n;
            //int r = rnd.nextInt(side - 2 * vRadius);
            int r = side - 2 * vRadius;
            int x = (int)(Math.cos(angle) * r) + side;
            int y = (int)(Math.sin(angle) * r) + side;
            centers[i] = new Point(x, y);
            drawingApi.drawCircle(x, y, vRadius);
            drawingApi.drawLabel(x, y, labels[i]);
        }

        for (; edgeIterator.hasNext();) {
            Edge e = edgeIterator.next();
            Point vec = centers[e.getV()].
                        minus(centers[e.getU()]).
                        normalize();
            Point uPoint = centers[e.getU()].plus(vec.multiply(vRadius));
            Point vPoint = centers[e.getV()].minus(vec.multiply(vRadius));
            drawingApi.drawLine(uPoint.x, uPoint.y, vPoint.x, vPoint.y);
        }
    }
}
