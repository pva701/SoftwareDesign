package draw;

import draw.DrawingApi;
import graph.Edge;

import java.util.Iterator;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 16.12.16
 */
public interface DrawStrategy {
    void drawGraph(DrawingApi drawingApi, String[] labels, Iterator<Edge> edgeIterator);
}
