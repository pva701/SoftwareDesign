package graph;

import draw.DrawStrategy;
import draw.DrawingApi;

import java.io.InputStream;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 02.12.16
 */
public abstract class Graph {

    protected DrawingApi drawingApi;
    protected DrawStrategy drawStrategy;

    public DrawingApi getDrawingApi() {
        return drawingApi;
    }

    public void setDrawingApi(DrawingApi drawingApi) {
        this.drawingApi = drawingApi;
    }


    public DrawStrategy getDrawStrategy() {
        return drawStrategy;
    }

    public void setDrawStrategy(DrawStrategy drawStrategy) {
        this.drawStrategy = drawStrategy;
    }

    public abstract void drawGraph();
}
