import draw.AwtDrawingApi;
import draw.DrawingApi;
import graph.Graph;

import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class AwtDrawing extends Frame {

    private Graph graph;
    public AwtDrawing(Graph graph) {
        this.graph = graph;

        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent we) {
                System.exit(0);
            }
        });
        setSize(1000, 700);
        setVisible(true);
    }

    @Override
    public void paint(Graphics g) {
        DrawingApi api = new AwtDrawingApi(g, getWidth(), getHeight());
        graph.setDrawingApi(api);
        graph.drawGraph();
    }
}