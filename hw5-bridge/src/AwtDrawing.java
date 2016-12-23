import draw.AwtDrawingApi;
import draw.DrawingApi;
import graph.Graph;

import javax.swing.*;
import java.awt.*;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public class AwtDrawing extends JFrame {

    private Graph graph;
    public AwtDrawing(Graph graph) {
        this.graph = graph;
        getContentPane().add(new JPanel() {
            @Override
            public void paint(Graphics g) {
                DrawingApi api = new AwtDrawingApi(g, getWidth(), getHeight());
                graph.setDrawingApi(api);
                graph.drawGraph();
            }
        });

        addWindowListener(new WindowAdapter() {
            public void windowClosing(WindowEvent we) {
                System.exit(0);
            }
        });
        setSize(1000, 700);
        setVisible(true);
    }
}