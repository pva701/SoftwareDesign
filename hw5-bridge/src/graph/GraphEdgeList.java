package graph;

import draw.DrawStrategy;
import draw.DrawingApi;

import java.io.BufferedReader;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 02.12.16
 */
public class GraphEdgeList extends Graph {

    private int n;
    private List<Edge> edges;

    public GraphEdgeList(InputStream is) {
        readGraph(is);
    }

    @Override
    public void drawGraph() {
        String[] labels = new String[n];
        for (int i = 0; i < n; ++i) {
            labels[i] = "" + (i + 1);
        }
        drawStrategy.drawGraph(drawingApi, labels, edges.iterator());
    }

    private void readGraph(InputStream is) {
        Scanner scanner = new Scanner(is);
        this.n = scanner.nextInt();
        int m = scanner.nextInt();
        this.edges = new ArrayList<>();
        for (int i = 0; i < m; ++i) {
            this.edges.add(new Edge(scanner.nextInt(), scanner.nextInt()));
        }
    }
}
