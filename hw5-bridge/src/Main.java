import draw.CircleDrawStrategy;
import graph.Graph;
import graph.GraphAdjacencyMatrix;
import graph.GraphEdgeList;
import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.canvas.*;
import javafx.stage.Stage;

import java.io.FileInputStream;
import java.io.IOException;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 16.12.16
 */
public class Main {
    public static void main(String[] args) throws IOException {
        if (args.length != 3) {
            System.out.println("Invalid arguments");
            System.exit(-1);
        }

        String api = args[0];
        String gr = args[1];
        String file = args[2];

        Graph graph = null;
        switch (gr) {
            case "list":
                graph = new GraphEdgeList(new FileInputStream(file));
                graph.setDrawStrategy(new CircleDrawStrategy());
                break;
            case "matrix":
                graph = new GraphAdjacencyMatrix(new FileInputStream(file));
                graph.setDrawStrategy(new CircleDrawStrategy());
                break;
            default:
                System.out.println("Invalid graph format");
                System.exit(-1);
        }

        switch (api) {
            case "awt":
                new AwtDrawing(graph);
                break;
            case "fx":
                FxDrawing.runFx(graph, args);
                break;
            default:
                System.out.println("Invalid draw api");
                System.exit(-1);
        }
    }
}
