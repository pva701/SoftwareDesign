import draw.FxDrawingApi;
import graph.Graph;
import javafx.application.Application;
import javafx.scene.Group;
import javafx.scene.Scene;
import javafx.scene.canvas.Canvas;
import javafx.scene.canvas.GraphicsContext;
import javafx.stage.Stage;

/**
 * @author akirakozov
 */
public class FxDrawing extends Application {
    private static Graph graph;

    public static void runFx(Graph g, String[] args) {
        graph = g;
        launch(args);
    }

    public FxDrawing() {}

    @Override
    public void start(Stage primaryStage) {
        primaryStage.setTitle("Drawing circle");
        Group root = new Group();
        Canvas canvas = new Canvas(1000, 700);
        GraphicsContext gc = canvas.getGraphicsContext2D();

        graph.setDrawingApi(new FxDrawingApi(gc));
        graph.drawGraph();

        root.getChildren().add(canvas);
        primaryStage.setScene(new Scene(root));
        primaryStage.show();
    }
}