package draw;

import javafx.scene.canvas.GraphicsContext;
import javafx.scene.paint.Color;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 16.12.16
 */
public class FxDrawingApi implements DrawingApi {
    private GraphicsContext gc;

    public FxDrawingApi(GraphicsContext gc) {
        this.gc = gc;
    }

    @Override
    public int getDrawingAreaWidth() {
        return (int)gc.getCanvas().getWidth();
    }

    @Override
    public int getDrawingAreaHeight() {
        return (int)gc.getCanvas().getHeight();
    }

    @Override
    public void drawCircle(double x, double y, double r) {
        int x1 = (int)(x - r);
        int y1 = (int)(y - r);
        int r1 = (int)r;
        gc.fillOval(x1, y1, 2 * r1, 2 * r1);
    }

    @Override
    public void drawLabel(double x, double y, String label) {
        gc.setFill(Color.WHITE);
        gc.fillText(label, x, y);
        gc.setFill(Color.BLACK);
    }

    @Override
    public void drawLine(double x1, double y1, double x2, double y2) {
        gc.strokeLine(x1, y1, x2, y2);
    }
}
