package draw;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 02.12.16
 */
public interface DrawingApi {
    int getDrawingAreaWidth();
    int getDrawingAreaHeight();
    void drawCircle(double x, double y, double r);
    void drawLabel(double x, double y, String label);
    void drawLine(double x1, double y1, double x2, double y2);
}
