package draw;

import java.awt.*;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 16.12.16
 */
public class AwtDrawingApi implements DrawingApi {
    private Graphics2D ga;
    private int w;
    private int h;

    public AwtDrawingApi(Graphics g, int w, int h) {
        this.ga = (Graphics2D)g;
        ga.setPaint(Color.black);
        this.w = w;
        this. h = h;
    }

    @Override
    public int getDrawingAreaWidth() {
        return w;
    }

    @Override
    public int getDrawingAreaHeight() {
        return h;
    }

    @Override
    public void drawCircle(double x, double y, double r) {
        int x1 = (int)(x - r);
        int y1 = (int)(y - r);
        int r1 = (int)r;
        ga.drawOval(x1, y1, 2 * r1, 2 * r1);
    }

    @Override
    public void drawLabel(double x, double y, String label) {
        ga.drawString(label, (float)x, (float)y);
    }

    @Override
    public void drawLine(double x1, double y1, double x2, double y2) {
        ga.drawLine((int)x1, (int)y1, (int)x2, (int)y2);
    }
}
