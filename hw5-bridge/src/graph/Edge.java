package graph;

/**
 * @author Ilya Peresadin <pva701@gmail.com>
 * @created 16.12.16
 */
public class Edge {
    public final int u, v;

    public Edge(int u, int v) {
        this.u = u;
        this.v = v;
    }

    public int getU() {
        return u;
    }

    public int getV() {
        return v;
    }
}
