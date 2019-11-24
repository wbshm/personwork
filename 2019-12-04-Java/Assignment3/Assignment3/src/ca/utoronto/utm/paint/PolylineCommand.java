package ca.utoronto.utm.paint;

import javafx.scene.canvas.GraphicsContext;

import java.util.ArrayList;


/**
 * 1. left click
 * 2. right click
 * 3. mouse move
 */
public class PolylineCommand extends PaintCommand {
    private ArrayList<Point> points = new ArrayList<Point>();

    private Point last;

    PolylineCommand(Point point) {
        this.add(point);
    }

    public void add(Point p) {
        this.points.add(p);
        this.setChanged();
        this.notifyObservers();
    }

    void setLast(Point last) {
        this.last = last;
        this.setChanged();
        this.notifyObservers();
    }

    private ArrayList<Point> getPoints() {
        return this.points;
    }


    @Override
    public void execute(GraphicsContext g) {
        ArrayList<Point> points = this.getPoints();
        g.setStroke(this.getColor());
        for (int i = 0; i < points.size() - 1; i++) {
            Point p2 = points.get(i + 1);
            Point p1 = points.get(i);
            g.strokeLine(p1.x, p1.y, p2.x, p2.y);
        }
        if (last != null) {
            Point end = points.get(points.size() - 1);
            g.strokeLine(end.x, end.y, last.x, last.y);
        }
    }
}
