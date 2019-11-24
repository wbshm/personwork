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

    PolylineCommand() {
    }

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

    ArrayList<Point> getPoints() {
        return this.points;
    }


    @Override
    public void execute(GraphicsContext g) {
        ArrayList<Point> points = this.getPoints();
        g.setStroke(getColor());
        double[] xPoints = new double[points.size()];
        double[] yPoints = new double[points.size()];
        for (int i = 0; i < points.size() - 1; i++) {
            Point p2 = points.get(i + 1);
            Point p1 = points.get(i);
            xPoints[i] = p1.x;
            yPoints[i] = p1.y;
            g.strokeLine(p1.x, p1.y, p2.x, p2.y);
        }
        if (last != null) {
            g.strokeLine(points.get(points.size() - 1).x, points.get(points.size() - 1).y, last.x, last.y);
        } else if (isFill()) {
            g.setFill(this.getColor());
            xPoints[points.size() - 1] = points.get(points.size() - 1).x;
            yPoints[points.size() - 1] = points.get(points.size() - 1).y;
            g.fillPolygon(xPoints, yPoints, points.size());
        } else if (points.size() > 2) {
            Point first = points.get(0);
            Point last = points.get(points.size() - 1);
            g.strokeLine(last.x, last.y, first.x, first.y);
        }
    }

    @Override
    public String toString() {
        StringBuilder str = new StringBuilder();
        str.append("\tpoints\n");
        for (Point p : points) {
            str.append("\t\tpoint:(").append(p.x).append(",").append(p.y).append(")\n");
        }
        str.append("\tend points\n");
        return "Polyline\n" +
                "\tcolor:" + getColorString() + "\n" +
                "\tfilled:" + isFill() + "\n" +
                str +
                "End Polyline\n";
    }
}
