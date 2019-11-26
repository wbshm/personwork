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

    public Point getLast() {
        return last;
    }

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
    public void accept(DrawVisitor drawVisitor) {
        drawVisitor.drawing(this);
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
