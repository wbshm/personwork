package ca.utoronto.utm.paint;

import javafx.scene.canvas.GraphicsContext;

import java.util.ArrayList;

public class SquiggleCommand extends PaintCommand {
    private ArrayList<Point> points = new ArrayList<Point>();

    public void add(Point p) {
        this.points.add(p);
        this.setChanged();
        this.notifyObservers();
    }

    public ArrayList<Point> getPoints() {
        return this.points;
    }


    @Override
    public void execute(GraphicsContext g) {
        ArrayList<Point> points = this.getPoints();
        g.setStroke(this.getColor());
        for (int i = 0; i < points.size() - 1; i++) {
            Point p1 = points.get(i);
            Point p2 = points.get(i + 1);
            g.strokeLine(p1.x, p1.y, p2.x, p2.y);
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
        return "Squiggle\n" +
                "\tcolor:" + getColorString() + "\n" +
                "\tfilled:" + isFill() + "\n" +
                str +
                "End Squiggle\n";
    }
}
