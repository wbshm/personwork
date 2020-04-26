package ca.utoronto.utm.paint;

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
        return "Squiggle\n" +
                "\tcolor:" + getColorString() + "\n" +
                "\tfilled:" + isFill() + "\n" +
                str +
                "End Squiggle\n";
    }
}
