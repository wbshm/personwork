package ca.utoronto.utm.paint;

import javafx.scene.canvas.GraphicsContext;

import java.util.ArrayList;
import java.util.Observable;
import java.util.Observer;

public class DrawVisitorImpl extends Observable implements DrawVisitor, Observer {

    private GraphicsContext g;

    public GraphicsContext getG() {
        return g;
    }

    public void setG(GraphicsContext g) {
        this.g = g;
    }


    @Override
    public void drawing(CircleCommand circle) {
        int x = circle.getCentre().x;
        int y = circle.getCentre().y;
        int radius = circle.getRadius();
        if (circle.isFill()) {
            g.setFill(circle.getColor());
            g.fillOval(x - radius, y - radius, 2 * radius, 2 * radius);
        } else {
            g.setStroke(circle.getColor());
            g.strokeOval(x - radius, y - radius, 2 * radius, 2 * radius);
        }
    }

    @Override
    public void drawing(RectangleCommand rectangle) {
        Point topLeft = rectangle.getTopLeft();
        Point dimensions = rectangle.getDimensions();
        if (rectangle.isFill()) {
            g.setFill(rectangle.getColor());
            g.fillRect(topLeft.x, topLeft.y, dimensions.x, dimensions.y);
        } else {
            g.setStroke(rectangle.getColor());
            g.strokeRect(topLeft.x, topLeft.y, dimensions.x, dimensions.y);
        }
    }

    @Override
    public void drawing(PolylineCommand polyline) {
        ArrayList<Point> points = polyline.getPoints();
        g.setStroke(polyline.getColor());
        double[] xPoints = new double[points.size()];
        double[] yPoints = new double[points.size()];
        for (int i = 0; i < points.size() - 1; i++) {
            Point p2 = points.get(i + 1);
            Point p1 = points.get(i);
            xPoints[i] = p1.x;
            yPoints[i] = p1.y;
            g.strokeLine(p1.x, p1.y, p2.x, p2.y);
        }
        if (polyline.getLast() != null) {
            g.strokeLine(points.get(points.size() - 1).x, points.get(points.size() - 1).y, polyline.getLast().x, polyline.getLast().y);
        } else if (polyline.isFill()) {
            g.setFill(polyline.getColor());
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
    public void drawing(SquiggleCommand squiggle) {
        ArrayList<Point> points = squiggle.getPoints();
        g.setStroke(squiggle.getColor());
        for (int i = 0; i < points.size() - 1; i++) {
            Point p1 = points.get(i);
            Point p2 = points.get(i + 1);
            g.strokeLine(p1.x, p1.y, p2.x, p2.y);
        }
    }

    @Override
    public void update(Observable o, Object arg) {
        this.setChanged();
        this.notifyObservers();
    }
}
