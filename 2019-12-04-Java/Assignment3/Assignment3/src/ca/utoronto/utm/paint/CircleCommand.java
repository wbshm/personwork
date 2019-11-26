package ca.utoronto.utm.paint;

import java.io.IOException;

public class CircleCommand extends PaintCommand {
    private Point centre;
    private int radius;

    public CircleCommand(Point centre, int radius) {
        this.centre = centre;
        this.radius = radius;
    }

    public Point getCentre() {
        return centre;
    }

    public void setCentre(Point centre) {
        this.centre = centre;
        this.setChanged();
        this.notifyObservers();
    }

    public int getRadius() {
        return radius;
    }

    public void setRadius(int radius) {
        this.radius = radius;
        this.setChanged();
        this.notifyObservers();
    }


    @Override
    public void accept(DrawVisitor drawVisitor) {
        drawVisitor.drawing(this);
    }

    @Override
    public void save(SaveVisitor saveVisitor) throws IOException {
        saveVisitor.save(this);
    }

    @Override
    public String toString() {
        return "Circle\n" +
                "\tcolor:" + getColorString() + "\n" +
                "\tfilled:" + isFill() + "\n" +
                "\tcenter:(" + centre.x + "," + centre.y + ")\n" +
                "\tradius:" + getRadius() + "\n" +
                "End Circle\n";
    }
}
