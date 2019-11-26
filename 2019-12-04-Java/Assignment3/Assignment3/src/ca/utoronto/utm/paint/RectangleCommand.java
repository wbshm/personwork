package ca.utoronto.utm.paint;

import java.io.IOException;

public class RectangleCommand extends PaintCommand {
    private Point p1, p2;

    public RectangleCommand(Point p1, Point p2) {
        this.p1 = p1;
        this.p2 = p2;
        this.setChanged();
        this.notifyObservers();
    }

    public Point getP1() {
        return p1;
    }

    public void setP1(Point p1) {
        this.p1 = p1;
        this.setChanged();
        this.notifyObservers();
    }

    public Point getP2() {
        return p2;
    }

    public void setP2(Point p2) {
        this.p2 = p2;
        this.setChanged();
        this.notifyObservers();
    }

    public Point getTopLeft() {
        return new Point(Math.min(p1.x, p2.x), Math.min(p1.y, p2.y));
    }

    public Point getBottomRight() {
        return new Point(Math.max(p1.x, p2.x), Math.max(p1.y, p2.y));
    }

    public Point getDimensions() {
        Point tl = this.getTopLeft();
        Point br = this.getBottomRight();
        return (new Point(br.x - tl.x, br.y - tl.y));
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
        return "Rectangle\n" +
                "\tcolor:" + getColorString() + "\n" +
                "\tfilled:" + isFill() + "\n" +
                "\tp1:(" + p1.x + "," + p1.y + ")\n" +
                "\tp2:(" + p2.x + "," + p2.y + ")\n" +
                "End Rectangle\n";
    }
}
