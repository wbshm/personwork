package ca.utoronto.utm.paint;

import javafx.scene.paint.Color;

import java.util.Observable;

public abstract class PaintCommand extends Observable {
    private Color color;
    private boolean fill;

    PaintCommand() {
        // Pick a random color for this
        int r = (int) (Math.random() * 256);
        int g = (int) (Math.random() * 256);
        int b = (int) (Math.random() * 256);
        this.color = Color.rgb(r, g, b);

        this.fill = (1 == (int) (Math.random() * 2));
    }

    public Color getColor() {
        return color;
    }

    public void setColor(Color color) {
        this.color = color;
    }

    public boolean isFill() {
        return fill;
    }

    public void setFill(boolean fill) {
        this.fill = fill;
    }

    @Override
    public String toString() {
        double r = this.color.getRed();
        double g = this.color.getGreen();
        double b = this.color.getBlue();

        String s = "";
        s += "\tcolor:" + r + "," + g + "," + b + "\n";
        s += "\tfilled:" + this.fill + "\n";
        return s;
    }

    public String getColorString() {
        int red = (int) (color.getRed() * 256);
        int green = (int) (color.getGreen() * 256);
        int blue = (int) (color.getBlue() * 256);
        return red + "," + green + "," + blue;
    }

    public abstract void accept(DrawVisitor drawVisitor);
}
