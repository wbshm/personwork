package ca.utoronto.utm.paint;

import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;

public class PolylineManipulatorStrategy extends ShapeManipulatorStrategy {

    private PolylineCommand polylineCommand;

    PolylineManipulatorStrategy(PaintModel paintModel) {
        super(paintModel);
    }

    @Override
    public void mouseMoved(MouseEvent e) {
        if (null != polylineCommand) {
            polylineCommand.setLast(new Point((int) e.getX(), (int) e.getY()));
        }
    }

    @Override
    public void mouseClicked(MouseEvent e) {
        Point point = new Point((int) e.getX(), (int) e.getY());
        if (e.getButton() == MouseButton.PRIMARY) {
            addPoint(point);
        } else if (e.getButton() == MouseButton.SECONDARY) {
            polylineCommand = null;
        }
    }

    private void addPoint(Point point) {
        if (null == polylineCommand) {
            polylineCommand = new PolylineCommand(point);
            this.addCommand(polylineCommand);
        } else {
            polylineCommand.add(point);
        }
    }
}
