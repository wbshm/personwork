package ca.utoronto.utm.paint;

public class ShapeManipulatorFactory {
    public static ShapeManipulatorStrategy create(String strategyName, PaintModel paintModel) {
        ShapeManipulatorStrategy strategy = null;
        if ("Circle".equals(strategyName)) {
            strategy = new CircleManipulatorStrategy(paintModel);
        } else if ("Squiggle".equals(strategyName)) {
            strategy = new SquiggleManipulatorStrategy(paintModel);
        } else if ("Rectangle".equals(strategyName)) {
            strategy = new RectangleManipulatorStrategy(paintModel);
        } else if ("Polyline".equals(strategyName)) {
            strategy = new PolylineManipulatorStrategy(paintModel);
        }
        return strategy;
    }
}
