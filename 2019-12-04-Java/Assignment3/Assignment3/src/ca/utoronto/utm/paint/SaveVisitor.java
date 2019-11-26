package ca.utoronto.utm.paint;

import java.io.IOException;

interface SaveVisitor {

    void save(CircleCommand circleCommand) throws IOException;

    void save(RectangleCommand rectangleCommand) throws IOException;

    void save(PolylineCommand polylineCommand) throws IOException;

    void save(SquiggleCommand squiggleCommand) throws IOException;
}
