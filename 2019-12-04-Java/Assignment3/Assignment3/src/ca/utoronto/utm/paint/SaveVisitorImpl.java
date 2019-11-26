package ca.utoronto.utm.paint;

import java.io.IOException;
import java.io.Writer;

public class SaveVisitorImpl implements SaveVisitor {
    private Writer writer;

    SaveVisitorImpl(Writer writer) {
        this.writer = writer;
    }

    @Override
    public void save(CircleCommand circleCommand) throws IOException {
        this.writer.append(circleCommand.toString());
    }

    @Override
    public void save(RectangleCommand rectangleCommand) throws IOException {
        this.writer.append(rectangleCommand.toString());
    }

    @Override
    public void save(PolylineCommand polylineCommand) throws IOException {
        this.writer.append(polylineCommand.toString());
    }

    @Override
    public void save(SquiggleCommand squiggleCommand) throws IOException {
        this.writer.append(squiggleCommand.toString());
    }
}
