package ca.utoronto.utm.paint;

public interface DrawVisitor {


    /**
     * paint circle
     *
     * @param circle circle
     */
    void drawing(CircleCommand circle);

    /**
     * paint rectangle
     *
     * @param rectangle rectangle
     */
    void drawing(RectangleCommand rectangle);

    /**
     * paint polyline
     *
     * @param polyline polyline
     */
    void drawing(PolylineCommand polyline);

    /**
     * paint squiggle
     *
     * @param squiggle squiggle
     */
    void drawing(SquiggleCommand squiggle);
}
