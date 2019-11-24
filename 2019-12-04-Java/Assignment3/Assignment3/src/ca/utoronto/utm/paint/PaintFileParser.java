package ca.utoronto.utm.paint;

import javafx.scene.paint.Color;

import java.io.BufferedReader;
import java.util.ArrayList;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * Parse a file in Version 1.0 PaintSaveFile format. An instance of this class
 * understands the paint save file format, storing information about
 * its effort to parse a file. After a successful parse, an instance
 * will have an ArrayList of PaintCommand suitable for rendering.
 * If there is an error in the parse, the instance stores information
 * about the error. For more on the format of Version 1.0 of the paint
 * save file format, see the associated documentation.
 *
 * @author
 */
public class PaintFileParser {
    /**
     * the current line being parsed
     */
    private int lineNumber = 0;
    /**
     * error encountered during parse
     */
    private String errorMessage = "";
    private PaintModel paintModel;

    private static final int CIRCLE = 2;
    private static final int RECTANGLE = 3;
    private static final int SQUIGGLE = 4;
    private static final int POLYLINE = 5;
    private static final int ENDFILE = 6;

    /**
     * Below are Patterns used in parsing
     */
    private Pattern pFileStart = Pattern.compile("^PaintSaveFileVersion1.0$");
    private Pattern pFileEnd = Pattern.compile("^EndPaintSaveFile$");

    private Pattern pPoint = Pattern.compile("^(center|p1|p2|point):\\((\\d+),(\\d+)\\)$");
    private Pattern pColor = Pattern.compile("^color:(\\d{1,3}),(\\d{1,3}),(\\d{1,3})$");
    private Pattern pFill = Pattern.compile("^filled:(false|true)$");

    /**
     * Circle
     */
    private Pattern pCircleStart = Pattern.compile("^Circle$");
    private Pattern pCircleRadius = Pattern.compile("^radius:(\\d+)$");
    private Pattern pCircleEnd = Pattern.compile("^EndCircle$");

    /**
     * Rectangle
     */
    private Pattern pRectangleStart = Pattern.compile("^Rectangle$");
    private Pattern pRectangleEnd = Pattern.compile("^EndRectangle$");

    /**
     * Squiggle
     */
    private Pattern pSquiggleStart = Pattern.compile("^Squiggle$");
    private Pattern pSquiggleEnd = Pattern.compile("^EndSquiggle$");

    /**
     * Polyline
     */
    private Pattern pPolylineStart = Pattern.compile("^Polyline$");
    private Pattern pPolylineEnd = Pattern.compile("^EndPolyline$");

    private ArrayList<Point> points = null;

    /**
     * Store an appropriate error message in this, including
     * lineNumber where the error occurred.
     *
     * @param mesg
     */
    private void error(String mesg) {
        this.errorMessage = "Error in line " + lineNumber + " " + mesg;
    }

    /**
     * @return the error message resulting from an unsuccessful parse
     */
    public String getErrorMessage() {
        return this.errorMessage;
    }

    /**
     * Parse the inputStream as a Paint Save File Format file.
     * The result of the parse is stored as an ArrayList of Paint command.
     * If the parse was not successful, this.errorMessage is appropriately
     * set, with a useful error message.
     *
     * @param inputStream the open file to parse
     * @param paintModel  the paint model to add the commands to
     * @return whether the complete file was successfully parsed
     */
    public boolean parse(BufferedReader inputStream, PaintModel paintModel) {
        this.paintModel = paintModel;
        this.errorMessage = "";

        // During the parse, we will be building one of the
        // following commands. As we parse the file, we modify
        // the appropriate command.
        PaintCommand paintCommand = null;

        try {
            int state = 0;
            Matcher m;
            String l;

            this.lineNumber = 0;
            int res = 1;
            while ((l = inputStream.readLine()) != null) {
                l = l.replaceAll("\\s+", "");
                this.lineNumber++;
                System.out.println(lineNumber + " " + l + " " + state);
                switch (state) {
                    case 0:
                        m = pFileStart.matcher(l);
                        if (m.matches()) {
                            state = 1;
                            break;
                        }
                        error("Expected Start of Paint Save File");
                        return false;
                    case 1:
                        state = -1;
                        if (pCircleStart.matcher(l).matches()) {
                            paintCommand = new CircleCommand(new Point(0, 0), 0);
                            state = CIRCLE;
                        } else if (pRectangleStart.matcher(l).matches()) {
                            paintCommand = new RectangleCommand(new Point(0, 0), new Point(0, 0));
                            state = RECTANGLE;
                        } else if (pSquiggleStart.matcher(l).matches()) {
                            paintCommand = new SquiggleCommand();
                            state = SQUIGGLE;
                        } else if (pPolylineStart.matcher(l).matches()) {
                            paintCommand = new PolylineCommand();
                            state = POLYLINE;
                        } else if (pFileEnd.matcher(l).matches()) {
                            state = ENDFILE;
                        }
                        break;
                    case CIRCLE:
                        res = parseCircle(l, (CircleCommand) paintCommand);
                        break;
                    case RECTANGLE:
                        res = parseRectangle(l, (RectangleCommand) paintCommand);
                        break;
                    case SQUIGGLE:
                        res = parseSquiggle(l, (SquiggleCommand) paintCommand);
                        break;
                    case POLYLINE:
                        res = parsePolyline(l, (PolylineCommand) paintCommand);
                        break;
                    case ENDFILE:
                        res = 1;
                        break;
                    default:
                        error("Nothing matched");
                        break;
                }
                if (res == 0) {
                    this.paintModel.addCommand(paintCommand);
                    state = 1;
                } else if (res == -1) {
                    //It's already gone wrong
                    return false;
                }
            }
        } catch (Exception e) {

        }
        return true;
    }

    /**
     * parse circle
     *
     * @param str    string input line str
     * @param circle CircleCommand
     * @return int 0 exist , -1 error , 1 continue
     */
    private int parseCircle(String str, CircleCommand circle) {
        Matcher m;
        int res = 1;
        if (str.contains("color:")) {
            Color color = getColor(str);
            if (color == null) {
                error("Failed to parse circle color");
                res = -1;
            }
            circle.setColor(color);
        } else if (str.contains("filled:")) {
            m = pFill.matcher(str);
            if (m.find()) {
                circle.setFill("true".equals(m.group(1)));
            } else {
                error("Failed to parse circle filled");
                res = -1;
            }
        } else if (str.contains("center:")) {
            m = pPoint.matcher(str);
            if (m.find()) {
                Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                circle.setCentre(point);
            } else {
                error("Failed to parse circle centre");
                res = -1;
            }
        } else if (str.contains("radius:")) {
            m = pCircleRadius.matcher(str);
            if (m.find()) {
                circle.setRadius(Integer.parseInt(m.group(1)));
            } else {
                error("Failed to parse circle radius");
                res = -1;
            }
        } else {
            m = pCircleEnd.matcher(str);
            res = m.matches() ? 0 : -1;
        }
        return res;
    }

    /**
     * parse rectangle
     *
     * @param str       string input line str
     * @param rectangle RectangleCommand
     * @return int 0 exist , -1 error , 1 continue
     */
    private int parseRectangle(String str, RectangleCommand rectangle) {
        int res = 1;
        Matcher m;
        if (str.contains("color:")) {
            Color color = getColor(str);
            if (color == null) {
                error("Failed to parse rectangle color");
                res = -1;
            }
            rectangle.setColor(color);
        } else if (str.contains("filled:")) {
            m = pFill.matcher(str);
            if (m.find()) {
                rectangle.setFill("true".equals(m.group(1)));
            } else {
                error("Failed to parse rectangle filled");
                res = -1;
            }
        } else if (str.contains("p1:")) {
            m = pPoint.matcher(str);
            if (m.find()) {
                Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                rectangle.setP1(point);
            } else {
                error("Failed to parse rectangle p1");
                res = -1;
            }
        } else if (str.contains("p2:")) {
            m = pPoint.matcher(str);
            if (m.find()) {
                Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                rectangle.setP2(point);
            } else {
                error("Failed to parse rectangle p2");
                res = -1;
            }
        } else {
            m = pRectangleEnd.matcher(str);
            res = m.matches() ? 0 : -1;
        }
        return res;
    }

    /**
     * parse squiggle
     * @param str       string input line str
     * @param squiggle SquiggleCommand
     * @return int 0 exist , -1 error , 1 continue
     */
    private int parseSquiggle(String str, SquiggleCommand squiggle) {
        Matcher m;
        int res = 1;
        if (str.contains("color:")) {
            Color color = getColor(str);
            if (color == null) {
                error("Failed to parse squiggle color");
                res = -1;
            }
            squiggle.setColor(color);
        } else if (str.contains("filled:")) {
            m = pFill.matcher(str);
            if (m.find()) {
                squiggle.setFill("true".equals(m.group(1)));
            } else {
                error("Failed to parse squiggle filled");
                res = -1;
            }
        } else if ("points".equals(str)) {
            if (this.points == null && squiggle.getPoints().size() == 0) {
                this.points = new ArrayList<>();
            } else {
                error("Failed to parse squiggle points");
                res = -1;
            }
        } else if (str.contains("point:")) {
            m = pPoint.matcher(str);
            if (m.find() && points != null) {
                Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                points.add(point);
            } else {
                error("Failed to parse squiggle point");
                res = -1;
            }
        } else if ("endpoints".equals(str)) {
            for (Point p : points) {
                squiggle.add(p);
            }
            points = null;
        } else {
            m = pSquiggleEnd.matcher(str);
            res = m.matches() ? 0 : -1;
        }
        return res;
    }

    /**
     * parse polyline
     * @param str string input line str
     * @param polyline PolylineCommand
     * @return int 0 exist , -1 error , 1 continue
     */
    private int parsePolyline(String str, PolylineCommand polyline) {
        Matcher m;
        int res = 1;
        if (str.contains("color:")) {
            Color color = getColor(str);
            if (color == null) {
                error("Failed to parse polyline color");
                res = -1;
            }
            polyline.setColor(color);
        } else if (str.contains("filled:")) {
            m = pFill.matcher(str);
            if (m.find()) {
                polyline.setFill("true".equals(m.group(1)));
            } else {
                error("Failed to parse polyline filled");
                res = -1;
            }
        } else if ("points".equals(str)) {
            if (this.points == null && polyline.getPoints().size() == 0) {
                this.points = new ArrayList<>();
            } else {
                error("Failed to parse polyline points");
                res = -1;
            }
        } else if (str.contains("point:")) {
            m = pPoint.matcher(str);
            if (m.find() && points != null) {
                Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                points.add(point);
            } else {
                error("Failed to parse polyline point");
                res = -1;
            }
        } else if ("endpoints".equals(str)) {
            for (Point p : points) {
                polyline.add(p);
            }
            points = null;
        } else {
            m = pPolylineEnd.matcher(str);
            res = m.matches() ? 0 : -1;
        }
        return res;
    }

    /**
     * get color by string
     * @param str string
     * @return Color
     */
    private Color getColor(String str) {
        Matcher m = pColor.matcher(str);
        if (m.find()) {
            int r = Integer.parseInt(m.group(1));
            int g = Integer.parseInt(m.group(2));
            int b = Integer.parseInt(m.group(3));
            if (Math.max(Math.max(r, g), b) > 256) {
                return null;
            }
            return Color.rgb(r, g, b);
        } else {
            return null;
        }
    }
}
