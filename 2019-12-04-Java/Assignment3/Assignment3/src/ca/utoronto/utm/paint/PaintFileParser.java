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

    private String nextOrder = "color";
    private static final String colorStr = "color";
    private static final String fillStr = "filled";
    private static final String endStr = "end";

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
        ArrayList<PaintCommand> commands = new ArrayList<>();

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
                        res = -1;
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
                        } else if ("".equals(l)) {
                            state = 1;
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
                        if ("".equals(l)) {
                            res = 1;
                        } else {
                            error("File end is wrong");
                            res = -1;
                        }
                        break;
                    default:
                        error("Nothing matched");
                        res = -1;
                        break;
                }
                if (res == 0) {
                    commands.add(paintCommand);
                    res = 1;
                    state = 1;
                } else if (res == -1) {
                    //It's already gone wrong
                    System.out.println(getErrorMessage());
                    return false;
                }
            }
        } catch (Exception e) {
            System.out.println("Fatal error");
        }
        for (PaintCommand paint : commands) {
            this.paintModel.addCommand(paint);
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
        final String centerStr = "center";
        final String radiusStr = "radius";
        int res = 1;
        Matcher m;
        switch (nextOrder) {
            case colorStr:
                res = setColor(str, circle);
                break;
            case fillStr:
                m = pFill.matcher(str);
                if (m.find()) {
                    circle.setFill("true".equals(m.group(1)));
                } else {
                    error("Failed to parse circle filled");
                    res = -1;
                }
                break;
            case centerStr:
                m = pPoint.matcher(str);
                if (m.find()) {
                    Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                    circle.setCentre(point);
                } else {
                    error("Failed to parse circle centre");
                    res = -1;
                }
                break;
            case radiusStr:
                m = pCircleRadius.matcher(str);
                if (m.find()) {
                    circle.setRadius(Integer.parseInt(m.group(1)));
                } else {
                    error("Failed to parse circle radius");
                    res = -1;
                }
                break;
            case endStr:
                m = pCircleEnd.matcher(str);
                res = 0;
                if (!m.matches()) {
                    error("Failed to parse circle end");
                    res = -1;
                }
                break;
            default:
                error("Failed to parse circle");
                res = -1;
        }
        if (res != -1) {
            String[] strArr = {colorStr, fillStr, centerStr, radiusStr, endStr};
            setNextOrder(strArr);
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
        final String p1Str = "p1";
        final String p2Str = "p2";
        switch (nextOrder) {
            case colorStr:
                res = setColor(str, rectangle);
                break;
            case fillStr:
                m = pFill.matcher(str);
                if (m.find()) {
                    rectangle.setFill("true".equals(m.group(1)));
                } else {
                    error("Failed to parse rectangle filled");
                    res = -1;
                }
                break;
            case p1Str:
                m = pPoint.matcher(str);
                if (m.find() && str.contains(p1Str)) {
                    Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                    rectangle.setP1(point);
                } else {
                    error("Failed to parse rectangle p1");
                    res = -1;
                }
                break;
            case p2Str:
                m = pPoint.matcher(str);
                if (m.find() && str.contains(p2Str)) {
                    Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                    rectangle.setP2(point);
                } else {
                    error("Failed to parse rectangle p2");
                    res = -1;
                }
                break;
            case endStr:
                m = pRectangleEnd.matcher(str);
                res = 0;
                if (!m.matches()) {
                    error("Failed to parse rectangle end");
                    res = -1;
                }
                break;
            default:
                error("Failed to parse rectangle");
                res = -1;
        }
        if (res != -1) {
            String[] strArr = {colorStr, fillStr, p1Str, p2Str, endStr};
            setNextOrder(strArr);
        }
        return res;
    }

    /**
     * parse squiggle
     *
     * @param str      string input line str
     * @param squiggle SquiggleCommand
     * @return int 0 exist , -1 error , 1 continue
     */
    private int parseSquiggle(String str, SquiggleCommand squiggle) {

        int res = 1;
        Matcher m;
        final String pointsStr = "points";
        final String pointStr = "point";
        final String endPointStr = "endpoints";
        switch (nextOrder) {
            case colorStr:
                res = setColor(str, squiggle);
                break;
            case fillStr:
                m = pFill.matcher(str);
                if (m.find()) {
                    squiggle.setFill("true".equals(m.group(1)));
                } else {
                    error("Failed to parse squiggle filled");
                    res = -1;
                }
                break;
            case pointsStr:
                if (this.points == null && squiggle.getPoints().size() == 0) {
                    this.points = new ArrayList<>();
                } else {
                    error("Failed to parse squiggle points");
                    res = -1;
                }
                break;
            case pointStr:
                m = pPoint.matcher(str);
                if (m.find() && points != null) {
                    Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                    points.add(point);
                    nextOrder = pointsStr;
                } else if (endPointStr.equals(str)) {
                    for (Point p : points) {
                        squiggle.add(p);
                    }
                    nextOrder = endPointStr;
                    points = null;
                } else {
                    error("Failed to parse squiggle point:" + str);
                    res = -1;
                }
                break;
            case endStr:
                m = pSquiggleEnd.matcher(str);
                res = 0;
                if (!m.matches()) {
                    error("Failed to parse squiggle end");
                    res = -1;
                }
                break;
            default:
                error("Failed to parse squiggle");
                res = -1;
        }
        if (res != -1) {
            String[] strArr = {colorStr, fillStr, pointsStr, pointStr, endPointStr, endStr};
            setNextOrder(strArr);
        }
        return res;
    }

    /**
     * parse polyline
     *
     * @param str      string input line str
     * @param polyline PolylineCommand
     * @return int 0 exist , -1 error , 1 continue
     */
    private int parsePolyline(String str, PolylineCommand polyline) {
        int res = 1;
        Matcher m;
        final String pointsStr = "points";
        final String pointStr = "point";
        final String endPointStr = "endpoints";
        switch (nextOrder) {
            case colorStr:
                res = setColor(str, polyline);
                break;
            case fillStr:
                m = pFill.matcher(str);
                if (m.find()) {
                    polyline.setFill("true".equals(m.group(1)));
                } else {
                    error("Failed to parse polyline filled");
                    res = -1;
                }
                break;
            case pointsStr:
                if (this.points == null && polyline.getPoints().size() == 0) {
                    this.points = new ArrayList<>();
                } else {
                    error("Failed to parse polyline points");
                    res = -1;
                }
                break;
            case pointStr:
                m = pPoint.matcher(str);
                if (m.find() && points != null) {
                    Point point = new Point(Integer.parseInt(m.group(2)), Integer.parseInt(m.group(3)));
                    points.add(point);
                    nextOrder = pointsStr;
                } else if (endPointStr.equals(str)) {
                    for (Point p : points) {
                        polyline.add(p);
                    }
                    nextOrder = endPointStr;
                    points = null;
                } else {
                    error("Failed to parse polyline point:" + str);
                    res = -1;
                }
                break;
            case endStr:
                m = pPolylineEnd.matcher(str);
                res = 0;
                if (!m.matches()) {
                    error("Failed to parse polyline end");
                    res = -1;
                }
                break;
            default:
                error("Failed to parse squiggle");
                res = -1;
        }
        if (res != -1) {
            String[] strArr = {colorStr, fillStr, pointsStr, pointStr, endPointStr, endStr};
            setNextOrder(strArr);
        }
        return res;
    }

    /**
     * get color by string
     *
     * @param str string
     * @return Color
     */
    private Color getColor(String str) {
        Matcher m = pColor.matcher(str);
        if (m.find()) {
            int r = Integer.parseInt(m.group(1));
            int g = Integer.parseInt(m.group(2));
            int b = Integer.parseInt(m.group(3));
            if (Math.max(Math.max(r, g), b) > 255) {
                return null;
            }
            return Color.rgb(r, g, b);
        } else {
            return null;
        }
    }

    /**
     * set the paintCommand color
     *
     * @param str          color string
     * @param paintCommand object
     * @return 1 success,-1 error
     */
    private int setColor(String str, PaintCommand paintCommand) {
        Color color = getColor(str);
        int res = 1;
        if (color == null) {
            error("Failed to parse color");
            res = -1;
        }
        paintCommand.setColor(color);
        return res;
    }

    /**
     * set the next string
     *
     * @param strArr orderList
     */
    private void setNextOrder(String[] strArr) {
        for (int i = 0; i < strArr.length; i++) {
            if (nextOrder.equals(strArr[i])) {
                int index = (i + 1) % strArr.length;
                nextOrder = strArr[index];
                break;
            }
        }
    }
}
