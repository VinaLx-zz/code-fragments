import java.awt.geom.*;
import java.awt.font.*;
import javax.swing.*;
import java.awt.*;

public class CenteredText extends JComponent {
    public static final String DEFAULT_TEXT = "Hello Swing";
    public static final Dimension DEFAULT_SIZE = new Dimension(300, 200);

    private Graphics2D graphic;
    private String text_;

    public CenteredText() {
        text_ = DEFAULT_TEXT;
    }

    public CenteredText(String text) {
        text_ = text;
    }

    @Override
    public void paintComponent(Graphics graphic) {
        this.graphic = (Graphics2D) graphic;

        Font font = createFont(Font.PLAIN, 50);
        drawCenteredString(DEFAULT_TEXT, font);
    }

    private void drawCenteredString(String text, Font font) {
        graphic.setFont(font);

        // calculate where the baseline of the string should be
        Point2D location = getCenterLocation(DEFAULT_TEXT, font);

        drawText(text, location);
    }
    
    private Font createFont(int style, int size) {
        return new Font("Ubuntu Mono", style, size);
    }

    private Point2D getCenterLocation(String text, Font font) {

        Rectangle2D bound = getTextBound(text, font);

        // the graphic draw the string according to the baseline of string
        // so then calculate where the baseline should be
        // note: text_bound.getY() yield the ascent(minus) of the text

        double location_x = (getWidth() - bound.getWidth()) / 2,
               location_y = (getHeight() - bound.getHeight()) / 2 - bound.getY();

        return new Point2D.Double(location_x, location_y);
    }

    private Rectangle2D getTextBound(String text, Font font) {
        return font.getStringBounds(text, graphic.getFontRenderContext());
    }

    private void drawText(String text, Point2D location) {
        graphic.drawString(
                text, (float)location.getX(), (float)location.getY());
    }


    @Override
    public Dimension getPreferredSize() {
        return DEFAULT_SIZE;
    }
}
