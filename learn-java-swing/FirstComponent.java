import java.awt.geom.*;
import java.awt.font.*;
import javax.swing.*;
import java.awt.*;

public class FirstComponent extends JComponent {
    public static final String TEXT = "Hello Swing";
    public static final Dimension DEFAULT_SIZE = new Dimension(300, 200);

    private final Dimension text_location_;

    public FirstComponent(int location_x, int location_y) {
        this.text_location_ = new Dimension(location_x, location_y);
        System.out.printf("Component width: %d, height: %d\n", getWidth(), getHeight());
    }

    @Override
    public void paintComponent(Graphics graphic) {
        // System.out.printf("Component width: %d, height: %d\n", getWidth(), getHeight());
        
        Graphics2D graphic_2d = (Graphics2D)graphic;
        
        Font font = new Font("Ubuntu Mono", 0, 50);
        FontRenderContext font_context = graphic_2d.getFontRenderContext();
        Rectangle2D text_bound = font.getStringBounds(TEXT, font_context);

        // System.out.printf("bound x: %.0f, y: %.0f, width: %.0f, height: %.0f\n",
                // text_bound.getX(), text_bound.getY(),
                // text_bound.getWidth(), text_bound.getHeight());

        // calculate where the upper-left corner of the string should be
        double location_x = (getWidth() - text_bound.getWidth()) / 2,
               location_y = (getHeight() - text_bound.getHeight()) / 2;

        // the graphic draw the string according to the baseline of string
        // so then calculate where the baseline should be
        // note: text_bound.getY() yield the ascent(minus) of the text
        double baseline_y = location_y - text_bound.getY();
        System.out.printf("location_y: %.0f, baseline: %.0f\n", location_y, baseline_y);

        // then draw all the things up
        graphic_2d.setFont(font);
        graphic_2d.drawString(TEXT, (float)location_x, (float)baseline_y);
        graphic_2d.draw(
                new Rectangle2D.Double(
                    location_x, location_y,
                    text_bound.getWidth(), text_bound.getHeight()));

        graphic_2d.setPaint(Color.BLUE);
        graphic_2d.draw(
                new Line2D.Double(
                    location_x, baseline_y,
                    location_x + text_bound.getWidth(), baseline_y));
    }

    @Override
    public Dimension getPreferredSize() {
        return DEFAULT_SIZE;
    }
}
