import java.awt.event.*;
import java.awt.geom.*;
import java.awt.font.*;
import javax.swing.*;
import java.awt.*;

public class CenteredText extends JPanel {
    public static final String DEFAULT_TEXT = "Hello Swing";
    public static final Dimension DEFAULT_SIZE = new Dimension(300, 200);

    private Graphics2D graphic;
    private String text_;
    private Point2D current_pos_;
    private boolean center_enabled = true;

    {
        initAction();
    }

    public CenteredText() {
        text_ = DEFAULT_TEXT;
    }

    public CenteredText(String text) {
        text_ = text;
    }

    @Override
    public void paintComponent(Graphics graphic) {
        super.paintComponent(graphic);
        this.graphic = (Graphics2D) graphic;
        Font font = createFont(Font.PLAIN, 50);
        drawCenteredString(DEFAULT_TEXT, font);
    }

    private void drawCenteredString(String text, Font font) {
        graphic.setFont(font);
        if (center_enabled) {
            // calculate where the baseline of the string should be
            updatePosition(DEFAULT_TEXT, font);
        }
        drawText(text);
    }
    
    private Font createFont(int style, int size) {
        return new Font("Ubuntu Mono", style, size);
    }

    private void updatePosition(String text, Font font) {
        Rectangle2D bound = getTextBound(text, font);
        // the graphic draw the string according to the baseline of string
        // so then calculate where the baseline should be
        // note: text_bound.getY() yield the ascent(minus) of the text
        double location_x = (getWidth() - bound.getWidth()) / 2,
               location_y = (getHeight() - bound.getHeight()) / 2 - bound.getY();
        current_pos_ = new Point2D.Double(location_x, location_y);
    }

    private Rectangle2D getTextBound(String text, Font font) {
        return font.getStringBounds(text, graphic.getFontRenderContext());
    }

    private void drawText(String text) {
        graphic.drawString(
                text, (float)current_pos_.getX(), (float)current_pos_.getY());
    }

    private void initAction() {
        Action change_pos = new ChangePosition();
        getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke("L"), "goleft");
        getActionMap().put("goleft", change_pos);
    }


    @Override
    public Dimension getPreferredSize() {
        return DEFAULT_SIZE;
    }

    class ChangePosition extends AbstractAction {
        public void actionPerformed(ActionEvent event) {
            Point2D new_pos = new Point2D.Double(
                    current_pos_.getX() + 10, current_pos_.getY());
            current_pos_ = new_pos;
            center_enabled = false;
            repaint();
        }
    }
}
