import javax.swing.*;
import java.awt.*;

public class FirstComponent extends JComponent {
    public static final String TEXT = "Hello Swing";
    public static final Dimension DEFAULT_SIZE = new Dimension(300, 200);

    private final Dimension text_location_;

    public FirstComponent(int location_x, int location_y) {
        this.text_location_ = new Dimension(location_x, location_y);
    }

    @Override
    public void paintComponent(Graphics graphic) {
        graphic.drawString(TEXT, text_location_.width, text_location_.height);
    }

    @Override
    public Dimension getPreferredSize() {
        return DEFAULT_SIZE;
    }
}
