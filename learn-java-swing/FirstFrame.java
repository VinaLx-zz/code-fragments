import java.net.URL;
import javax.swing.*;
import java.awt.*;

public class FirstFrame extends JFrame {
    private static final int WIDTH = 500;
    private static final int HEIGHT = 300;

    public FirstFrame() {
        initFrame();
        addComponents();
    }

    private void initFrame() {
        setMinimumSize(new Dimension(300, 200));
        // setResizable(false);
        // setLocation(0, 0);
        setTitle("FIRST FRAME");
        // setIconImage(new ImageIcon(ICON_URL).getImage());
    }

    private void addComponents() {
        add(new CenteredText());
        pack();
    }
}
