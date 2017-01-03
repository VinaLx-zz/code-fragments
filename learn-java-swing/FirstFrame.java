import javax.swing.*;
import java.awt.*;

public class FirstFrame extends JFrame {
    private static final int WIDTH = 500;
    private static final int HEIGHT = 300;

    private static final String ICON_PATH = "/home/xuemq12/Downloads/2.jpg";

    public FirstFrame() {
        initFrame();
        addComponents();
    }

    private void initFrame() {
        setMinimumSize(new Dimension(300, 200));
        // setResizable(false);
        // setLocation(0, 0);
        setTitle("FIRST FRAME");
        setIconImage(new ImageIcon(ICON_PATH).getImage());
    }

    private void addComponents() {
        getContentPane().add(new FirstComponent(WIDTH / 3, HEIGHT / 3));
        pack();
    }
}
