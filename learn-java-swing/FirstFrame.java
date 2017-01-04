import java.awt.event.*;
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
        add(createColorSwitchButton());
        add(new CenteredText());
        pack();
    }

    static private Color[] colors = new Color[] {
        Color.WHITE, Color.BLUE, Color.RED, Color.GREEN, Color.YELLOW, Color.PINK
    };

    private int color_index = 0;

    private JButton createColorSwitchButton() {
        JButton color_button = new JButton("switch");
        color_button.setSize(100, 50);
        color_button.addActionListener(new ColorSwitch());
        return color_button;
    }

    private class ColorSwitch implements ActionListener {
        public void actionPerformed(ActionEvent event) {
            color_index = (1 + color_index) % colors.length;
            FirstFrame.this.setBackground(colors[color_index]);
        }
    }
}
