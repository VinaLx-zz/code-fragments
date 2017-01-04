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
        setTitle("FIRST FRAME");
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
        Action color_switch = new ColorSwitch();
        color_button.addActionListener(color_switch);

        color_button.getInputMap().put(
                KeyStroke.getKeyStroke("N"), "switch color");
        color_button.getActionMap().put("switch color", color_switch);

        return color_button;
    }

    private class ColorSwitch extends AbstractAction {
        public void actionPerformed(ActionEvent event) {
            color_index = (1 + color_index) % colors.length;
            FirstFrame.this.setBackground(colors[color_index]);
        }
    }
}
