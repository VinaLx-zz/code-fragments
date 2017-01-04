import java.awt.*;
import javax.swing.*;
import java.awt.event.*;

public class ColorSwitchButton extends JButton {

    static private Color[] colors =
        new Color[] {
            Color.WHITE, Color.BLUE, Color.RED,
            Color.GREEN, Color.YELLOW, Color.PINK,
        };

    private int color_index = 0;
    private JComponent component_;

    public ColorSwitchButton(String text, JComponent component) {
        super(text);
        this.component_ = component;

        Action color_switch = new ColorSwitch();
        addActionListener(color_switch);
        getInputMap().put(KeyStroke.getKeyStroke("N"), "switch");
        getActionMap().put("switch", color_switch);
    }

    @Override
    public Dimension getPreferredSize() {
        return new Dimension(100, 50);
    }

    private class ColorSwitch extends AbstractAction {
        public void actionPerformed(ActionEvent event) {
            color_index = (1 + color_index) % colors.length;
            component_.setBackground(colors[color_index]);
        }
    }
}
