import java.awt.event.*;
import javax.swing.*;
import java.awt.*;

public class FirstFrame extends JFrame {
    private static final int WIDTH = 500;
    private static final int HEIGHT = 300;

    private CenteredText text_panel_;


    public FirstFrame() {
        initFrame();
        addComponents();
    }

    private void initFrame() {
        setMinimumSize(new Dimension(300, 200));
        setTitle("FIRST FRAME");
    }

    private void addComponents() {
        text_panel_ = new CenteredText();
        // setLayout(null);
        // text_panel_.setBounds(100, 100, 500, 500);
        add(text_panel_);
        pack();
    }


}
