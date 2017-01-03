import javax.swing.*;
import java.awt.*;
import java.awt.geom.*;

public class Main {
    public static void main(String[] argv) {
        EventQueue.invokeLater(() -> {
            DrawingFrame frame = new DrawingFrame();
            frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
            frame.setVisible(true);
        });
    }
}

class DrawingFrame extends JFrame {
    static final int DEFAULT_WIDTH;
    static final int DEFAULT_HEIGHT;

    static {
        Toolkit toolkit = Toolkit.getDefaultToolkit();
        Dimension screen_size = toolkit.getScreenSize();
        DEFAULT_WIDTH = screen_size.width / 6;
        DEFAULT_HEIGHT = screen_size.height / 3;
        System.out.printf(
            "Initializing Frame:\nScreen width: = %d, Screen height = %d\nDEFAULT_WIDTH = %d, DEFAULT_HEIGHT = %d\n",
            screen_size.width, screen_size.height, DEFAULT_WIDTH, DEFAULT_HEIGHT);
    }

    public DrawingFrame() {
        initTitle();
        initSize();
        initLocation();
        addCenteredShapes();
    }

    private void initTitle() {
        setTitle("Drawing Some Centered Shapes");
    }

    private void initSize() {
        setMinimumSize(new Dimension((int) (DEFAULT_WIDTH / 1.5), (int) (DEFAULT_HEIGHT / 1.5)));
        setSize(DEFAULT_WIDTH, DEFAULT_HEIGHT);
        // setResizable(true);
    }

    private void initLocation() {
        setLocationByPlatform(true);
    }

    private void addCenteredShapes() {
        getContentPane().add(new CenteredShapes(this, 300, 200));
        pack();
    }
}

class CenteredShapes extends JComponent {
    private Point top_left_corner_, bottom_right_corner_;

    private Dimension size_;

    private JFrame frame_;

    public CenteredShapes(JFrame frame, int width, int height) {
        size_ = new Dimension(width, height);
        frame_ = frame;
    }

    @Override
    public Dimension getPreferredSize() {
        return size_;
    }

    @Override
    public void paintComponent(Graphics graphics) {
        Graphics2D graphics_2d = (Graphics2D) graphics;
        calculatePosition();

        Rectangle2D rectangle = new Rectangle2D.Float();
        Ellipse2D ellipse = new Ellipse2D.Float();
        rectangle.setFrameFromDiagonal(top_left_corner_, bottom_right_corner_);
        ellipse.setFrameFromDiagonal(top_left_corner_, bottom_right_corner_);

        graphics_2d.draw(rectangle);
        graphics_2d.draw(ellipse);
    }

    private void calculatePosition() {
        Dimension frame_size = frame_.getSize();
        int x = (frame_size.width - size_.width) / 2,
            y = (frame_size.height - size_.height) / 2;
        x = Math.max(0, x);
        y = Math.max(0, y);
        top_left_corner_ = new Point(x, y);
        bottom_right_corner_ = new Point(x + size_.width, y + size_.height);
    }

}
