import javax.swing.*;
import java.awt.*;

/**
 * Created by gros on 23.01.17.
 */
public class SwiatDanePanel extends JPanel {
    private JLabel pozycja = new JLabel();
    public JLabel metody = new JLabel();
    public String danePozycja;

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);


        pozycja.setFont(new Font("Verdana",1,12));
        if(danePozycja != null) {
            pozycja.setText(danePozycja);
            add(pozycja, BorderLayout.EAST);
        } else
            remove(pozycja);
    }
}
