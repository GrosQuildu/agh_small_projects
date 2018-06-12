import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;


public class StworzGraficzny extends JFrame {
    private JLabel jlabel = new JLabel();
    public String daneTekst;
    private SwiatDanePanel dane;

    public StworzGraficzny(Swiat swiat) {
        this.setDefaultCloseOperation(JFrame.HIDE_ON_CLOSE);
        dane = new SwiatDanePanel();
        SwiatPanel panel = new SwiatPanel(swiat, dane);

        dane.setSize(new Dimension(panel.getWidth(), 30));
        dane.setLayout(new FlowLayout(FlowLayout.LEFT));
        JLabel jlabel = new JLabel("Metody: ");
        jlabel.setFont(new Font("Verdana",1,12));
        dane.add(jlabel);

        add(dane, BorderLayout.NORTH);
        JScrollPane scrPane = new JScrollPane(panel);
        add(scrPane, BorderLayout.CENTER);

        setTitle("Świat");
        setMinimumSize(new Dimension(400, 400));
        setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        setLocationRelativeTo(null);


        //zmiana rozmiaru, zachowiaj ratio
//        addComponentListener(new ComponentAdapter() {
//            @Override
//            public void componentResized(ComponentEvent e) {
//                setSize(new Dimension(getWidth(), (int)(panel.wezRatio()*getWidth())));
//                super.componentResized(e);
//            }
//        });
    }

    public void edytujMetodyTekst(String metody){
        dane.metody.setFont(new Font("Verdana",1,12));
        dane.metody.setText(metody);
        dane.add(dane.metody, BorderLayout.EAST);
        dane.repaint();
    }

}
