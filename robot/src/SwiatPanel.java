import javax.swing.*;
import javax.swing.border.CompoundBorder;
import java.awt.*;
import java.awt.event.MouseAdapter;


class SwiatPanel extends JPanel {
    private Point zaznaczonePolePunkt;
    private double zaznaczonePoleMoce[];
    private Pole zaznaczonaStacja;
    private Swiat swiat;
    private double ratio;
    private JLabel pozycjaLabel;

    public double wezRatio(){ return this.ratio; }

    SwiatPanel(Swiat swiat, SwiatDanePanel dane){
        this.swiat = swiat;

        //oblicz rozmiary
        int sizePanelSzerokosc = swiat.szerokosc();
        int sizePanelWysokosc = swiat.wysokosc();

        //size - wielkosc pola, dla graficznych rzeczy
        int size = 32;
        if(swiat.szerokosc() >= 30 || swiat.wysokosc() >= 30)
            size = 16;

        sizePanelSzerokosc *= size;
        sizePanelWysokosc *= size;
        this.ratio = (double)sizePanelWysokosc/sizePanelSzerokosc;

        if(sizePanelSzerokosc < 900) {
            this.setSize(new Dimension(sizePanelSzerokosc, sizePanelWysokosc));
            setLayout(new GridLayout(swiat.wysokosc(), swiat.szerokosc(), 2, 2)); // rows, cols, space, space
        } else{
            this.setSize(new Dimension(900, (int)(900*ratio)));
            setLayout(new GridLayout(swiat.wysokosc(), swiat.szerokosc(), 0, 0)); // rows, cols, space, space
        }

        for(int y=0; y<swiat.wysokosc(); y++){
            for(int x=0; x<swiat.szerokosc(); x++){
                swiat.pole(x, y).setIconSize(size);
                setComponentZOrder(swiat.pole(x, y), 0);
                Pole pole = swiat.pole(x, y);
                if(sizePanelSzerokosc < 900)
                    pole.setBorder(BorderFactory.createDashedBorder(null, 5, 5));

                pole.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mouseEntered(java.awt.event.MouseEvent evt) {


                        if(pole instanceof Robot || pole instanceof Gwiazda) {
                            zaznaczonePoleMoce = pole.wezMoce();
                            zaznaczonePolePunkt = pole.wezPunkt();
                            repaint();
                        } else if(pole instanceof Stacja){
                            zaznaczonaStacja = pole;
                            repaint();
                        }
                        dane.danePozycja = "Pozycja: x="+pole.x+", y="+pole.y;
                        dane.repaint();
                    }

                    @Override
                    public void mouseExited(java.awt.event.MouseEvent evt) {
                        zaznaczonePoleMoce = null;
                        zaznaczonePolePunkt = null;
                        zaznaczonaStacja = null;
                        dane.danePozycja = null;
                        dane.repaint();
                        repaint();
                    }
                });

                add(pole);
            }
        }

        setBorder(new CompoundBorder(
                BorderFactory.createMatteBorder(3,3,3,3, Color.BLACK),
                BorderFactory.createEmptyBorder(3,3,3,3)));
    }

    @Override
    protected void paintComponent(Graphics g) {
        super.paintComponent(g);

        Color kolory[] = {Color.red, Color.cyan, Color.green};

        if(zaznaczonePolePunkt != null){
            // narysuj linie miedzy polem a stacjami
            Point pozycjeStacji[] = swiat.wezPozycjeStacji();
            Graphics2D g2d = (Graphics2D) g;
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2d.setStroke(new BasicStroke(3, BasicStroke.CAP_ROUND, BasicStroke.JOIN_BEVEL));

            for(int stacjaID = 0; stacjaID < pozycjeStacji.length; stacjaID++){
                g2d.setColor(kolory[stacjaID % 3]);
                g.drawLine(zaznaczonePolePunkt.x, zaznaczonePolePunkt.y, pozycjeStacji[stacjaID].x, pozycjeStacji[stacjaID].y);

                g2d.setColor(Color.black);
                g2d.setFont(new Font("TimesRoman", Font.BOLD, 17));
                g.drawString(String.format("%.3f", zaznaczonePoleMoce[stacjaID]), pozycjeStacji[stacjaID].x, pozycjeStacji[stacjaID].y+20);
            }

        } else if(zaznaczonaStacja != null){
            //pokoloruj pola zgodnie z moca
            int stacjaID = Integer.parseInt(zaznaczonaStacja.id.substring(1));
            int red = kolory[stacjaID%3].getRed();
            int green = kolory[stacjaID%3].getGreen();
            int blue = kolory[stacjaID%3].getBlue();
            for(int y=0; y<swiat.wysokosc(); y++) {
                for (int x = 0; x < swiat.szerokosc(); x++) {
                    int alpha = 255;
                    try {
                        alpha = (int)(255.0/swiat.maxMoc * swiat.podajMoc(zaznaczonaStacja, swiat.pole(x, y)));
                    } catch (Exception e){
                        System.out.println("Błąd podczas ustawiania kolorów pól");
                    }

                    swiat.pole(x, y).setBackground(new Color(red, green, blue, alpha));
                    swiat.pole(x, y).setOpaque(true);
                }
            }
            swiat.pole(swiat.wezRobot().x, swiat.wezRobot().y).setBorder(BorderFactory.createLineBorder(Color.black, 2, true));

            //zaznacz linie miedzy stacjami
            Point pozycjeStacji[] = swiat.wezPozycjeStacji();

            Graphics2D g2d = (Graphics2D) g;
            g2d.setColor(Color.black);
            g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
            g2d.setStroke(new BasicStroke(3, BasicStroke.JOIN_ROUND, BasicStroke.JOIN_BEVEL));
            for(stacjaID=0; stacjaID < swiat.iloscStacji(); stacjaID++){
                g.drawLine(pozycjeStacji[stacjaID].x, pozycjeStacji[stacjaID].y,
                        pozycjeStacji[(stacjaID+1)%swiat.iloscStacji()].x, pozycjeStacji[(stacjaID+1)%swiat.iloscStacji()].y);
            }
        } else{
//             wyczysc wszystko
            for(int y=0; y<swiat.wysokosc(); y++) {
                for (int x = 0; x < swiat.szerokosc(); x++) {
                    swiat.pole(x, y).setOpaque(false);
                }
            }
            swiat.pole(swiat.wezRobot().x, swiat.wezRobot().y).setBorder(BorderFactory.createDashedBorder(null, 5, 5));
        }
    }
}
