import javax.swing.*;
import java.awt.*;
import java.io.File;

class Pole extends JLabel{
	Swiat swiat;
	String id;
	private String icon = "";
	int x, y;

	Pole(Swiat swiat, String id, int x, int y){
		this.swiat = swiat;
		this.id = id;
		this.x = x;
		this.y= y;
	}

	public void setIconSize(int size){
        if(this.icon.isEmpty())
            return;
        ImageIcon imageIcon = new ImageIcon(getClass().getResource(this.icon));
        Image image = imageIcon.getImage();
        Image newimg = image.getScaledInstance(size, size, java.awt.Image.SCALE_SMOOTH);
        imageIcon = new ImageIcon(newimg);

        this.setIcon(imageIcon);
        this.setHorizontalAlignment(CENTER);
        this.setVerticalAlignment(CENTER);
        this.setMinimumSize(new Dimension(size, size));
        this.setMaximumSize(new Dimension(size, size));
        this.setPreferredSize(new Dimension(size, size));
    }

	Pole(Swiat swiat, String id, String icon, int x, int y){
		this.swiat = swiat;
		this.id = id;
		this.x = x;
		this.y= y;
        this.icon = icon;
	}
	
	public String toString(){
		return " ";
	}

	public double[] wezMoce(){
		return null;
	}

	Point wezPunkt(){
		return null;
	}
}
