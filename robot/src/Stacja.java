import javax.swing.*;
import java.awt.*;

class Stacja extends Pole {

	Stacja(Swiat swiat, String id, int x, int y){
		super(swiat, id, "/stacja.png", x, y);
	}
	
	public String toString(){
		return "S";
	}

	Point wezPunkt(){
		return new Point(this.getX()+this.getWidth()/2, this.getY()+this.getHeight()/2);
	}
}
