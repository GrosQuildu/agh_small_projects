import javax.swing.*;
import java.awt.*;
import java.util.ArrayList;

class Robot extends Pole {

	Robot(Swiat swiat, String id, int x, int y){
		super(swiat, id, "/robot.png", x, y);
	}

	public String toString(){
		return "R";
	}

	public double[] wezMoce(){
        ArrayList<Stacja> stacje = swiat.wezStacje();
		double[] moce = new double[stacje.size()];
		try {
			for (int stacjaID = 0; stacjaID < stacje.size(); stacjaID++) {
				moce[stacjaID] = this.swiat.podajMoc(stacje.get(stacjaID), x, y);
			}
		} catch(Exception e){
			System.out.println(e.getMessage());
		}
		return moce;
	}

	public Point wezPunkt(){
		return new Point(this.getX()+this.getWidth()/2, this.getY()+this.getHeight()/2);
	}
}
