import java.awt.*;
import java.util.ArrayList;

/**
 * Created by gros on 23.01.17.
 */

public class Metody {
    public static boolean apit(Swiat swiat){
        //znajdz co najmniej trzy gwiazdy
        //dla szybszego działania nie pytamy wszystkich gwiazd, tylko od razu biezemy najblizsze
        ArrayList<Stacja> stacje = swiat.wezStacje();
        Robot robot = swiat.wezRobot();
        double moceOdStacji[] = robot.wezMoce();
        ArrayList<Gwiazda> bliskieGwiazdy = new ArrayList<Gwiazda>();

        int odleglosc = 1; // liczona w polach
        int maxOdleglosc = 5;

        while(bliskieGwiazdy.size() < 3 && odleglosc <= maxOdleglosc){
            for(int ruchY=-1; ruchY < 2; ruchY++) {
                for (int ruchX = -1; ruchX < 2; ruchX++) {
                    if(ruchX == 0 && ruchY == 0)
                        continue;
                    try {
                        if (swiat.pole(robot.x + ruchX, robot.y + ruchY) instanceof Gwiazda)
                            bliskieGwiazdy.add((Gwiazda) swiat.pole(robot.x + ruchX, robot.y + ruchY));
                    } catch (ArrayIndexOutOfBoundsException e) {}
                }
            }
            odleglosc++;
        }

        for(Gwiazda gwiazda : bliskieGwiazdy){
            int iloscPrzyblizenDoStacji = 0;
            for(int stacjaID=0; stacjaID<stacje.size(); stacjaID++){
                try {
                    double mocPoRuchu = swiat.podajMoc(stacje.get(stacjaID), gwiazda);
                    if(mocPoRuchu > moceOdStacji[stacjaID]) //jestesmy blizej tej stacji
                        iloscPrzyblizenDoStacji++;
                } catch(Exception e) {} // exception jesli poza mapa
            }
            if(iloscPrzyblizenDoStacji == 0 || iloscPrzyblizenDoStacji == stacje.size())
                return false;
        }
        return true;
    }

    public static boolean dokladna(Swiat swiat){
        ArrayList<Stacja> stacje = swiat.wezStacje();
        Point robot = new Point(swiat.wezRobot().x, swiat.wezRobot().y);
        Polygon wielokoat = new Polygon();
        for(Stacja stacja : stacje)
            wielokoat.addPoint(stacja.x, stacja.y);
        return wielokoat.contains(robot.x, robot.y);
    }

    public static boolean pit(Swiat swiat){
        ArrayList<Stacja> stacje = swiat.wezStacje();
        Robot robot = swiat.wezRobot();
        double moceOdStacji[] = robot.wezMoce();

        for(int ruchY=-1; ruchY < 2; ruchY++) {
            for (int ruchX =-1; ruchX < 2; ruchX++) {
                if(ruchX == 0 && ruchY == 0)
                    continue;
                int iloscPrzyblizenDoStacji = 0;
                for(int stacjaID=0; stacjaID<stacje.size(); stacjaID++){
                    try {
                        double mocPoRuchu = swiat.podajMoc(stacje.get(stacjaID), robot.x + ruchX, robot.y + ruchY);
                        if(mocPoRuchu > moceOdStacji[stacjaID]) //jestesmy blizej tej stacji
                            iloscPrzyblizenDoStacji++;
                    } catch(Exception e) {} // exception jesli poza mapa
                }
                if(iloscPrzyblizenDoStacji == 0 || iloscPrzyblizenDoStacji == stacje.size())
                    return false;
            }
        }
        return true;
    }
}
