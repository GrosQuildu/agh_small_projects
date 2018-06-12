import java.awt.*;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

public class Swiat {
	private int wysokosc, szerokosc;
	public double maxMoc;
	private ArrayList<Stacja> stacje = new ArrayList<Stacja>();
	private ArrayList<Gwiazda> gwiazdy = new ArrayList<Gwiazda>();
	private Robot robot = null;
	private Pole[][] pola;
	
	public Swiat(int wysokosc, int szerokosc, double prawdopodobienstwo, int iloscStacji, double maxMoc){
		this.maxMoc = maxMoc;
		this.wysokosc = wysokosc;
		this.szerokosc = szerokosc;

		this.pola = new Pole[szerokosc][wysokosc];
		for(int y=0; y<this.wysokosc; y++) {
			for (int x = 0; x < this.szerokosc; x++) {
				this.pola[x][y] = new Pole(this, "P", x, y);
			}
		}

		Random generator = new Random();
		int iloscGwiazd = (int)(((wysokosc*szerokosc) - 4)*prawdopodobienstwo);

		//dodaj stacje
		while(this.stacje.size() < iloscStacji){
			int x = generator.nextInt(szerokosc);
			int y = generator.nextInt(wysokosc);
			this.dodajStacje(x, y);
		}

		//dodaj robota
		while(this.robot == null){
			int x = generator.nextInt(szerokosc);
			int y = generator.nextInt(wysokosc);
			this.dodajRobota(x, y);
		}

		//dodaj gwiazdy
		while(this.gwiazdy.size() < iloscGwiazd && this.gwiazdy.size()+this.stacje.size()+1 < wysokosc*szerokosc){
			int x = generator.nextInt(szerokosc);
			int y = generator.nextInt(wysokosc);
			this.dodajGwiazde(x, y);
		}
	}

	public Swiat(String sciezka) throws FileNotFoundException{
        File plik = new File(sciezka);
        Scanner scanner = new Scanner(plik, "UTF-8");

        this.szerokosc = Integer.parseInt(scanner.nextLine());
        this.wysokosc = Integer.parseInt(scanner.nextLine());
        this.maxMoc = Double.parseDouble(scanner.nextLine());

        this.pola = new Pole[szerokosc][wysokosc];
        for(int y=0; y<this.wysokosc; y++) {
            for (int x = 0; x < this.szerokosc; x++) {
                this.pola[x][y] = new Pole(this, "P", x, y);
            }
        }

        String pozycjeStacji = scanner.nextLine();
        if(pozycjeStacji.length() != 0) {
            for (String xy : pozycjeStacji.split(";")) {
                int x = Integer.parseInt(xy.split(",")[0]);
                int y = Integer.parseInt(xy.split(",")[1]);
                this.dodajStacje(x, y);
            }
        }

        String pozycjeGwiazd = scanner.nextLine();
        if(pozycjeGwiazd.length() != 0) {
            for (String xy : pozycjeGwiazd.split(";")) {
                int x = Integer.parseInt(xy.split(",")[0]);
                int y = Integer.parseInt(xy.split(",")[1]);
                this.dodajGwiazde(x, y);
            }
        }

        String pozycjeRobota = scanner.nextLine();
        if(pozycjeRobota.length() != 0) {
            pozycjeRobota = pozycjeRobota.substring(0, pozycjeRobota.length() - 1);
            int x = Integer.parseInt(pozycjeRobota.split(",")[0]);
            int y = Integer.parseInt(pozycjeRobota.split(",")[1]);
            this.dodajRobota(x, y);
        }
    }

	public void export(String sciezka) throws FileNotFoundException, UnsupportedEncodingException{
        File plik = new File(sciezka);
        PrintWriter writer = null;
		writer = new PrintWriter(plik, "UTF-8");

        writer.println(this.szerokosc);
        writer.println(this.wysokosc);
        writer.println(this.maxMoc);

        for(Stacja stacja : stacje)
            writer.print(stacja.x+","+stacja.y+";");
        writer.print("\n");
        for(Gwiazda gwiazda : gwiazdy)
            writer.print(gwiazda.x+","+gwiazda.y+";");
        writer.print("\n");

        writer.print(robot.x+","+robot.y+";");
        writer.print("\n");
        writer.close();
	}

    private double funkcjaMocy(double odleglosc){
        if(odleglosc == 0)
            return maxMoc;
        double res = ((-maxMoc*6)/(szerokosc*5)) * odleglosc + maxMoc;
        if(res > 0)
            return res;
        return 0;
//        return maxMoc*(1/(odleglosc))*0.99;
//        return maxMoc*Math.abs(Math.cos((3.1415*odleglosc)/(this.szerokosc*2)));
    }

    public double odlegloscZMocy(double moc){
        if(moc == maxMoc)
            return 0;
        return 1;
    }

    public Pole pole(int x, int y) throws ArrayIndexOutOfBoundsException {
		return this.pola[x][y];
    }

    public int szerokosc(){ return this.szerokosc; }

    public int wysokosc(){ return this.wysokosc; }

    public double podajMoc(Pole pole1, Pole pole2) throws Exception {
		if(pole1.x < 0 || pole1.y < 0 || pole2.x < 0 || pole2.y < 0 || pole1.x >= this.szerokosc ||
				pole2.x >= this.szerokosc || pole1.y >= this.wysokosc || pole2.y >= this.wysokosc)
			throw new Exception("Błędna pozycja");
        double odl = odleglosc(pole1.x, pole1.y, pole2.x, pole2.y);
        return funkcjaMocy(odl);
    }

    public double podajMoc(Pole pole, int x, int y) throws Exception {
		if(pole.x < 0 || pole.y < 0 || x < 0 || y < 0 || pole.x >= this.szerokosc || x >= this.szerokosc || pole.y >= this.wysokosc || y >= this.wysokosc)
			throw new Exception("Błędna pozycja");
        double odl = odleglosc(pole.x, pole.y, x, y);
        return funkcjaMocy(odl);
    }

    public double podajMoc(int x1, int y1, int x2, int y2) throws Exception {
        if(x1 < 0 || y1 < 0 || x2 < 0 || y2 < 0 || x1 >= this.szerokosc || x2 >= this.szerokosc || y1 >= this.wysokosc || y2 >= this.wysokosc)
            throw new Exception("Błędna pozycja");
        double odl = odleglosc(x1, y1, x2, y2);
        return funkcjaMocy(odl);
    }

	public double podajMoc(String poleID, String stacjaID) throws Exception {
		int gwiazdaX=-1, gwiazdaY=-1;
		Stacja stacja = null;

		for(int x=0; x<this.szerokosc; x++){
			for(int y=0; y<this.wysokosc; y++){
				if(poleID.equals(pola[x][y].id)){
					gwiazdaX = x;
					gwiazdaY = y;
				}
				if(stacjaID.equals(pola[x][y].id)){
					stacja = (Stacja)pola[x][y];
				}
			}
		}
		if(stacja == null || gwiazdaY == -1){
			throw new Exception("Nie znaleziono");
		}
		return podajMoc(stacja, gwiazdaX, gwiazdaY);
	}

	public Robot wezRobot(){
		return this.robot;
	}

	public Point[] wezPozycjeStacji(){
		Point pozycje[] = new Point[iloscStacji()];
		for(int id=0; id<iloscStacji(); id++){
			pozycje[id] = new Point(stacje.get(id).wezPunkt());
		}
		return pozycje;
	}

    public ArrayList<Stacja> wezStacje(){
        ArrayList<Stacja> stacje = new ArrayList<Stacja>();
        stacje.addAll(this.stacje);
        return stacje;
    }

	public String toString(){
		String w = "";
		for(int y=0; y<this.wysokosc; y++){
			for(int x=0; x<this.szerokosc; x++){
				if(pola[x][y] == null)
					w += " ";
				else
					w += pola[x][y];
				w += "|";
			}
			w+="\n";
			for(int tmp=0; tmp<2*this.szerokosc; tmp++){
				w += "-";
			}
			w+="\n";
		}
		return w;
	}
	
	public int iloscStacji(){
		return this.stacje.size();
	}
	
	public int iloscGwiazd(){
		return this.gwiazdy.size();
	}

	private double odleglosc(int x1, int y1, int x2, int y2){
		return Math.sqrt(Math.pow((double)x1-x2, 2) + Math.pow((double)y1-y2, 2));
	}

	private boolean dodajObiekt(Pole pole, int x, int y){
		if(!"P".equals(pola[x][y].id))
			return false;
		pola[x][y] = pole;
		return true;
	}
	
	private boolean dodajStacje(int x, int y){
		Stacja stacja = new Stacja(this, "S"+stacje.size(), x, y);
		if(dodajObiekt(stacja, x, y)){
			stacje.add(stacja);
			return true;
		}
		return false;
	}

	private boolean dodajGwiazde(int x, int y){
		Gwiazda gwiazda = new Gwiazda(this, "G"+gwiazdy.size(), x, y);
		if(dodajObiekt(gwiazda, x, y)){
			gwiazdy.add(gwiazda);
			return true;
		}
		return false;
	}

	private boolean dodajRobota(int x, int y){
		Robot robot = new Robot(this, "R", x, y);
		if(dodajObiekt(robot, x, y)){
			this.robot = robot;
			return true;
		}
		return false;
	}
}
