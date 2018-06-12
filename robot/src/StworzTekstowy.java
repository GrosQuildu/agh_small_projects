import java.util.Random;

public class StworzTekstowy {

	public static void main(String[] args) {
		try{
			int wysokosc = 5;
			int szerokosc = 10;
			double prawdopodobienstwo = 0.7;
			int iloscStacjcji = 3;
			double maxMoc = 100;
			Swiat mojSwiat = new Swiat(wysokosc, szerokosc, prawdopodobienstwo, iloscStacjcji, maxMoc);
			

			System.out.println("Swiat:");
			System.out.println(mojSwiat);
			
		} catch (Exception e){
			e.printStackTrace();
		}
	}

}
