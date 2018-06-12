import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartFrame;
import org.jfree.chart.ChartUtilities;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.DomainInfo;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;


import java.io.*;
import java.util.ArrayList;
import java.util.Locale;
import java.util.Scanner;

/**
 * Created by gros on 23.01.17.
 */
public class Main {
    public static void main(String[] args){
        Scanner scanner = new Scanner(System.in);
        while(true) {
            System.out.println("1) Statystyki");
            System.out.println("2) Pokaż świat z pliku");
            System.out.println("3) Stwórz losowy świat");
            System.out.println("4) Wyjdź");
            int wybor = scanner.nextInt();
            switch (wybor) {
                case 1:
                    statystyki();
                    break;
                case 2:
                    stworzZPliku();
                    break;
                case 3:
                    stworzLosowy();
                    break;
                case 4:
                    System.exit(0);
                default:
                    System.out.println("Błędny wybór");
                    break;
            }
        }
    }

    private static void stworzGraficzny(Swiat swiat){
        String metody = "";

        System.out.println("Robie testy");
        boolean dokladna = Metody.dokladna(swiat);
        if(dokladna){
            metody += "Dokładna=wewnatrz, ";
        } else{
            metody += "Dokładna=na zewnatrz, ";
        }

        boolean pit = Metody.pit(swiat);
        if(pit){
            metody += "PIT=wewnatrz, ";
        } else{
            metody += "PIT=na zewnatrz, ";
        }

        boolean apit = Metody.apit(swiat);
        if(apit){
            metody += "APIT=wewnatrz, ";
        } else{
            metody += "APIT=na zewnatrz, ";
        }

        System.out.println("Tworze świat...");
        StworzGraficzny swiatGraficzny = new StworzGraficzny(swiat);
        swiatGraficzny.setVisible(true);
        swiatGraficzny.edytujMetodyTekst(metody);
    }

    private static void stworzZPliku(){
        System.out.print("Podaj ścieżke do pliku (np. ./swiaty/40.00/apit/37): ");
        Scanner scanner = new Scanner(System.in);
        String sciezka = scanner.next();
        Swiat swiat;
        try {
            swiat = new Swiat(sciezka);
        } catch (FileNotFoundException e){
            System.out.println("Błąd, nie znaleziono pliku");
            return;
        }
        stworzGraficzny(swiat);
    }

    private static void stworzLosowy(){
        Scanner scanner = new Scanner(System.in);
        System.out.println("Wysokość/szerokość poniżej 100");
        System.out.print("Podaj wysokość (ilość pól): ");
        int wysokosc = scanner.nextInt();
        System.out.print("Podaj szerokość (ilość pól): ");
        int szerokosc = scanner.nextInt();
        System.out.print("Podaj prawdopodobieństwo gwiazd [0,0-1,0]: ");
        double prawdopodobienstwo = scanner.nextDouble();

        int iloscStacji = 3;
        double maxMoc = 100;

        Swiat swiat = new Swiat(wysokosc, szerokosc, prawdopodobienstwo, iloscStacji, maxMoc);
        stworzGraficzny(swiat);
    }

    private static void deleteRec(File plik){
        if(plik.isDirectory()) {
            String[]entries = plik.list();
            for(String s: entries){
                File currentFile = new File(plik.getPath(),s);
                if(currentFile.isDirectory()) {
                    deleteRec(currentFile);
                }
                else {
                    currentFile.delete();
                }
            }
        }
        plik.delete();
    }

    private static void przygotujFoldery(String sciezkaDoZapisow, ArrayList<Double> prawdopodobienstwa) throws Exception {

        File folderDoZapisow = new File(sciezkaDoZapisow);

        deleteRec(folderDoZapisow);

        if(!folderDoZapisow.mkdir()){
            throw new Exception("Nie można stworzyć ścieżki do zapisów: "+sciezkaDoZapisow);
        } else{
            for(double prawdopodobienstwo : prawdopodobienstwa){
                String sciezka = sciezkaDoZapisow+"/"+String.format(Locale.US, "%.2f", prawdopodobienstwo*100);
                folderDoZapisow = new File(sciezka);
                if(!folderDoZapisow.mkdir())
                    throw new Exception("Nie można stworzyć ścieżki do zapisów: ");
                folderDoZapisow = new File(sciezka+"/pit");
                if(!folderDoZapisow.mkdir())
                    throw new Exception("Nie można stworzyć ścieżki do zapisów: "+"/pit");
                folderDoZapisow = new File(sciezka+"/apit");
                if(!folderDoZapisow.mkdir())
                    throw new Exception("Nie można stworzyć ścieżki do zapisów: "+"/apit");
            }
        }
    }

    private static void statystyki(){
        Scanner scanner = new Scanner(System.in);
        String sciezkaDoZapisow = "./swiaty";

        // pomiar dla takich prawdopodobienst
        ArrayList<Double> prawdopodobienstwa = new ArrayList<Double>();
        for(double i=0.1; i<=1; i+=0.1)
            prawdopodobienstwa.add(i);

        System.out.println("Wysokość/szerokość poniżej 100, ilość testów poniżej 200");
        System.out.println("Światy z błędnymi wynikami będą zapisane do "+sciezkaDoZapisow+"/<prawdopodobienstwo>/<metoda>/<nr_testu>");
        System.out.print("Podaj wysokość (ilość pól): ");
        int wysokosc = scanner.nextInt();
        System.out.print("Podaj szerokość (ilość pól): ");
        int szerokosc = scanner.nextInt();
        System.out.print("Podaj ilość testów: ");
        int iloscTestow = scanner.nextInt();

        int iloscStacji = 3;
        double maxMoc = 100;

        boolean czyFolderyOk = true;
        try {
            przygotujFoldery(sciezkaDoZapisow, prawdopodobienstwa);
        } catch (Exception e){
            System.out.println(e.getMessage());
            czyFolderyOk = false;
        }

        XYSeries seriesPit = new XYSeries("PIT");
        XYSeries seriesApit = new XYSeries("APIT");
        XYSeries seriesPitInToOut = new XYSeries("InToOut");
        XYSeries seriesPitOutToIn = new XYSeries("OutToIn");
        XYSeries seriesApitInToOut = new XYSeries("InToOut");
        XYSeries seriesApitOutToIn = new XYSeries("OutToIn");

        for(double prawdopodobienstwo : prawdopodobienstwa){
            int pitPorazki = 0;
            int pitInToOut = 0;
            int apitPorazki = 0;
            int apitInToOut = 0;
            String sciezka = sciezkaDoZapisow+"/"+String.format(Locale.US, "%.2f", prawdopodobienstwo*100);
            for(int licznik=0; licznik<iloscTestow; licznik++){
                Swiat swiat = new Swiat(wysokosc, szerokosc, prawdopodobienstwo, iloscStacji, maxMoc);
                boolean dokladna = Metody.dokladna(swiat);
                boolean pit = Metody.pit(swiat);
                boolean apit = Metody.apit(swiat);
                if(apit != dokladna) {
                    apitPorazki++;
                    if(dokladna)  // robot wewnatrz, ale apit mowi ze na zewnatrz
                        apitInToOut++;
                    if(czyFolderyOk){
                        try {
                            swiat.export(sciezka+"/apit/"+licznik);
                        } catch (FileNotFoundException e){
                            System.out.println("Błąd pliku: "+sciezka+"/apit/"+licznik);
                        } catch (UnsupportedEncodingException e){
                            System.out.println("Błąd kodowania: "+sciezka+"/apit/"+licznik);
                        }
                    }
                }
                if(pit != dokladna) {
                    pitPorazki++;
                    if(dokladna)  // robot wewnatrz, ale pit mowi ze na zewnatrz
                        pitInToOut++;
                    if(czyFolderyOk){
                        try {
                            swiat.export(sciezka+"/pit/"+licznik);
                        } catch (FileNotFoundException e){
                            System.out.println("Błąd pliku: "+sciezka+"/apit/"+licznik);
                        } catch (UnsupportedEncodingException e){
                            System.out.println("Błąd kodowania: "+sciezka+"/apit/"+licznik);
                        }
                    }
                }
            }
            double pitPrawdopodobientwo = (100.0*pitPorazki)/(double)iloscTestow;
            double apitPrawdopodobientwo = (100.0*apitPorazki)/(double)iloscTestow;
            System.out.printf("Prawdopodobieństwo %.1f%%, porażek: ", prawdopodobienstwo*100);
            System.out.printf("pit-%.1f%%; apit-%.1f%%\n", pitPrawdopodobientwo, apitPrawdopodobientwo);

            seriesPit.add(prawdopodobienstwo*100, pitPrawdopodobientwo);
            seriesApit.add(prawdopodobienstwo*100, apitPrawdopodobientwo);

            seriesPitInToOut.add(prawdopodobienstwo*100, pitInToOut);
            seriesPitOutToIn.add(prawdopodobienstwo*100, pitPorazki-pitInToOut);

            seriesApitInToOut.add(prawdopodobienstwo*100, apitInToOut);
            seriesApitOutToIn.add(prawdopodobienstwo*100, apitPorazki-apitInToOut);
        }

        System.out.println("Tworze wykresy...");
        XYSeriesCollection dataset = new XYSeriesCollection();
        dataset.addSeries(seriesPit);
        dataset.addSeries(seriesApit);
        // Generate the graph
        JFreeChart chart = ChartFactory.createXYLineChart(
                "Położenie punktu w trójkącie", // Title
                "Gęstość gwiazd [%]", // x-axis Label
                "Procent błędów [%]", // y-axis Label
                dataset, // Dataset
                PlotOrientation.VERTICAL, // Plot Orientation
                true, // Show Legend
                true, // Use tooltips
                false // Configure chart to generate URLs?
        );
        ChartFrame frame = new ChartFrame("Results", chart);
        frame.pack();
        frame.setVisible(true);

        XYSeriesCollection dataset2 = new XYSeriesCollection();
        dataset2.addSeries(seriesPitInToOut);
        dataset2.addSeries(seriesPitOutToIn);
        // Generate the graph
        JFreeChart chart2 = ChartFactory.createXYLineChart(
                "PIT rodzaj błędu", // Title
                "Gęstość gwiazd [%]", // x-axis Label
                "Ilość błędów", // y-axis Label
                dataset2, // Dataset
                PlotOrientation.VERTICAL, // Plot Orientation
                true, // Show Legend
                true, // Use tooltips
                false // Configure chart to generate URLs?
        );
        ChartFrame frame2 = new ChartFrame("Results", chart2);
        frame2.pack();
        frame2.setVisible(true);

        XYSeriesCollection dataset3 = new XYSeriesCollection();
        dataset3.addSeries(seriesApitInToOut);
        dataset3.addSeries(seriesApitOutToIn);
        // Generate the graph
        JFreeChart chart3 = ChartFactory.createXYLineChart(
                "APIT rodzaj błędu", // Title
                "Gęstość gwiazd [%]", // x-axis Label
                "Ilość błędów", // y-axis Label
                dataset3, // Dataset
                PlotOrientation.VERTICAL, // Plot Orientation
                true, // Show Legend
                true, // Use tooltips
                false // Configure chart to generate URLs?
        );
        ChartFrame frame3 = new ChartFrame("Results", chart3);
        frame3.pack();
        frame3.setVisible(true);
    }
}
