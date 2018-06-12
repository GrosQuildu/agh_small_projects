#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <assert.h>
#include <limits.h>

/*
Kompresja Huffmana
Paweł Płatek
*/

int DEBUG=0;
void printf_error(const char *format, ...){
    va_list args;
    va_start (args, format);
    vfprintf (stderr,format, args);
    va_end (args);
    puts("");
    exit(1);
}

//Struktury
//Binarne drzewo przeszukiwan
typedef struct node{
    unsigned long int waga;
    short wartosc;
    struct node *lewy, *prawy;
} node;

//Kolejka priorytetowa na kopcu
node * kolejka[256]={0};
unsigned short kolejka_wskaznik=0; //wskazuje koniec kolejki (rozmiar kolejki)

void kolejka_swap(short a, short b){
    node *tmp_node;
    tmp_node=kolejka[b];
    kolejka[b]=kolejka[a];
    kolejka[a]=tmp_node;
}

void kolejka_dodaj(node *node_dodawany){
    if(kolejka_wskaznik==0){
        kolejka[kolejka_wskaznik]=node_dodawany;
        kolejka_wskaznik++;
        return;
    }

    kolejka[kolejka_wskaznik]=node_dodawany;
    kolejka_wskaznik++;
    char rodzic,tmp_wskaznik=kolejka_wskaznik;
    while(tmp_wskaznik>1){
        rodzic=tmp_wskaznik/2;
        if(kolejka[tmp_wskaznik-1]->waga < kolejka[rodzic-1]->waga){
            kolejka_swap(tmp_wskaznik-1,rodzic-1);
            tmp_wskaznik=rodzic;
        } else{
            return;
        }
    }
}

node* kolejka_wez(){
    if(kolejka_wskaznik==0)
        return 0;
    node *wynik=kolejka[0];
    kolejka[0]=kolejka[kolejka_wskaznik-1];
    kolejka[kolejka_wskaznik-1]=0;
    kolejka_wskaznik--;

    short potomek,tmp_wskaznik=1;
    while(tmp_wskaznik<kolejka_wskaznik){
        if(tmp_wskaznik*2>=256){
            break;
        }
        if(kolejka[(tmp_wskaznik*2)-1]==0) //break potomkow
            break;
        if(kolejka[tmp_wskaznik*2]==0) //nie ma prawego potomka
            potomek=tmp_wskaznik*2;
        else if(kolejka[tmp_wskaznik*2]->waga < kolejka[(tmp_wskaznik*2)-1]->waga)
            potomek=(tmp_wskaznik*2)+1;
        else
            potomek=tmp_wskaznik*2;

        if(kolejka[tmp_wskaznik-1]->waga > kolejka[potomek-1]->waga){
            kolejka_swap(tmp_wskaznik-1,potomek-1);
            tmp_wskaznik=potomek;
        } else{
            return wynik;
        }
    }
    return wynik;
}


//nieuporządkowana kolejka górska do przeszukiwania(wypisywania) drzewa wszerz
typedef struct uoqu{
    node *drzewko;
    struct uoqu *nastepny;
} uoqu;
uoqu *uoqu_poczatek=0;
uoqu *uoqu_koniec=0;

void uoqu_dodaj(node *node_dodawany, node *empty_node){
    uoqu *dodawany=(uoqu*)malloc(sizeof(uoqu));
    dodawany->drzewko=node_dodawany;
    dodawany->nastepny=0;
    if(uoqu_poczatek==0 && uoqu_koniec==0){
        uoqu_poczatek=uoqu_koniec=dodawany;
        return;
    }
    uoqu_koniec->nastepny=dodawany;
    uoqu_koniec=dodawany;
}

node* uoqu_wez(node *empty_node){
    if(uoqu_poczatek==0)
        return empty_node;
    node *wynik=uoqu_poczatek->drzewko;
    uoqu *tmp=uoqu_poczatek;
    if(uoqu_poczatek==uoqu_koniec)
        uoqu_poczatek=uoqu_koniec=0;
    else
        uoqu_poczatek = uoqu_poczatek->nastepny;
    free(tmp);
    return wynik;
}


void wypisz_drzewo(node *root, node *empty_node, node *level_node){
    node *tmp_node;
    uoqu_dodaj(root, empty_node);
    uoqu_dodaj(level_node, empty_node); //drzewo specjalne, reprezentujące kolejny poziom w drzewie
    printf("\n%40sDrzewo Huffmana\n","");

    while((tmp_node=uoqu_wez(empty_node))!=empty_node){
        if(tmp_node==level_node){
            if(uoqu_poczatek!=0)
                uoqu_dodaj(level_node, empty_node);
            continue;
        }
        printf("%u %02x: ", tmp_node->waga, abs(tmp_node->wartosc));
        if(tmp_node->lewy!=0){
            printf("l(%u %02x) ",tmp_node->lewy->waga,abs(tmp_node->lewy->wartosc));
            uoqu_dodaj(tmp_node->lewy, empty_node);
        }
        if(tmp_node->prawy!=0){
            printf("p(%u %02x) ",tmp_node->prawy->waga,abs(tmp_node->prawy->wartosc));
            uoqu_dodaj(tmp_node->prawy, empty_node);
        }
        puts("");
    }
    puts("\n");
    return;
}

void zetnij_drzewo(node *root){
    if(root->lewy!=0)
        zetnij_drzewo(root->lewy);
    if(root->prawy!=0)
        zetnij_drzewo(root->prawy);
    free(root);
}

//ustawia 'count' bitow na pozycji 'pointer'
//zwraca pierwsza niezapisana pozycje
void bitset(char *buffer, const short BUFSIZE, unsigned short *pointer, short val, short count){
    short i=0;
    count%=9; //potrzebuje pisac max 1 bajt naraz
    char tmp;
    for(i=0;i<count;i++)
    {
        tmp=(val>>i)&1; //wez bit
        if((*pointer)>=BUFSIZE*8)
            printf_error("[-]Przepelnienie buffora\n");
        if(tmp==0)
            buffer[(*pointer)/8]&=~(1<<((*pointer)%8));
        else
            buffer[(*pointer)/8]|=1<<((*pointer)%8);
        (*pointer)++;
    }
}

//pisze 'pointer' bitow
void bitprint(char *buffer, const short BUFSIZE, unsigned short pointer){
    unsigned short i;
    for(i=0;i<pointer;i++){
        if(i%8==0)
            printf(" ");
        printf("%u",(buffer[i/8]>>(i%8))&1);
    }
    puts("");
}

//przechodzenie drzewa w porzadku pre-order
//towrzy slowa kodowe && bitowa reprezentacje drzewa huffmana
void tworz_kody(node *wezel, char **kody, char *teraz, char *buffer, const short BUFSIZE, unsigned short *buf_pointer){
    if(wezel->wartosc!=-1){
        strncpy(kody[wezel->wartosc],teraz,299);
        bitset(buffer,BUFSIZE,buf_pointer,1,1);
        bitset(buffer,BUFSIZE,buf_pointer,wezel->wartosc,8);
        return;
    } else{
        bitset(buffer,BUFSIZE,buf_pointer,0,1);
    }
    if(wezel->lewy!=0){
        char *tmp=(char *)malloc(300);
        memset(tmp,0,300);
        strncpy(tmp,teraz,299);
        strncat(tmp,"0",1);
        tworz_kody(wezel->lewy, kody, tmp, buffer, BUFSIZE, buf_pointer);
        free(tmp);
    }
    if(wezel->prawy!=0){
        char *tmp=(char *)malloc(300);
        memset(tmp,0,300);
        strncpy(tmp,teraz,299);
        strncat(tmp,"1",1);
        tworz_kody(wezel->prawy, kody, tmp, buffer, BUFSIZE, buf_pointer);
        free(tmp);
    }
}


void koduj(FILE *plik, char *nazwa, char *magic, FILE *plik_nowy){
    int i=1,j=0,tmp;
    short znak;
    int unikalnych=0;
    unsigned long int wystapien[256]={0};
    unsigned int posortowane[256]={0};

    //Ilosc wystapien kazdego bajtu
    if(DEBUG)
        printf("[+]Zliczanie wystapien kazdego znaku\n");
    while((znak=getc(plik))!=EOF){
        if(wystapien[znak]>=ULONG_MAX){
            printf_error("[-]Integer overflow: plik za duzy");
        }
        wystapien[znak]++;
    }
    rewind(plik);

    //Posortuj niemalejaco
    if(DEBUG)
        printf("[+]Sortowanie\n");
    for(i = 0; i < 256; ++i)
    {
        if(wystapien[i]!=0){
            posortowane[unikalnych++]=i;
            tmp=unikalnych-1;
            while(tmp>0 && wystapien[posortowane[tmp]]<wystapien[posortowane[tmp-1]]){
                // swap(posortowane[tmp],posortowane[--tmp]);
                posortowane[tmp]^=posortowane[tmp-1];
                posortowane[tmp-1]^=posortowane[tmp];
                posortowane[tmp]^=posortowane[tmp-1];
                tmp--;
            }
        }
    }

    if(DEBUG){
        for(i=0;i<unikalnych;i++)
            printf("    %02x: %u\n",posortowane[i], wystapien[posortowane[i]]);
        printf("[+]Tworzenie drzewa bst\n");
    }

    //Tworzenie drzewa BST
    node *tmp_node=0;
    node *tmp_node2=0;
    node *tmp_node3=0;
    for(i=0; i<unikalnych; i++)
    {
        tmp_node=(node *)malloc(sizeof(node));
        tmp_node->lewy=tmp_node->prawy=0;
        tmp_node->waga=wystapien[posortowane[i]];
        tmp_node->wartosc=posortowane[i];
        kolejka_dodaj(tmp_node);
    }

    if(DEBUG){
        printf("[+]Unikalnych znakow: %u\n", kolejka_wskaznik);
        printf("[+]Kolejka z liscmi:\n\t");
        i=0;
        while(i<kolejka_wskaznik){
            printf("%u(%02x) ",kolejka[i]->waga, kolejka[i]->wartosc);
            i++;
        }
        printf("\n[+]Kolejka po laczeniu dwoch lisci z min. wagami:");
    }

    if(kolejka_wskaznik==1){
        tmp_node2=(node *)malloc(sizeof(node));
        kolejka_dodaj(tmp_node2);
    }
    while(kolejka_wskaznik>1){
        tmp_node=kolejka_wez();
        tmp_node3=kolejka_wez();
        tmp_node2=(node *)malloc(sizeof(node));

        tmp_node2->lewy=tmp_node;
        tmp_node2->prawy=tmp_node3;
        if(tmp_node->waga+tmp_node3->waga>=ULONG_MAX)
            printf_error("[-]Long integer overflow: plik za duzy");
        tmp_node2->waga=tmp_node->waga+tmp_node3->waga;
        tmp_node2->wartosc=-1;
        kolejka_dodaj(tmp_node2);

        if(DEBUG){
            printf("\n    ");
            for(i=0; i<kolejka_wskaznik; i++){
                printf("%u ",kolejka[i]->waga);
            }
        }
    }

    tmp_node=kolejka_wez();
    assert(kolejka_wskaznik==0);
    assert(tmp_node!=0);

    if(DEBUG){
        node *level_node=(node *)malloc(sizeof(node));
        node *empty_node=(node *)malloc(sizeof(node));
        wypisz_drzewo(tmp_node, empty_node, level_node);
        free(level_node);
        free(empty_node);
    }

    //Tworzenie kodów
    if(DEBUG)
        printf("[+]Tworzenie kodow:\n");
    char *teraz="";
    const short BUFSIZE=256+320;
    unsigned short buf_pointer=0;
    char *kody[256];
    for(i=0; i<256; i++)
        kody[i]=(char *)malloc(300);
    char *buffer=(char *)malloc(sizeof(char)*BUFSIZE);

    if(!buffer){
        printf_error("[-]Nie moge zaalokowac wystarczajacej ilosci pamieci");
    }
    memset(buffer,0,sizeof(char)*BUFSIZE);

    tworz_kody(tmp_node,kody,teraz,buffer,BUFSIZE, &buf_pointer);
    if(DEBUG){
        for(i=0;i<unikalnych;i++)
            printf("    %02x - %s\n",posortowane[i], kody[posortowane[i]]);
        printf("[+]Wielkosc (w bitach) drzewa huffmana: %u\n",buf_pointer);
        printf("[+]Buffer z drzewem huffmana\n   ");
        bitprint(buffer,BUFSIZE,buf_pointer);
    }

    //Otwieranie pliku
    if(DEBUG)
        printf("[+]Zapis drzewa do pliku\n");

    //magic num
    if(DEBUG)
        printf("    ->Magiczny numer\n");
    fprintf(plik_nowy,"%s",magic);

    //pisz do bufera, przezucaj do pliku
    char symbol;
    unsigned short wyrownaj;
    while((znak=getc(plik))!=EOF){
        if(buf_pointer>=256*8){ //zapis co 256 bajtow
            if(fwrite(buffer, 1, buf_pointer/8, plik_nowy)<buf_pointer/8)
                printf_error("[-]Blad zapisu");
            wyrownaj=(buf_pointer-(8*(buf_pointer/8)))/8;
            if((buf_pointer%8)!=0)
                wyrownaj++;
            memcpy(buffer,buffer+(buf_pointer/8),wyrownaj);
            buf_pointer=buf_pointer-(8*(buf_pointer/8));
        }
        //wczytaj kod po bajcie
        for(symbol=0; symbol<strlen(kody[znak]); symbol++){
            if(kody[znak][symbol]=='0')
                bitset(buffer,BUFSIZE, &buf_pointer, 0, 1);
            else
                bitset(buffer,BUFSIZE, &buf_pointer, 1, 1);
        }
    }

    //dokoncz
    wyrownaj=buf_pointer/8;
    if((buf_pointer%8)!=0)
                wyrownaj++;
    char nadmiar=wyrownaj*8-buf_pointer;
    if(DEBUG)
        printf("    ->Slowa kodowe\n");
    if(fwrite(buffer, 1, wyrownaj, plik_nowy)<wyrownaj)
        printf_error("[-]Blad zapisu");
    if(DEBUG)
        printf("    ->Nadmiar: %x(bitow)\n", nadmiar);

    if(fwrite(&nadmiar, 1, 1, plik_nowy)<1)
        printf_error("[-]Blad zapisu");

    //czyszczenie
    if(DEBUG)
        printf("[+]Czyszczenie\n");
    fclose(plik);
    fclose(plik_nowy);
    free(buffer);
    for(i=0; i<256; i++)
        free(kody[i]);
    zetnij_drzewo(tmp_node);

}

//przeczytaj 'count' bitow
short readbit(char *buffer, unsigned int *buf_pointer, unsigned short count){
    short bajt=0,bit,i;
    for(i=0; i<count; i++)
    {
        bit=(buffer[(*buf_pointer)/8]>>((*buf_pointer)%8))&1;
        (*buf_pointer)++;
        if(bit==0)
            bajt&=~(1<<i);
        else
            bajt|=1<<i;
    }
    return bajt;
}

node * posadz_drzewo(char *buffer, const short BUFSIZE, unsigned int *buf_pointer){
    if((*buf_pointer)>BUFSIZE*8)
        printf_error("[-]Nieprawidlowy plik");
    char bit;
    bit=readbit(buffer, buf_pointer, 1);
    if(bit==1){
        node *tmp_node=(node *)malloc(sizeof(node));
        tmp_node->lewy=tmp_node->prawy=0;
        tmp_node->waga=0;
        tmp_node->wartosc=readbit(buffer, buf_pointer, 8);
        return tmp_node;
    } else{
        node *tmp_node=(node *)malloc(sizeof(node));
        node *tmp_node1=(node *)malloc(sizeof(node));
        node *tmp_node2=(node *)malloc(sizeof(node));
        tmp_node1=posadz_drzewo(buffer, BUFSIZE, buf_pointer);
        tmp_node2=posadz_drzewo(buffer, BUFSIZE, buf_pointer);
        tmp_node->wartosc=-1;
        tmp_node->waga=0;
        tmp_node->lewy=tmp_node1;
        tmp_node->prawy=tmp_node2;
        return tmp_node;
    }
}

unsigned char wez_znak(node* root, char *buffer, unsigned int *buf_pointer){
    if(root->wartosc!=-1){
        if(DEBUG)
            printf(" - %x\n", root->wartosc);
        return root->wartosc;
    }
    unsigned char bit=readbit(buffer, buf_pointer, 1);
    if(bit==0){
        if(DEBUG)
            printf("0");
        return wez_znak(root->lewy, buffer, buf_pointer);
    }
    else{
        if(DEBUG)
            printf("1");
        return wez_znak(root->prawy, buffer, buf_pointer);
    }
}


void dekoduj(FILE *plik, char *nazwa, char *magic, FILE *plik_nowy){
    unsigned int i,j,tmp,rozmiar;
    short wyrownaj;
    unsigned char znak;

    if(DEBUG)
        printf("[+]Dekodowanie\n");

    //sprawdz magiczna liczbe
    if(DEBUG)
        printf("[+]Sprawdzanie magicznej liczby\n");
    char magic_read[5];
    if(!fgets(magic_read,5,plik))
        printf_error("[-]Blad podczas czytania");
    if(strncmp(magic_read,magic,2)!=0)
        printf_error("[-]Nieprawidlowy plik");

    //wez ilosc nadmiarowych bitow
    unsigned short nadmiar;
    fseek(plik, -1, SEEK_END);
    memset(magic_read,0,5);
    fread(magic_read, 1, 1, plik);
    nadmiar=magic_read[0];
    fseek(plik, 2, SEEK_SET);
    if(DEBUG)
        printf("[+]Nadmiarowych bitow: %hu\n",nadmiar);
    if(nadmiar>=8)
        printf_error("[-]Nieprawidlowy plik");

    //odbuduj drzewo
    if(DEBUG)
        printf("[+]Budowanie drzewa\n");
    char teraz[51]="";
    const short BUFSIZE=256+320;
    unsigned int buf_pointer=0, buf_end;
    char *buffer=(char *)malloc(BUFSIZE+1);

    if(!buffer){
        printf_error("[-]Nie moge zaalokowac wystarczajacej ilosci pamieci");
    }
    memset(buffer,0,BUFSIZE);
    buf_end=fread(buffer,1,BUFSIZE,plik);

    buf_pointer=0;
    node *root=posadz_drzewo(buffer,BUFSIZE,&buf_pointer);

    if(DEBUG){
        node *level_node=(node *)malloc(sizeof(node));
        node *empty_node=(node *)malloc(sizeof(node));
        wypisz_drzewo(root, empty_node, level_node);
        free(level_node);
        free(empty_node);
    }

    //stworz nowy plik
    if(DEBUG)
        printf("[+]Zapisywanie:\n");

    //zdekoduj
    while(buf_pointer<((buf_end-1)*8)-nadmiar){
        if(buf_pointer/8>BUFSIZE/2){ //odswiezamy buffer
            memcpy(buffer, buffer+(buf_pointer/8), buf_end-(buf_pointer/8));
            buf_end=fread(buffer+buf_end-(buf_pointer/8), 1, (buf_pointer/8), plik)+buf_end-(buf_pointer/8);
            buf_pointer=buf_pointer%8;
        }
        znak=wez_znak(root, buffer, &buf_pointer);
        if(fputc(znak, plik_nowy)==EOF)
            printf_error("[-]Blad zapisu");
    }

    if(DEBUG)
        printf("[+]Czyszczenie\n");
    fclose(plik);
    fclose(plik_nowy);
    free(buffer);
    zetnij_drzewo(root);

}

int main(int argc, char **argv){
    if(argc<2){
        printf_error("[-]Usage: %s [OPTIONS] filename",argv[0]);
    }

    //Deklaracja zmiennych
    char *nazwa="", *nazwa_nowa="";
    char *magic="\x13\x37";
    unsigned char mode=0, force=0;
    unsigned int rozmiar=0;
    int i=1;
    FILE *plik, *plik_nowy;

    //Parsuj argumenty
    while(i<argc){
        if(strcmp(argv[i],"-h")==0 || strcmp(argv[i],"--help")==0){
            printf("Usage: %s [OPTIONS] file\n\n\
Options:\n\
    -d, --decompress\tDecompress file\n\
    -o, --output fn\tWrite to fn (defalut is input with .pp\\.dc extension)\n\
    -f, --force\t\tOverwrite file (instead of creating new)\n\
    -h, --help\t\tShow help message\n\
    -v, --verbose\tDebug mode\n",argv[0]);
            return 0;
        } else if(strcmp(argv[i],"-d")==0 || strcmp(argv[i],"decompress")==0){
            mode=1;
        } else if(strcmp(argv[i],"-v")==0 || strcmp(argv[i],"--verbose")==0){
            puts("Debug mode");
            DEBUG=1;
        } else if(strcmp(argv[i],"-f")==0 || strcmp(argv[i],"--force")==0){
            force=1;
        } else if(strcmp(argv[i],"-o")==0 || strcmp(argv[i],"--output")==0){
            if(i+1>=argc || i+1>255)
                printf_error("[-]Nie podano nazwy nowego pliku dla opcji %s", argv[i+1]);
            nazwa_nowa=argv[i+1];
            i++;
        } else{
            nazwa=argv[i];
            break;
        }
        i++;
    }

    //Przygotowanie nazw plikow
    if(nazwa==""){
        printf_error("[-]Nie podano pliku do (de)kompresji");
    }
    if(strlen(nazwa_nowa)==0){
        nazwa_nowa=(char *)malloc(strlen(nazwa)+3);
        strcpy(nazwa_nowa, nazwa);
        if(mode==0)
            strcpy(nazwa_nowa+strlen(nazwa), ".pp");
        else
            strcpy(nazwa_nowa+strlen(nazwa), ".dc");
    }

    if(DEBUG)
        printf("[+]Otwieranie %s\n", nazwa);
    if(force==1){
        plik_nowy=fopen(nazwa_nowa, "w");
    } else{
        plik_nowy=fopen(nazwa_nowa, "wx");
        
    }
    if(plik_nowy==NULL){
      if(errno == EEXIST)
        printf_error("[-]Nie moge otworzyc %s do zapisu, plik istnieje", nazwa_nowa);
      else
        printf_error("[-]Nie moge otworzyc %s do zapisu", nazwa_nowa);
    }
    
    //Przygotowanie pliku
    if(DEBUG)
        printf("[+]Otwieranie %s\n", nazwa);
    plik=fopen(nazwa,"rb");
    if(!plik){
        fclose(plik_nowy);
        printf_error("[-]Nie moge otworzyc %s do odczytu",nazwa);
    }
    fseek(plik, 0, SEEK_END);
    rozmiar = ftell(plik);
    rewind(plik);
    if(rozmiar<2){
        fclose(plik_nowy);
        fclose(plik);
        printf_error("[-]Za maly plik");
    }
    if(DEBUG)
        printf("[+]Nazwa nowego pliku: %s\n", nazwa_nowa);

    if(mode==0)
        koduj(plik,nazwa,magic, plik_nowy);
    else
        dekoduj(plik,nazwa,magic, plik_nowy);

    return 0;
}