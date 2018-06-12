-- przegra.adb


with System;
with Ada.Text_IO;
use Ada.Text_IO;
with Ada.Exceptions;
use Ada.Exceptions;

with Conf;
with PakRoad; use PakRoad;
with PakEntryPoint; use PakEntryPoint;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;


procedure PrzeGra is

    pragma Priority (System.Priority'First);

    RoadSize : Conf.RoadSizeType;

    Visibility : Cursor_Visibility;

    Margin_Y : Line_Position;
    Margin_X : Column_Position;

    Max_Y : Line_Position;
    Max_X : Column_Position;

    Terminal_Size_Y: Line_Count;

    RoadSizeY : Line_Count;
    RoadSizeX : Column_Count;

    TerminalWindow : Window;
    RoadWindow : Window;

    done : Boolean;
    c : Key_Code;

    type EntryPointPtr is access all EntryPoint;
    type EntryPointsList is array (Natural range <>) of EntryPointPtr;

begin
    -- Initialize curses stuff
    Init_Screen;
    Set_NL_Mode;
    Set_Echo_Mode(False);

    Visibility := Invisible;
    Set_Cursor_Visibility(Visibility);
    Set_Timeout_Mode(Standard_Window, Non_Blocking, 0);

    Margin_Y := 1;
    Margin_X := 10;
    Max_Y := Lines - 2*Margin_Y;
    Max_X := Columns - 2*Margin_X;
    Terminal_Size_Y := Line_Count(Conf.Terminal_Size_Y);

    -- Compute Road size (for curses and as array. Curses road size = road arary size * car size)
    RoadSizeY := Line_Count(Integer(Max_Y - Terminal_Size_Y - Margin_Y) / Conf.CarSize(1) * Conf.CarSize(1)); 
    RoadSizeX := Column_Count(Integer(Max_X) / Conf.CarSize(2) * Conf.CarSize(2));
    RoadSize := (Integer(RoadSizeY)/Conf.CarSize(1), Integer(RoadSizeX)/Conf.CarSize(2));


    TerminalWindow := Sub_Window(Number_Of_Lines=>Terminal_Size_Y, Number_Of_Columns=>Max_X,
                                    First_Line_Position=>Margin_Y, First_Column_Position=>Margin_X);

    RoadWindow := Sub_Window(Number_Of_Lines=>RoadSizeY+2, Number_Of_Columns=>RoadSizeX+2,
                                    First_Line_Position=>Terminal_Size_Y + 2*Margin_Y, First_Column_Position=>Margin_X);
                                    
    Road.Initialize(RoadSize, TerminalWindow, RoadWindow);

    -- Curses sometimes breaks layout without this delay
    delay 1.5;

    done := False;
    while not done loop
        
        c := Get_Keystroke(Standard_Window);
        case c is
            when Character'Pos ('q') => done := True;
            when Character'Pos ('Q') => done := True;
            when Key_Resize => done := True;
            when others => null;
        end case;

        Nap_Milli_Seconds (50);
    end loop;

    -- clear stuff
    Road.Stop;

    Visibility := Normal;
    Set_Cursor_Visibility (Visibility);
    End_Windows;
    Curses_Free_All;

    Put_Line("CTRL + C to terminate");

end PrzeGra;
