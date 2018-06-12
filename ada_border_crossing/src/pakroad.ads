
with System;

with Conf;
with PakShed; use PakShed;
with PakCar; use PakCar;
with PakEntryPoint; use PakEntryPoint;

with Terminal_Interface.Curses; use Terminal_Interface.Curses;


package PakRoad is
    type Direction is array(1..2) of Integer;
    Forward: Direction := (0, -1);
    Left: Direction := (-1, -1);
    Right: Direction := (1, -1);

    type CarArray is array (Natural range <>, Natural range <>) of CarObject;
    type CarArrayPtr is access CarArray;
    type CarIdArray is array (Natural range <>, Natural range <>) of Conf.CarIdType;

    type ShedPtr is access all Shed;
    type ShedList is array (Natural range <>) of ShedPtr;
    type ShedListPtr is access ShedList;

    type EntryPointPtr is access all EntryPoint;
    type EntryPointsList is array (Natural range <>) of EntryPointPtr;
    type EntryPointsListPtr is access EntryPointsList;

    -- Cars' free list
    type Node_Car;
    type Node_Access is access Node_Car;

    type Node_Car is limited record
        O    : CarPtr;
        Next : Node_Access;
    end record;

    protected FreeList is
        procedure CreateOrGetCar(NewCarPtr : out CarPtr);
        procedure FreeCar(CarToFreePtr : CarPtr);
    private
        Free_List_Head : Node_Access := null;
    end FreeList;
    -- Cars' free list END

    protected Road is
        pragma Priority (System.Default_Priority+8);

        entry Initialize(RoadSize : Conf.RoadSizeType; TW, RW: Window);
        entry Stop;
        entry Move(Y,X: in out Integer; D: Direction; Stopped: in out Boolean; InsideShed: in out ShedPtr);
        entry NewCar(Y,X: Integer; NewCarObject: CarObject; CarSpawned: out Boolean);
        entry Show(Text : String; LineNo : Integer := 1);
        function GetCarIdArray return CarIdArray;

    private
        RoadCarArray : CarArrayPtr;
        RoadSheedsList: ShedListPtr;
        RoadEntryPointsList: EntryPointsListPtr;

        Initialized : Boolean := False;
        GatesPosition : Integer;
        CarCounter : Integer := 0;
        CarCrashCounter : Integer := 0;

        RoadWindow : Window;
        RoadTerminalWindow : Window;
        ScaleY : Integer := Conf.CarSize(1);
        ScaleX : Integer := Conf.CarSize(2);
    end Road;

    

end PakRoad;
