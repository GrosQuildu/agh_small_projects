
with Ada.Text_IO; use    Ada.Text_IO;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;

with Ada.Float_Text_IO; use Ada.Float_Text_IO;
with Ada.Exceptions; use Ada.Exceptions;

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Normal_Random; use Normal_Random;


package body PakRoad is

    procedure PrintCar(RoadWindow : Window; Y,X,ScaleY,ScaleX: Integer; CarString : Conf.RoadPrintableType) is
    begin
        Add(Win=>RoadWindow, Line=>Line_Position((Y-1)*ScaleY+1), Column=>Column_Position((X-1)*ScaleX+1), Str=>CarString(1));
        Add(Win=>RoadWindow, Line=>Line_Position((Y-1)*ScaleY+2), Column=>Column_Position((X-1)*ScaleX+1), Str=>CarString(2));
        Add(Win=>RoadWindow, Line=>Line_Position((Y-1)*ScaleY+3), Column=>Column_Position((X-1)*ScaleX+1), Str=>CarString(3));
        Add(Win=>RoadWindow, Line=>Line_Position((Y-1)*ScaleY+4), Column=>Column_Position((X-1)*ScaleX+1), Str=>CarString(4));
    end;

    protected body FreeList is
        procedure CreateOrGetCar(NewCarPtr : out CarPtr) is
            Node : Node_Access;
        begin
            if Free_List_Head = null then
                NewCarPtr := new Car;
            else
                Node := Free_List_Head;
                Free_List_Head := Free_List_Head.Next;
                NewCarPtr := Node.O;
            end if;
        end;

        procedure FreeCar(CarToFreePtr : CarPtr) is
            Node : Node_Access;
        begin
            if CarToFreePtr /= null then
                Node := new Node_Car;
                Node.O := CarToFreePtr;
                Node.Next := Free_List_Head;
                Free_List_Head := Node;
            end if;
        end;
    end FreeList;


    protected body Road is
        entry Initialize(RoadSize : Conf.RoadSizeType; TW, RW : Window) when Initialized = False is
        begin
            RoadWindow := RW;
            Box(RoadWindow);

            RoadTerminalWindow := TW;
            Box(RoadTerminalWindow);
            Refresh(RoadTerminalWindow);

            GatesPosition := Conf.GatesPosition;

            RoadCarArray := new CarArray(1..RoadSize(1), 1..RoadSize(2));
            RoadSheedsList := new ShedList(1..RoadSize(1));
            RoadEntryPointsList := new EntryPointsList(1..RoadSize(1));

            for Y in RoadCarArray'Range(1) loop
                for X in RoadCarArray'Range(2) loop
                    RoadCarArray(Y,X) := (Id=>Conf.CarIdEmpty, Printable=>Conf.Empty, Ptr=>null, Crashed=>False, Waiting=>False);
                    PrintCar(RoadWindow, Y,X, ScaleY, ScaleX, Conf.Empty);

                    if X = GatesPosition then
                        PrintCar(RoadWindow, Y,X, ScaleY, ScaleX, Conf.Shed);
                    end if;
                end loop;

                RoadSheedsList(Y) := new Shed;
                RoadEntryPointsList(Y) := new EntryPoint(Y, RoadCarArray'Length(2));
            end loop;

            Initialized := True;
        end;

        entry Stop when Initialized is
        begin
            for Y in RoadCarArray'Range(1) loop
                for X in RoadCarArray'Range(2) loop
                    RoadCarArray(Y,X).Crashed := True;
                end loop;
            end loop;

            Initialized := False;
        end;

        entry Show(Text : String; LineNo : Integer := 1) when Initialized is
        begin
            Add(RoadTerminalWindow, Line_Position(LineNo), 1, Text);
            Refresh(RoadTerminalWindow);
        end;

        entry NewCar(Y,X: Integer; NewCarObject: CarObject; CarSpawned: out Boolean) when Initialized is
        begin
            if RoadCarArray(Y, X).Ptr = null then
                CarCounter := CarCounter + 1;
                CarSpawned := True;
                RoadCarArray(Y, X) := NewCarObject;

                PrintCar(RoadWindow, Y,X, ScaleY, ScaleX, RoadCarArray(Y,X).Printable);
                Refresh(RoadWindow);

                Add(RoadTerminalWindow, 6, 1, "Cars so far: " & CarCounter'Img);
                Refresh(RoadTerminalWindow);
            else
                CarSpawned := False;
            end if;
        end;


        entry Move(Y,X: in out Integer; D: Direction; Stopped: in out Boolean; InsideShed: in out ShedPtr) when Initialized is
            NextY: Integer := Y + D(1);
            NextX: Integer := X + D(2);
        begin
            if NextY < 1 then
                NextY := 1;
            elsif NextY > RoadCarArray'Length(1) then
                NextY := RoadCarArray'Length(1);
            end if;

            if RoadCarArray(Y,X).Crashed or NextX = 0 then
                -- remove car from road
                RoadCarArray(Y,X).Ptr := null;
                PrintCar(RoadWindow, Y,X, ScaleY, ScaleX, Conf.Empty);
                Stopped := True;
            elsif RoadCarArray(NextY,NextX).Ptr /= null and RoadCarArray(NextY,NextX).Crashed = False then
                if RoadCarArray(NextY,NextX).Waiting then
                    -- WAIT in queue for shed (because next car is also waiting)
                    RoadCarArray(Y,X).Waiting := True;
                elsif X < RoadCarArray'Length(2) - 3 then
                    -- CRASH if not just spawned (not to crash cars early after spawn)
                    RoadCarArray(Y,X).Crashed := True;
                    RoadCarArray(Y,X).Printable := Conf.Crash;
                    PrintCar(RoadWindow, Y,X, ScaleY, ScaleX, Conf.Crash);

                    RoadCarArray(NextY,NextX).Crashed := True;
                    RoadCarArray(NextY,NextX).Printable := Conf.Crash;
                    PrintCar(RoadWindow, NextY,NextX, ScaleY, ScaleX, Conf.Crash);

                    CarCrashCounter := CarCrashCounter + 2;

                    Add(RoadTerminalWindow, 3, 1, "Crash, cars: " & String(RoadCarArray(Y,X).Id) & " and " & String(RoadCarArray(NextY,NextX).Id));
                    Add(RoadTerminalWindow, 4, 1, "Crashed cars total: " & CarCrashCounter'Img);
                    Refresh(RoadTerminalWindow);
                end if;
            elsif RoadCarArray(NextY,NextX).Ptr = null then
                -- NORMAL MOVE
                RoadCarArray(NextY,NextX) := RoadCarArray(Y,X);
                RoadCarArray(Y,X).Ptr := null;

                PrintCar(RoadWindow, Y,X, ScaleY, ScaleX, Conf.Empty);
                PrintCar(RoadWindow, NextY,NextX, ScaleY, ScaleX, RoadCarArray(NextY,NextX).Printable);

                if NextX = GatesPosition then
                    -- ENTRY SHED
                    RoadCarArray(NextY,NextX).Waiting := True;
                    InsideShed := RoadSheedsList(Y);
                    PrintCar(RoadWindow, NextY,NextX, ScaleY, ScaleX, Conf.ShedFull);
                elsif X = GatesPosition then
                    -- SHED LEAVED
                    PrintCar(RoadWindow, Y,X, ScaleY, ScaleX, Conf.Shed);
                elsif (NextX-1 > 0 and NextX-1 = GatesPosition) and then RoadCarArray(NextY, NextX-1).Ptr /= null then
                    -- after move, next car is in shed
                    RoadCarArray(Y,X).Waiting := True;
                end if;

                Y := NextY;
                X := NextX;
            end if;
            Refresh(RoadWindow);

            exception
                when Error: others =>
                    Put ("Unexpected exception in Road.Move: ");
                    Put_Line (Exception_Information(Error));
        end;

        function GetCarIdArray return CarIdArray is
            RoadCarIdArray: CarIdArray(RoadCarArray'Range(1), RoadCarArray'Range(2));
        begin
            for I in RoadCarArray'Range(1) loop
                for J in RoadCarArray'Range(2) loop
                    if J = GatesPosition then
                        -- shed
                        if RoadCarArray(I, GatesPosition).Ptr = null then
                            RoadCarIdArray(I, GatesPosition) := "she";
                        else
                            RoadCarIdArray(I, GatesPosition) := "SHE";
                        end if;
                    else
                        -- car or empty
                        if RoadCarArray(I, J).Ptr = null then
                            RoadCarIdArray(I, J) := "   ";
                        else
                            RoadCarIdArray(I, J) := RoadCarArray(I, J).Id;
                        end if;
                    end if;
                end loop;
            end loop;
            return RoadCarIdArray;
        end;
    end Road;

end PakRoad;
