
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Float_Text_IO; use Ada.Float_Text_IO;

with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Exceptions; use Ada.Exceptions;
with Ada.Strings.Fixed;  use Ada.Strings.Fixed;

With Conf;
with Normal_Random; use Normal_Random;
with PakRoad; use PakRoad;
with PakCar; use PakCar;

package body PakEntryPoint is

    task body EntryPoint is
        NewCarId: Conf.CarIdType;
        NewCarPrintable : Conf.RoadPrintableType;
        NewCarObject: CarObject;

        NewCarBrandIndex : Integer;

        NewCarPtr: CarPtr := null;
        CarSpawned : Boolean;

        RandomNormal, Mu, Sigma: Float;
        Seed: Generator;

        Letters : String(1..36) := "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
        subtype LetterRange is Integer range 1..Letters'Length;
        package CarIdRand is new Ada.Numerics.Discrete_Random(LetterRange);
        CarIdGen: CarIdRand.Generator;

        Nastepny : Ada.Real_Time.Time;
        Okres : Ada.Real_Time.Time_Span;


    begin
        CarIdRand.Reset(CarIdGen);
        Reset(Seed);
        Mu := Random(Seed) * 1.0;
        Sigma := Random(Seed) * 2.0;
        Road.Show("Started EntryPoint (" & Y'Img & " ): mu = " & Mu'Img & ", sigma = " & Sigma'Img);

        Nastepny := Ada.Real_Time.Clock;

        loop
            delay until Nastepny;
            RandomNormal := abs Normal_Distribution(Seed, Mu, Sigma);
            Okres := Ada.Real_Time.Milliseconds(Integer(RandomNormal * 1000.0) + 15);
            Nastepny := Nastepny + Okres;
            Road.Show("EntryPoint (" & Y'Img & " ) spawns car. Next car in" & RandomNormal'Img & " seconds      ");

            if NewCarPtr = null then
                NewCarId := (Letters(CarIdRand.Random(CarIdGen)),Letters(CarIdRand.Random(CarIdGen)),Letters(CarIdRand.Random(CarIdGen)));

                NewCarBrandIndex := CarIdRand.Random(CarIdGen) mod Conf.CarBrands'Length + 1;
                NewCarPrintable := Conf.CarBrands(NewCarBrandIndex);
                Replace_Slice(NewCarPrintable(Conf.CarIdPos(NewCarBrandIndex).Y), Conf.CarIdPos(NewCarBrandIndex).X,
                                                Conf.CarIdPos(NewCarBrandIndex).X+3, String(NewCarId));

                FreeList.CreateOrGetCar(NewCarPtr);
                NewCarObject := (Id=>NewCarId, Printable=>NewCarPrintable, Ptr=>NewCarPtr, Crashed=>False, Waiting=>False);
            end if;

            Road.NewCar(Y,X, NewCarObject, CarSpawned);
            if CarSpawned then
                NewCarPtr.StartDriving(NewCarPtr, NewCarId, Y, X);
                NewCarPtr := null;
            end if;
        end loop;

    exception
        when Error: others =>
            Put ("Unexpected exception in EntryPoint: " & Y'Img);
            Put_Line (Exception_Information(Error));
    end EntryPoint;

end PakEntryPoint;