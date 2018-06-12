
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Numerics.Float_Random; use Ada.Numerics.Float_Random;
with Ada.Numerics.Discrete_Random;
with Normal_Random; use Normal_Random;

with Ada.Real_Time; use Ada.Real_Time;
with Ada.Exceptions; use Ada.Exceptions;

with PakRoad; use PakRoad;

package body PakCar is
    task body Car is
        Y: Integer;
        X: Integer;
        CarId: CarIdType;
        ThisCarPtr : CarPtr;

        ToWait: Float;
        Stopped: Boolean;
        Crash: Boolean;
        ShedCarIsIn : ShedPtr;

        package RandDirection is new Ada.Numerics.Discrete_Random(Integer);
        DirectionSeed : RandDirection.Generator;
        DirectionDrawnId : Integer;
        DirectionDrawn : Direction;

        RandomNormal, Mu, Sigma: Float;
        Seed: Generator;

        Nastepny : Ada.Real_Time.Time;
        Okres : Ada.Real_Time.Time_Span;


    begin
        Reset(Seed);
        RandDirection.Reset(DirectionSeed);

        loop
            select
              accept StartDriving(ThisCarPtr : CarPtr; CarId: CarIdType; StartY, StartX: Integer) do
                X := StartX;
                Y := StartY;
                Car.CarId := CarId;
                Car.ThisCarPtr := ThisCarPtr;
              end;
            or
              terminate;
            end select;

            ToWait := 0.0;
            Stopped := False;
            Crash := False;
            ShedCarIsIn := null;

            Mu := Conf.CarsMeanSpeed;
            Sigma := Conf.CarsSigmaSpeed;

            RandomNormal := abs Normal_Distribution(Seed, Mu, Sigma);
            Okres := Ada.Real_Time.Milliseconds(Integer(RandomNormal * 1000.0));

            Nastepny := Ada.Real_Time.Clock;

            while not Stopped
            loop
                delay until Nastepny;
                Nastepny := Nastepny + Okres;

                DirectionDrawnId := RandDirection.Random(DirectionSeed) mod 6;
                if DirectionDrawnId < 3 then
                    DirectionDrawn := Forward;
                elsif DirectionDrawnId = 4 then
                    DirectionDrawn := Right;
                else
                    DirectionDrawn := Left;
                end if;

                Road.Move(Y, X, DirectionDrawn, Stopped, ShedCarIsIn);
                if ShedCarIsIn /= null then
                    ShedCarIsIn.ValidateCar;
                    ShedCarIsIn := null;
                    Nastepny := Ada.Real_Time.Clock + Okres;
                end if;
            end loop;

            -- the car was stopped, add to cars' free list
            FreeList.FreeCar(ThisCarPtr);
        end loop;

    exception
        when Error: others =>
            Put ("Unexpected exception in Car: ");
            Put_Line (Exception_Information(Error));
    end Car;
end PakCar;