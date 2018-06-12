
with System;
With Conf; use Conf;
with Ada.Strings.Fixed;

package PakCar is
    type Car;
    type CarPtr is access all Car;

    task type Car is
        pragma Priority (System.Default_Priority+3);
        entry StartDriving(ThisCarPtr : CarPtr; CarId: CarIdType; StartY, StartX: Integer);
    end Car;

    type CarObject is record
        Id: CarIdType;
        Printable: RoadPrintableType;
        Ptr: CarPtr; 
        Crashed: Boolean;
        Waiting: Boolean;
    end record;

end PakCar;