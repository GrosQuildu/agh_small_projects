
with System;
with PakCar; use PakCar;

package PakShed is

    task type Shed is
        pragma Priority (System.Default_Priority+3);
        entry ValidateCar;
    end Shed;

end PakShed;