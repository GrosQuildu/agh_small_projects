
with Ada.Text_IO; use Ada.Text_IO;

with Conf;
with PakRoad; use PakRoad;

package body PakShed is
    task body Shed is
        ToWait : Duration;
    begin
        loop
            select
                accept ValidateCar do
                    ToWait := Conf.ShedWaitingTime;
                    delay ToWait;
                end;
            or
                terminate;
            end select;
        end loop;
    end Shed;
end PakShed;