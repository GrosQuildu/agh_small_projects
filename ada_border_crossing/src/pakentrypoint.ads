
with System;

package PakEntryPoint is

    task type EntryPoint(Y,X: Integer) is
        pragma Priority (System.Default_Priority+2);
    end EntryPoint;

end PakEntryPoint;