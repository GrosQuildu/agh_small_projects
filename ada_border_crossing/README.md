# agh_ada
Small project for concurrent programming in Ada

## Build'n'run
```bash
gprbuild przegra.gpr
./bin/przegra
```

The project make use of [ncurses](https://github.com/mirror/ncurses), so install it and possibly edit paths in przegra.gpr.

## Drive

Every car (PakCar.ads), booth at the border crossing (PakShed.ads) and cars spawn point (PakEntryPoint.ads) is an Ada task.

Car tasks are stored in a "free list", the rest are created at program beginning.

![Gif it!](/ada_border_crossing/przegra-record.gif)

(I will fix that gif someday)

Note that terminal resize will close the program.
