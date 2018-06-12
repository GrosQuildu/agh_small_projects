-- conf.ads


package Conf is
    -- Counting from 1 (left) to some size determined at runtime
    GatesPosition : Integer := 4;

    -- How long sheds validate cars
    ShedWaitingTime : Duration := 3.0;

    -- for normal distribution
    CarsMeanSpeed : Float := 0.5;
    CarsSigmaSpeed : Float := 0.3;

    -- min is 8
    Terminal_Size_Y: Integer := 8;


    type CarIdType is new String(1..3);
    CarIdEmpty : CarIdType := "   ";

    type RoadSizeType is array(1..2) of Integer;

    CarSize : array(1..2) of Integer := (4, 11);
    type RoadPrintableType is array(1..CarSize(1)) of String(1..CarSize(2));

    Empty : RoadPrintableType := (
        "           ",
        "           ",
        "           ",
        "___________"
    );

    Crash : RoadPrintableType := (
        "      ^    ",
        " ~*~!\@/~*~",
        "     /*\   ",
        "______~____"
    );

    Shed : RoadPrintableType := (
        "X  ___|-|  ",
        "  /______\ ",
        "  |O  _ O| ",
        "__|__|_|_|_"
    );

    ShedFull : RoadPrintableType := (
        "X  ___|-|  ",
        "  /______\ ",
        "  |x  _ x| ",
        "__|__|X|_|_"
    );

    CarBrand1 : RoadPrintableType := (
        "       __  ",
        "     _/__| ",
        "   ((_XXX| ",
        "____()__()_"
    );

    CarBrand2 : RoadPrintableType := (
        "     XXX   ",
        "     __    ",
        "  __/= |   ",
        "_/_o____o\_"
    );

    CarBrand3 : RoadPrintableType := (
        "    _      ",
        "  _/~|____ ",
        " [-|_|_XXX]",
        "_(_)___(_)_"
    );

    CarBrand4 : RoadPrintableType := (
        "      _    ",
        "  ___/_|__ ",
        " [_,_XXX,_]",
        "___o____o__"
    );

    CarBrand5 : RoadPrintableType := (
        "     _     ",
        " ===[_]__  ",
        "   /__XXX\ ",
        "__(o_o_o_o)"
    );

    CarBrands : array(1..5) of RoadPrintableType := (CarBrand1, CarBrand2, CarBrand3, CarBrand4, CarBrand5);

    type RoadPrintableReplaceXY is record
        Y: Integer;
        X: Integer;
    end record;
    type RoadPrintableReplaceType is array(1..CarBrands'Length) of RoadPrintableReplaceXY;

    -- Positions of XXX's that will be replace with random id
    CarIdPos : RoadPrintableReplaceType := ((3,7), (1,6), (3,7), (3, 6), (3,6));

end Conf;
