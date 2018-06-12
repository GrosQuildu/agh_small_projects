with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
 
package Normal_Random is
   function Normal_Distribution
            (  Seed  : Generator;
               Mu    : Float := 1.0;
               Sigma : Float := 0.5
            )  return Float;
end;