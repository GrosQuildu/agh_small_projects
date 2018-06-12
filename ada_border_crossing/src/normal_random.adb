with Ada.Numerics;                       use Ada.Numerics;
with Ada.Numerics.Float_Random;          use Ada.Numerics.Float_Random;
with Ada.Numerics.Elementary_Functions;  use Ada.Numerics.Elementary_Functions;
 
package body Normal_Random is
   function Normal_Distribution
            (  Seed  : Generator;
               Mu    : Float := 1.0;
               Sigma : Float := 0.5
            )  return Float is 
   begin
      return Mu + (Sigma * Sqrt (-2.0 * Log (Random (Seed), 10.0)) * Cos (2.0 * Pi * Random (Seed)));
   end Normal_Distribution;

end Normal_Random;