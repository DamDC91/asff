with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

generic
   Max_Size : Natural;
   with package String_Vector is new Ada.Containers.Vectors
     (Index_Type => <>,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "=" => <>);
package Damerau_Levenshtein_Matrix is

   type Damerau_Levenshtein_Distance_Type is new Natural;

   type Row_Index_Type is new Natural range 0 .. Max_Size;

   type Col_Index_Type is new Natural range 0 .. Max_Size;

   type Matrix_Type is array
     (Row_Index_Type range <>,
      Col_Index_Type range <>) of Damerau_Levenshtein_Distance_Type;

   function Compute_Matrix (L : String_Vector.Vector;
                            R : String_Vector.Vector)
                            return Matrix_Type;

   type Indices_Pair_Type is record
      Row : Row_Index_Type;
      Col : Col_Index_Type;
   end record;

   type Result_Index_Type is new Natural range 0 .. Max_Size;

   type Result_Indices_Type is array (Result_Index_Type range <>)
     of Indices_Pair_Type;

   function Compute_Minimun_Distance (Matrix : Matrix_Type)
                                      return Result_Indices_Type;

end Damerau_Levenshtein_Matrix;
