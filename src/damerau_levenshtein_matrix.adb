with GNATCOLL.Damerau_Levenshtein_Distance;

package body Damerau_Levenshtein_Matrix is

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   --------------------
   -- Compute_Matrix --
   --------------------

   function Compute_Matrix
     (L : String_Vector.Vector; R : String_Vector.Vector) return Matrix_Type
   is
      Matrix : Matrix_Type (1 .. Row_Index_Type (String_Vector.Length (L)),
                            1 .. Col_Index_Type (String_Vector.Length (R)));
   begin
      for I in Matrix'Range (1) loop
         for J in Matrix'Range (2) loop
            Matrix (I, J) :=
              Damerau_Levenshtein_Distance_Type
                (GNATCOLL.Damerau_Levenshtein_Distance
                   (-L.Element (String_Vector.Index_Type (I)),
                    -R.Element (String_Vector.Index_Type (J))));
         end loop;
      end loop;
      return Matrix;
   end Compute_Matrix;

   ------------------------------
   -- Compute_Minimun_Distance --
   ------------------------------

   function Compute_Minimun_Distance (Matrix : Matrix_Type)
                                      return Result_Indices_Type
   is
      Lowest_Dim : constant Natural :=
        (if Matrix'Last (1) < Row_Index_Type (Matrix'Last (2))
         then Natural (Matrix'Last (1))
         else Natural (Matrix'Last (2)));

      Result_Indices : Result_Indices_Type
        (1 .. Result_Index_Type (Lowest_Dim));
   begin

      for Step in Result_Index_Type range
        Result_Indices'First .. Result_Index_Type (Lowest_Dim)
      loop
         declare
            Min_I : Row_Index_Type;
            Min_J : Col_Index_Type;
            Min   : Damerau_Levenshtein_Distance_Type :=
              Damerau_Levenshtein_Distance_Type'Last;
         begin
            for I in Matrix'Range (1) loop
               for J in Matrix'Range (2) loop
                  if Matrix (I, J) < Min and then
                    (for all P in Result_Indices'First .. Step - 1 =>
                       I /= Result_Indices (P).Row and then
                     J /= Result_Indices (P).Col)
                  then
                     Min_I := I;
                     Min_J := J;
                     Min   := Matrix (I, J);
                  end if;
               end loop;
            end loop;
            Result_Indices (Step) := (Row => Min_I, Col => Min_J);
         end;
      end loop;
      return Result_Indices;
   end Compute_Minimun_Distance;

end Damerau_Levenshtein_Matrix;
