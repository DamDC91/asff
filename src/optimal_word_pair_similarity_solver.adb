package body Optimal_Word_Pair_Similarity_Solver is

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   --------------------
   -- Compute_Matrix --
   --------------------

   function Compute_Similarity_Matrix
     (L : String_Vector.Vector; R : String_Vector.Vector) return Matrix_Type
   is
      Matrix : Matrix_Type (1 .. Row_Index_Type (String_Vector.Length (L)),
                            1 .. Col_Index_Type (String_Vector.Length (R)));
   begin
      for I in Matrix'Range (1) loop
         for J in Matrix'Range (2) loop
            declare
               S1 : constant String := -L.Element
                 (String_Vector.Index_Type (I));
               S2 : constant String := -R.Element
                 (String_Vector.Index_Type (J));
            begin
               Matrix (I, J) := Compure_Similarity (S1, S2);
            end;
         end loop;
      end loop;
      return Matrix;
   end Compute_Similarity_Matrix;

   ------------------------------
   -- Compute_Minimun_Distance --
   ------------------------------

   function Find_Optimal_Pairs (Matrix : Matrix_Type)
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
            Max_I : Row_Index_Type;
            Max_J : Col_Index_Type;
            Max   : Similarity_Score_Type :=
              Similarity_Score_Type'First;
            pragma Warnings
              (Off,
               """Result_Indices"" may be referenced before it has a value");
         begin
            for I in Matrix'Range (1) loop
               for J in Matrix'Range (2) loop

                  if Matrix (I, J) >= Max and then
                    (for all P in Result_Indices'First .. Step - 1 =>
                       I /= Result_Indices (P).Row and then
                     J /= Result_Indices (P).Col)
                  then
                     Max_I := I;
                     Max_J := J;
                     Max   := Matrix (I, J);
                  end if;
               end loop;
            end loop;
            Result_Indices (Step) := (Row => Max_I, Col => Max_J);
         end;
         pragma Warnings
           (On, """Result_Indices"" may be referenced before it has a value");
      end loop;
      return Result_Indices;
   end Find_Optimal_Pairs;

end Optimal_Word_Pair_Similarity_Solver;
