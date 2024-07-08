with Ada.Numerics.Generic_Elementary_Functions;

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




   function Transposition (M : Matrix_Type) return Matrix_Type
   is
      M_T : Matrix_Type
        (Row_Index_Type (M'First (2)) .. Row_Index_Type (M'Last (2)),
         Col_Index_Type (M'First (1)) .. Col_Index_Type (M'Last (1)));
   begin
      for I in M'Range (1) loop
         for Y in M'Range (2) loop
            M_T (Row_Index_Type (Y), Col_Index_Type (I)) := M (I, Y);
         end loop;
      end loop;
      return M_T;
   end Transposition;

   function Log (M : in Matrix_Type) return Matrix_Bd_Type
   is
      package Elem_Func is new Ada.Numerics.Generic_Elementary_Functions
        (Float_Type => Similarity_Db_Type);
      Log_M : Matrix_Bd_Type (M'First (1) .. M'Last (1),
                           M'First (2) .. M'Last (2));
   begin
      for I in M'Range (1) loop
         for Y in M'Range (2) loop
            Log_M (I, Y) := Elem_Func.Log (Base => 10.0,
                                           X    =>
                                             Similarity_Db_Type (M (I, Y)));
         end loop;
      end loop;
      return Log_M;
   end Log;

   function Ckmin (A : in out Similarity_Db_Type;
                   B :        Similarity_Db_Type) return Boolean
   is
   begin
      if B < A then
         A := B;
         return True;
      end if;
      return False;
   end Ckmin;

   function Hungarian (M : in Matrix_Bd_Type) return Result_Indices_Type
   is
      J   : constant Natural := M'Length (1);
      W   : constant Natural := M'Length (2);
      Job : array (0 .. W) of Integer := (others => -1);
      ys  : array (0 .. J - 1) of Similarity_Db_Type := (others => 0.0);
      yt  : array (0 .. W) of Similarity_Db_Type := (others => 0.0);
      Answers : array (0 .. J - 1) of Similarity_Db_Type;
      Last_Answer : Integer := Answers'First;
   begin
      for J_Current in 0 .. J - 1 loop
         declare
            W_Current : Integer := W;
            Min_To : array (0 .. W) of Similarity_Db_Type :=
              (others => Similarity_Db_Type'Last);
            Prv : array (0 .. W) of Integer := (others => -1);
            In_Z : array (0 .. W) of Boolean := (others => False);
         begin
            Job (W_Current) := J_Current;
            while Job (W_Current) /= -1 loop
               In_Z (W_Current) := True;
               declare
                  JJ : constant Integer := Job (W_Current);
                  Deltaa : Similarity_Db_Type := Similarity_Db_Type'Last;
                  W_Next : Integer := -1;
               begin
                  for WW in 0 .. W -1 loop
                     if not In_Z (WW) then
                        if Ckmin (Min_To (WW), M (Row_Index_Type (JJ) + M'First (1), Col_Index_Type (WW) + M 'First (2)) - Ys (JJ) - Yt (WW)) then
                           Prv (WW) := W_Current;
                        end if;
                        if Ckmin (deltaa, Min_To (WW)) then
                           W_Next := WW;
                        end if;
                     end if;
                  end loop;
                  for WW in 0 .. W loop
                     if In_Z (WW) then
                        Ys (Job (WW)) := Ys (Job (WW)) + deltaa;
                        Yt (WW) := Yt (WW) - deltaa;
                     else
                        Min_To (WW) := Min_To (WW) - deltaa;
                     end if;
                  end loop;
                  W_Current := W_Next;
               end;
            end loop;

            declare
               WW : Integer := -1;
            begin
               while W_Current /= W loop
                  WW := Prv (W_Current);
                  Job (W_Current) := Job (WW);
                  W_Current := WW;
               end loop;
            end;
            Answers (Last_Answer) := -Yt (W);
            Last_Answer := Last_Answer + 1;
         end;
      end loop;

      declare
         Pairs : Result_Indices_Type (1 .. Result_Index_Type (J));
         L : Result_Index_Type := Pairs'First;
      begin
         for I in Job'First .. Job'Last - 1 loop
            if Job (I) /= -1 then
               Pairs (L) :=
                 (Row => Row_Index_Type (Job (I)) + M'First (1),
                  Col => Col_Index_Type (I) + M'First (2));
               L := L + 1;
            end if;
         end loop;
         return Pairs;
      end;
   end Hungarian;

end Optimal_Word_Pair_Similarity_Solver;
