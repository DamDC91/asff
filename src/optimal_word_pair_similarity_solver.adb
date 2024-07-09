with Ada.Numerics.Generic_Elementary_Functions;
with Ada.Assertions;

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
      J   : constant Row_Index_Type := M'Length (1);
      W   : constant Col_Index_Type := M'Length (2);
      Job : array (1 .. W + 1) of Row_Index_Type :=
        (others => Row_Index_Type (Max_Size));
      Ys  : array (1 .. J)  of Similarity_Db_Type := (others => 0.0);
      Yt  : array (Col_Index_Type range 1 .. W + 1) of Similarity_Db_Type :=
        (others => 0.0);
      Answers : array (1 .. J) of Similarity_Db_Type;
      Last_Answer : Row_Index_Type := Answers'First;
   begin
      Ada.Assertions.Assert (J <= Row_Index_Type (W));
      for J_Current in 1 .. J loop
         declare
            W_Current : Col_Index_Type := W + 1;
            Min_To : array (1 .. W + 1) of Similarity_Db_Type :=
              (others => Similarity_Db_Type'Last);
            Prv : array (1 .. W + 1) of Col_Index_Type :=
              (others => Col_Index_Type (Max_Size));
            In_Z : array (1 .. W + 1) of Boolean := (others => False);
         begin
            Job (W_Current) := J_Current;
            while Job (W_Current) /= Row_Index_Type (Max_Size) loop
               In_Z (W_Current) := True;
               declare
                  JJ : constant Row_Index_Type := Job (W_Current);
                  Deltaa : Similarity_Db_Type := Similarity_Db_Type'Last;
                  W_Next : Col_Index_Type := Col_Index_Type (Max_Size);
               begin
                  for WW in 1 .. W loop
                     if not In_Z (WW) then
                        if Ckmin (Min_To (WW),
                                  M (JJ + M'First (1) - 1,
                                     WW + M'First (2) - 1) - Ys (JJ) - Yt (WW))
                        then
                           Prv (WW) := W_Current;
                        end if;
                        if Ckmin (Deltaa, Min_To (WW)) then
                           W_Next := WW;
                        end if;
                     end if;
                  end loop;
                  for WW in 1 .. W + 1 loop
                     if In_Z (WW) then
                        Ys (Job (WW)) := Ys (Job (WW)) + Deltaa;
                        Yt (WW) := Yt (WW) - Deltaa;
                     else
                        Min_To (WW) := Min_To (WW) - Deltaa;
                     end if;
                  end loop;
                  W_Current := W_Next;
               end;

            end loop;

            declare
               WW : Col_Index_Type := Col_Index_Type (Max_Size);
            begin
               while W_Current /= Job'Last loop
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
            if Job (I) /= Row_Index_Type (Max_Size) then
               Pairs (L) :=
                 (Row => Job (I),
                  Col => I);
               L := L + 1;
            end if;
         end loop;
         return Pairs;
      end;
   end Hungarian;

end Optimal_Word_Pair_Similarity_Solver;
