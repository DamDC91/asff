pragma Ada_2022;

with Libadalang.Common;
with Libadalang.Iterators;
with Langkit_Support.Text;
with GNATCOLL.Damerau_Levenshtein_Distance;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Damerau_Levenshtein_Matrix;

package body Fuzzy_Matcher is

   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   package LALIT renames Libadalang.Iterators;

   function To_Lower (S : String) return String renames
     Ada.Characters.Handling.To_Lower;

   function "+" (S : String) return Ada.Strings.Unbounded.Unbounded_String
                 renames Ada.Strings.Unbounded.To_Unbounded_String;

   function "-" (S : Ada.Strings.Unbounded.Unbounded_String) return String
                 renames Ada.Strings.Unbounded.To_String;

   function Designated_Type_Decl (T : LAL.Type_Expr'Class)
                                  return LAL.Base_Type_Decl
   is
      D : LAL.Base_Type_Decl := T.P_Designated_Type_Decl;
      use type LALCO.Ada_Node_Kind_Type;
   begin
      if D.Kind = LALCO.Ada_Classwide_Type_Decl then
         D := D.F_Name.P_Basic_Decl.As_Base_Type_Decl;
      end if;
      return D;
   end Designated_Type_Decl;

   function Type_Name (T : LAL.Type_Expr) return String is
      Name : constant LAL.Unbounded_Text_Type_Array :=
        Designated_Type_Decl (T).P_Fully_Qualified_Name_Array;
      use Langkit_Support.Text;
   begin
         return To_Lower (Image (To_Text (Name (Name'Last))));
   end Type_Name;

   function Type_Full_Name (T : LAL.Type_Expr) return String is
     (To_Lower (Langkit_Support.Text.Image
      (Designated_Type_Decl (T).P_Fully_Qualified_Name)));

   function Get_Arguments_Type_Name
     (Spec : LAL.Subp_Spec;
      Fully_Qualified : Boolean)
      return Search_Queries.Arguments_Vectors.Vector
   is
      Args : Search_Queries.Arguments_Vectors.Vector :=
        Search_Queries.Arguments_Vectors.Empty;
   begin
      if not Spec.F_Subp_Params.Is_Null then
         for P of Spec.F_Subp_Params.F_Params loop
            Search_Queries.Arguments_Vectors.Append
              (Args,
               +(if Fully_Qualified then
                      Type_Full_Name (P.F_Type_Expr)
                 else Type_Name (P.F_Type_Expr)));
         end loop;
      end if;
      return Args;
   end Get_Arguments_Type_Name;

   function Get_Return_Type_Name (Spec : LAL.Subp_Spec;
                                  Fully_Qualified : Boolean)
                                  return Ada.Strings.Unbounded.Unbounded_String
   is
   begin
      if not Spec.F_Subp_Returns.Is_Null then
         return +(if Fully_Qualified then
                     Type_Full_Name (Spec.F_Subp_Returns)
                  else Type_Name (Spec.F_Subp_Returns));
      else
         return Ada.Strings.Unbounded.Null_Unbounded_String;
      end if;
   end Get_Return_Type_Name;

   function Compute_Arguments_Fitness
     (Subp_Args : Search_Queries.Arguments_Vectors.Vector;
      Search_Args : Search_Queries.Arguments_Vectors.Vector)
      return Fitness_Type
   is
      package Distance_Matrix is new Damerau_Levenshtein_Matrix
        (Max_Size => 50,
         String_Vector => Search_Queries.Arguments_Vectors);

      Matrix : constant Distance_Matrix.Matrix_Type :=
        Distance_Matrix.Compute_Matrix (Subp_Args, Search_Args);

      Indicies : constant Distance_Matrix.Result_Indices_Type :=
        Distance_Matrix.Compute_Minimun_Distance (Matrix);

      Fitness : Fitness_Type := 0;
      use type Ada.Containers.Count_Type;
      use Distance_Matrix;
   begin
      for I of Indicies loop
         Fitness := Fitness + Fitness_Type (Matrix (I.Row, I.Col));
      end loop;

      if Subp_Args.Length < Search_Args.Length then
         for J in 1 .. Search_Args.Length loop
            if (for all P of Indicies => Col_Index_Type (J) /= P.Col) then
               declare
                  use Ada.Strings.Unbounded;
                  Arg : constant Unbounded_String :=
                    Search_Args.Element (Positive (J));
               begin
                  Fitness := Fitness + Fitness_Type (Length (Arg));
               end;
            end if;
         end loop;

      elsif Subp_Args.Length > Search_Args.Length then
         for I in 1 .. Subp_Args.Length loop
            if (for all P of Indicies => Row_Index_Type (I) /= P.Row) then
               declare
                  use Ada.Strings.Unbounded;
                  Arg : constant Unbounded_String :=
                    Subp_Args.Element (Positive (I));
               begin
                  Fitness := Fitness + Fitness_Type (Length (Arg));
               end;
            end if;
         end loop;
      end if;

      return Fitness;
   end Compute_Arguments_Fitness;

   function Make_Entry (Subp : LAL.Subp_Decl;
                        Search_Query : Search_Queries.Search_Query_Type)
                        return Entry_Type
   is
      Args      : Search_Queries.Arguments_Vectors.Vector;
      Fitness   : Fitness_Type := 0;
      Spec      : constant LAL.Subp_Spec := Subp.F_Subp_Spec;
      use type LALCO.Ada_Subp_Kind;
   begin
      Args := Get_Arguments_Type_Name
        (Spec,
         Search_Query.Use_Fully_Qualified);

      Fitness := Compute_Arguments_Fitness (Args, Search_Query.Arguments_Type);
      case Spec.F_Subp_Kind is
         when LALCO.Ada_Subp_Kind_Function =>
            declare

            begin
               Fitness := Fitness +
                 Fitness_Type (GNATCOLL.Damerau_Levenshtein_Distance
                               (-Get_Return_Type_Name (Spec,
                                  Search_Query.Use_Fully_Qualified),
                                  -Search_Query.Returned_Type));
            end;

         when LALCO.Ada_Subp_Kind_Procedure =>
            null;
      end case;
      return (Subp => Subp,
              Fitness => Fitness);
   end Make_Entry;

   procedure Sort_Entries (Entries : in out Entries_Vectors.Vector)
   is
      function Compare (L, R : Entry_Type) return Boolean is
        (L.Fitness < R.Fitness);

      package Sorting is new Entries_Vectors.Generic_Sorting (Compare);
   begin
      Sorting.Sort (Entries);
   end Sort_Entries;

   -----------
   -- Image --
   -----------

   function Image (E : Entry_Type) return String
   is
   begin
      return Langkit_Support.Text.Image (LAL.Full_Sloc_Image (E.Subp)) &
        Langkit_Support.Text.Encode (E.Subp.Text, "ASCII");

   end Image;

   -----------
   -- Match --
   -----------

   function Match
     (Files        : Libadalang.Project_Provider.Filename_Vectors.Vector;
      Context      : Libadalang.Analysis.Analysis_Context;
      Search_Query : Search_Queries.Search_Query_Type;
      Nb_Of_Match : Positive)
      return Entries_Vectors.Vector
   is
      Entries : Entries_Vectors.Vector := Entries_Vectors.Empty;
   begin
      for File of Files loop
         declare
            File_Name : constant String :=
              Ada.Strings.Unbounded.To_String (File);
            Unit : constant LAL.Analysis_Unit :=
              LAL.Get_From_File (Context, File_Name);

         begin
            case Unit.Root.As_Compilation_Unit.P_Unit_Kind is
               when LALCO.Unit_Specification =>
                  for N of LALIT.Find (Unit.Root,
                                       LALIT.Kind_Is (LALCO.Ada_Subp_Decl))
                    .Consume
                  loop
                     declare
                        New_Entry : constant Entry_Type := Make_Entry
                          (N.As_Subp_Decl, Search_Query);
                     begin
                        Entries.Append (New_Entry);
                     end;
                  end loop;
               when LALCO.Unit_Body => null;
            end case;
         end;
      end loop;

      declare
         use type Ada.Containers.Count_Type;
      begin
         Sort_Entries (Entries);
         if Entries.Length > Ada.Containers.Count_Type (Nb_Of_Match) then
            Entries.Set_Length (Ada.Containers.Count_Type (Nb_Of_Match));
         end if;
      end;
      return Entries;
   end Match;

end Fuzzy_Matcher;
