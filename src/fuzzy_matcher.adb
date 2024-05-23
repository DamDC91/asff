with Libadalang.Common;
with Libadalang.Iterators;
with Langkit_Support.Text;
with GNATCOLL.Damerau_Levenshtein_Distance;
with Ada.Containers;
with Ada.Strings.Unbounded;
with Ada.Characters.Handling;
with Damerau_Levenshtein_Matrix;
with Ada.Text_IO;

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

   function Get_Subtype_Indication_Name
     (T                : LAL.Subtype_Indication;
      Fully_Quallified : Boolean)
      return String;

   function Get_Anonymous_Type_Decl_Name
     (T                : LAL.Anonymous_Type_Decl;
      Fully_Quallified : Boolean)
      return String;

   function Get_Name (T                : LAL.Base_Type_Decl'Class;
                      Fully_Quallified : Boolean) return String
   is
      use Langkit_Support.Text;
   begin
      if T.Is_Null or else T.F_Name.Is_Null then
         return "";
      else
         declare
            Is_Standard_Type : constant Boolean :=
              LAL."=" (T.Unit, T.P_Standard_Unit);
         begin
            --  We don't want to prefix standard types with "standard."
            if Fully_Quallified and then (not Is_Standard_Type) then
               return To_Lower (Image (T.P_Fully_Qualified_Name));
            else
               return  To_Lower (Image (T.F_Name.Text));
            end if;
         end;
      end if;
   end Get_Name;

   function Get_Anonymous_Type_Decl_Name
     (T                : LAL.Anonymous_Type_Decl;
      Fully_Quallified : Boolean)
      return String
   is
      Type_Def : constant LAL.Type_Def := T.F_Type_Def;
   begin
      case Type_Def.Kind is
         when LALCO.Ada_Access_To_Subp_Def =>
            return
              (case Type_Def.As_Access_To_Subp_Def.F_Subp_Spec.F_Subp_Kind is
                  when LALCO.Ada_Subp_Kind_Procedure => "procedure",
                  when LALCO.Ada_Subp_Kind_Function => "function");
         when LALCO.Ada_Type_Access_Def =>
            return Get_Subtype_Indication_Name
              (Type_Def.As_Type_Access_Def.F_Subtype_Indication,
               Fully_Quallified);
         when others =>
            raise Constraint_Error with Type_Def.Kind_Name;
      end case;
   end Get_Anonymous_Type_Decl_Name;

   function Get_Type_Decl_Name (T                : LAL.Type_Decl;
                                Fully_Quallified : Boolean) return String
   is
   begin
      case T.Kind is
         when LALCO.Ada_Concrete_Type_Decl =>
            return Get_Name (T, Fully_Quallified);
         when LALCO.Ada_Formal_Type_Decl =>
            --  TODO: better handling of generic
            return Get_Name (T, Fully_Quallified);
         when LALCO.Ada_Anonymous_Type_Decl =>
            return Get_Anonymous_Type_Decl_Name (T.As_Anonymous_Type_Decl,
                                                 Fully_Quallified);
         when others =>
            raise Constraint_Error;
      end case;
   end Get_Type_Decl_Name;

   function Get_Subtype_Indication_Name
     (T                : LAL.Subtype_Indication;
      Fully_Quallified : Boolean)
      return String
   is
      Base_Type : constant LAL.Base_Type_Decl := T.P_Designated_Type_Decl;
   begin
      if Base_Type.Is_Null then
         return "";
      end if;

      case Base_Type.Kind is
         when LALCO.Ada_Type_Decl =>
            return Get_Type_Decl_Name
              (Base_Type.As_Type_Decl, Fully_Quallified);
         when LALCO.Ada_Base_Subtype_Decl =>
            return Get_Name (Base_Type, Fully_Quallified);
         when LALCO.Ada_Classwide_Type_Decl =>
            return Get_Name (Base_Type.F_Name.P_Basic_Decl.As_Base_Type_Decl,
                             Fully_Quallified);
         when LALCO.Ada_Incomplete_Type_Decl =>
            raise Constraint_Error;
         when LALCO.Ada_Protected_Type_Decl =>
            return Get_Name (Base_Type, Fully_Quallified);
         when LALCO.Ada_Task_Type_Decl =>
            return Get_Name (Base_Type, Fully_Quallified);
         when others =>
            raise Constraint_Error;
      end case;
   end Get_Subtype_Indication_Name;

   function Get_Type_Name (T                : LAL.Type_Expr;
                           Fully_Quallified : Boolean)
                       return String
   is
      use type LALCO.Ada_Node_Kind_Type;
   begin
      case T.Kind is
         when LALCO.Ada_Anonymous_Type =>
            return Get_Anonymous_Type_Decl_Name
              (T.As_Anonymous_Type.F_Type_Decl, Fully_Quallified);
         when LALCO.Ada_Subtype_Indication =>
            return Get_Subtype_Indication_Name
              (T.As_Subtype_Indication,
               Fully_Quallified);
         when LALCO.Ada_Synthetic_Type_Expr =>
            raise Constraint_Error;
         when LALCO.Ada_Enum_Lit_Synth_Type_Expr =>
            raise Constraint_Error;
         when others =>
            raise Constraint_Error;
      end case;
   end Get_Type_Name;

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
              (Args, +Get_Type_Name (P.F_Type_Expr, Fully_Qualified));
         end loop;
      end if;
      return Args;
   end Get_Arguments_Type_Name;

   function Get_Return_Type_Name (Spec : LAL.Subp_Spec;
                                  Fully_Qualified : Boolean)
                                  return String
   is
   begin
      if Spec.F_Subp_Returns.Is_Null then
         return "";
      else
         return Get_Type_Name (Spec.F_Subp_Returns, Fully_Qualified);
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
   begin
      Args := Get_Arguments_Type_Name
        (Spec,
         Search_Query.Use_Fully_Qualified);

      Fitness := Compute_Arguments_Fitness (Args, Search_Query.Arguments_Type);

      Fitness := Fitness +
        Fitness_Type (GNATCOLL.Damerau_Levenshtein_Distance
                      (Get_Return_Type_Name (Spec,
                         Search_Query.Use_Fully_Qualified),
                         -Search_Query.Returned_Type));
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

   procedure Process_Unit (Unit         : LAL.Analysis_Unit;
                           Search_Query : Search_Queries.Search_Query_Type;
                           Entries      : in out Entries_Vectors.Vector)
   is
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
   end Process_Unit;

   -----------
   -- Match --
   -----------

   function Match
     (Files        : Libadalang.Project_Provider.Filename_Vectors.Vector;
      Context      : Libadalang.Analysis.Analysis_Context;
      Search_Query : Search_Queries.Search_Query_Type)
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
            if Unit.Has_Diagnostics then
               for D of Unit.Diagnostics loop
                  Ada.Text_IO.Put_Line
                    (Ada.Text_IO.Standard_Error,
                     LAL.Format_GNU_Diagnostic (Unit, D));
               end loop;
            else
               Process_Unit (Unit, Search_Query, Entries);
            end if;
         end;
      end loop;

      Sort_Entries (Entries);
      return Entries;
   end Match;

end Fuzzy_Matcher;
