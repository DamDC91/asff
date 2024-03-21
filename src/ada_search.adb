with Libadalang;
with Libadalang.Helpers;
with Libadalang.Analysis;
with Ada.Command_Line;
with GNATCOLL.Projects;
with Libadalang.Project_Provider;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Libadalang.Iterators;
with Ada.Containers.Vectors;
with Libadalang.Common;
with Langkit_Support.Text;
with GNATCOLL.Damerau_Levenshtein_Distance;

procedure Ada_Search is

   package LAL renames Libadalang.Analysis;
   package LALCO renames Libadalang.Common;
   Gpr   : constant String := Ada.Command_Line.Argument (1);
   Query : constant String := Ada.Command_Line.Argument (2);
   Tree  : GNATCOLL.Projects.Project_Tree_Access;
   Env   : GNATCOLL.Projects.Project_Environment_Access;
   Files : Libadalang.Project_Provider.Filename_Vectors.Vector;

   Ctx : LAL.Analysis_Context;

   type Subp_Entry_Type is record
      Node : LAL.Ada_Node;
      Name : Ada.Strings.Unbounded.Unbounded_String;
      File_Name : Ada.Strings.Unbounded.Unbounded_String;
      Distance : Natural;
   end record;

   package Subp_Entries_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Subp_Entry_Type);
   Subp_Entries : Subp_Entries_Vectors.Vector;
begin
   Libadalang.Helpers.Load_Project
     (Project_File => Gpr, Project => Tree, Env =>  Env);
   Files := Libadalang.Project_Provider.Source_Files
     (Tree.all, Libadalang.Project_Provider.Root_Project);
   Ctx := LAL.Create_Context
     (Unit_Provider => Libadalang.Helpers.Project_To_Provider (Tree));
   for File of Files loop
      declare
         File_Name : constant String := Ada.Strings.Unbounded.To_String (File);
         Unit : constant LAL.Analysis_Unit := LAL.Get_From_File (Ctx, File_Name);

      begin
         for N of Libadalang.Iterators.Find (Unit.Root, Libadalang.Iterators.Kind_Is (LALCO.Ada_Subp_Decl)).Consume loop
            declare
               Spec : constant LAL.Subp_Spec := N.As_Subp_Decl.F_Subp_Spec;
               Subp_Entry : Ada.Strings.Unbounded.Unbounded_String;
               First_Loop : Boolean := True;
               use Ada.Strings.Unbounded;
            begin

               Append (Subp_Entry, Langkit_Support.Text.Image (N.As_Subp_Decl.F_Subp_Spec.F_Subp_Name.F_Name.Text));
               if not N.As_Subp_Decl.F_Subp_Spec.F_Subp_Params.Is_Null then
                  for P of N.As_Subp_Decl.F_Subp_Spec.F_Subp_Params.F_Params loop
                     if First_Loop then
                        First_Loop := False;
                        Append (Subp_Entry, "(");
                     else
                        Append (Subp_Entry, ",");
                     end if;
                     Append (Subp_Entry, Langkit_Support.Text.Image (P.As_Param_Spec.F_Type_Expr.P_Type_Name.Text));
                  end loop;
               end if;
               Append (Subp_Entry, ")");
               if not N.As_Subp_Decl.F_Subp_Spec.F_Subp_Returns.Is_Null then
                  Append (Subp_Entry, Langkit_Support.Text.Image (N.As_Subp_Decl.F_Subp_Spec.F_Subp_Returns.P_Type_Name.Text));
               end if;

               Subp_Entries_Vectors.Append (Subp_Entries, (Node => N.As_Ada_Node,
                                                           Name => Subp_Entry,
                                                           File_Name => File,
                                                           Distance =>
                                                             GNATCOLL.Damerau_Levenshtein_Distance
                                                               (To_String (Subp_Entry),
                                                                Query)));
            end;
         end loop;
      end;
   end loop;

   declare
      function Compare (L, R : Subp_Entry_Type) return Boolean
      is
      begin
         return L.Distance < R.Distance;
      end Compare;

      package Sort is new Subp_Entries_Vectors.Generic_Sorting (Compare);
      use Ada.Strings.Unbounded;
   begin
      Sort.Sort (Subp_Entries);
      for E of Subp_Entries loop
         Ada.Text_IO.Put_Line (To_String (E.File_Name & ": " & E.Name));
      end loop;
   end;
end Ada_Search;
