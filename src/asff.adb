with Libadalang.Helpers;
with Libadalang.Analysis;
with GNATCOLL.Projects;
with Libadalang.Project_Provider;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with Search_Queries;
with Fuzzy_Matcher;
with Ada.Characters.Handling;
with Pretty_Print_Result;
with Asff_Config;
with Asff_Args;
with Ada.Unchecked_Conversion;
with GNAT.OS_Lib;
with GNAT.Traceback.Symbolic;
with Ada.Exceptions;

procedure Asff is

   package LAL renames Libadalang.Analysis;

   procedure Print_Version_And_Exit with No_Return
   is
      use Asff_Config;
   begin
      Ada.Text_IO.Put_Line (Crate_Name & " (" & Alire_Host_OS & " " &
                              Alire_Host_Arch & ")" & " " & Crate_Version);
      GNAT.OS_Lib.OS_Exit (0);
   end Print_Version_And_Exit;

   procedure Print_Error_And_Exit (Error_Msg   : String;
                                   Print_Help  : Boolean;
                                   Status_Code : Integer := 1)
     with No_Return
   is
   begin
      if Error_Msg'Length /= 0 then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error,
            "ERROR: " & Error_Msg);
      end if;
      if Print_Help then
         Ada.Text_IO.Put_Line
           (Ada.Text_IO.Standard_Error, Asff_Args.Help);
      end if;
      GNAT.OS_Lib.OS_Exit (Status_Code);
   end Print_Error_And_Exit;

   function Is_Empty (S : Ada.Strings.Unbounded.Unbounded_String)
                      return Boolean
   is
     (Ada.Strings.Unbounded.Length (S) = 0);

   function Conversion is new Ada.Unchecked_Conversion
     (Source => Asff_Args.Filename_Vectors.Vector,
      Target => Libadalang.Project_Provider.Filename_Vectors.Vector);

begin

   declare
      Parsed_Args : constant Asff_Args.Parsed_Arguments :=
        Asff_Args.Parse_Arguments;
   begin

      if Parsed_Args.Success then

         declare
            function To_Str (S : Ada.Strings.Unbounded.Unbounded_String)
                          return String
                          renames Ada.Strings.Unbounded.To_String;
            Args : constant Asff_Args.Arguments := Parsed_Args.Args;
            Query_Result : constant Search_Queries.Search_Query_Result_Type :=
              Search_Queries.Parse_Query (To_Str (Args.Query));
            Should_Print_Statistics : constant Boolean :=
              not Is_Empty (Args.Statistics);
            Percentage : Natural;
         begin

            if Args.Version then
               Print_Version_And_Exit;
            end if;

            if (for all C of To_Str (Args.Limit_Percentage) =>
                  Ada.Characters.Handling.Is_Digit (C))
            then
               Percentage := Natural'Value (To_Str (Args.Limit_Percentage));
               if Percentage > 100 then
                  Print_Error_And_Exit (Error_Msg =>
                                       "Limit percentage maximum value is 100",
                                     Print_Help => True);
               end if;
            else
               Print_Error_And_Exit (Error_Msg =>
                                       "Limit percentage must be a number",
                                     Print_Help => True);
            end if;

            if not Is_Empty (Args.Query) and then not Query_Result.Valid then
               Print_Error_And_Exit (Error_Msg =>
                                       To_Str (Query_Result.Error_Msg),
                                     Print_Help => True);
            end if;

            declare
               Tree  : GNATCOLL.Projects.Project_Tree_Access;
               Env   : GNATCOLL.Projects.Project_Environment_Access;
               Files : Libadalang.Project_Provider.Filename_Vectors.Vector;
               Ctx : LAL.Analysis_Context;
            begin
               if not Is_Empty (Args.Project) then
                  Libadalang.Helpers.Load_Project
                    (Project_File => To_Str (Args.Project),
                     Project      => Tree,
                     Env          =>  Env);

                  Ctx := LAL.Create_Context
                    (Unit_Provider =>
                       Libadalang.Helpers.Project_To_Provider (Tree));
               else
                  Ctx := LAL.Create_Context;
               end if;

               if not Asff_Args.Filename_Vectors.Is_Empty (Args.Files) then
                  Files := Conversion (Args.Files);
               elsif not Is_Empty (Args.Project) then
                  Files := Libadalang.Project_Provider.Source_Files
                    (Tree.all,
                     Libadalang.Project_Provider.Whole_Project_With_Runtime);
               else
                  Print_Error_And_Exit (Error_Msg =>
                                          "Sources files or project file " &
                                          "must be provided",
                                        Print_Help => True);
               end if;

               declare
                  Result : constant Fuzzy_Matcher.Entries_Vectors.Vector :=
                    Fuzzy_Matcher.Match
                      (Files        => Files,
                       Context      => Ctx,
                       Search_Query => Query_Result.Query);
               begin
                  Pretty_Print_Result.Print_Result
                    (Results    => Result,
                     Name_Only  => Args.Name_Only,
                     Percentage => Pretty_Print_Result.Percentage_Type
                       (Percentage));

                  if Should_Print_Statistics then
                     Pretty_Print_Result.Print_Statistic
                       (Results   => Result,
                        File_Name => To_Str (Args.Statistics));
                  end if;
               end;
            end;
         end;
      end if;
   end;

exception
   when E : others =>
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            Ada.Exceptions.Exception_Name (E) & ":" &
                              Ada.Exceptions.Exception_Message (E));

      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error,
                            GNAT.Traceback.Symbolic.Symbolic_Traceback (E));
      GNAT.OS_Lib.OS_Exit (1);
end Asff;
