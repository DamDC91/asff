with Libadalang;
with Libadalang.Helpers;
with Libadalang.Analysis;
with GNATCOLL.Projects;
with Libadalang.Project_Provider;
with Ada.Text_IO;
with Ada.Strings.Unbounded;
with GNATCOLL.Opt_Parse;
with Search_Queries;
with Fuzzy_Matcher;
with Ada.Characters.Handling;

procedure Ada_Search is

   package Args is
      use GNATCOLL.Opt_Parse;
      use Ada.Strings.Unbounded;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Ada_Search helps you find the subprogram you need");

      package Query is new Parse_Positional_Arg
        (Parser,
         Name        => "Query",
         Arg_Type    => Unbounded_String,
         Help        => "Function  : ""(First_Arg_Type, Second_Arg_Type, ...) -> Returned_Type"" "
         & "Procedure : ""(First_Arg_Type, Second_Arg_Type, ...)""");

      package Number_Of_Match is new Parse_Option
        (Parser, "-n",
         Name        => "Number of match",
         Arg_Type    => Unbounded_String,
         Default_Val => Ada.Strings.Unbounded.To_Unbounded_String ("5"),
         Help        => "Display N match, default is 5");

      package Project_File is new Parse_Option
        (Parser, "-P", "--project",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Project file to use");

      package Files is new Parse_Option_List
        (Parser, "-f", "--files",
         Arg_Type    => Unbounded_String,
         Help        => "Files to analyze");

   end Args;

   package LAL renames Libadalang.Analysis;
begin
   if Args.Parser.Parse then

      declare
         use Ada.Strings.Unbounded;
         function "+" (S : Ada.Strings.Unbounded.Unbounded_String)
                       return String renames Ada.Strings.Unbounded.To_String;
         Gpr   : constant String := +Args.Project_File.Get;
         Query_Result : constant Search_Queries.Search_Query_Result_Type :=
           Search_Queries.Parse_Query (+Args.Query.Get);
         Tree  : GNATCOLL.Projects.Project_Tree_Access;
         Env   : GNATCOLL.Projects.Project_Environment_Access;
         Files : Libadalang.Project_Provider.Filename_Vectors.Vector;
         Ctx : LAL.Analysis_Context;
         Number_Of_Match_Str : constant String :=
           Ada.Strings.Unbounded.To_String (Args.Number_Of_Match.Get);
         Number_Of_Match   : Natural;

      begin

         if (for all C of Number_Of_Match_Str =>
               Ada.Characters.Handling.Is_Digit (C))
         then
            Number_Of_Match := Natural'Value (Number_Of_Match_Str);
         else
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, "-n must be a number");
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, Args.Parser.Help);
            return;
         end if;

         if not Query_Result.Valid then
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, "Invalid query");
            Ada.Text_IO.Put_Line
              (Ada.Text_IO.Standard_Error, Args.Parser.Help);
            return;
         end if;

         if Args.Project_File.Get /= "" then
            Libadalang.Helpers.Load_Project
              (Project_File => Gpr,
               Project => Tree,
               Env =>  Env);

            Ctx := LAL.Create_Context
              (Unit_Provider => Libadalang.Helpers.Project_To_Provider (Tree));
         else
            Ctx := LAL.Create_Context;
         end if;

         if not Args.Files."=" (Args.Files.Get, Args.Files.No_Results) then
            for File of Args.Files.Get loop
               Files.Append (File);
            end loop;
         elsif Args.Project_File.Get /= "" then
            Files := Libadalang.Project_Provider.Source_Files
              (Tree.all,
               Libadalang.Project_Provider.Root_Project);
         end if;

         declare
            Result : constant Fuzzy_Matcher.Entries_Vectors.Vector :=
              Fuzzy_Matcher.Match
                (Files => Files,
                 Context => Ctx,
                 Search_Query => Query_Result.Query,
                 Nb_Of_Match => Number_Of_Match);
         begin
            for R of Result loop
               Ada.Text_IO.Put_Line (Fuzzy_Matcher.Image (R));
            end loop;
         end;
      end;
   end if;

end Ada_Search;
