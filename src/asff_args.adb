with GNATCOLL.Opt_Parse;
with Ada.Containers;

package body Asff_Args is

   package Opt_Args is
      use GNATCOLL.Opt_Parse;
      use Ada.Strings.Unbounded;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Asff: Ada subprogram fuzzy finder");

      package Query is new Parse_Option
        (Parser,
         Name        => "Query",
         Short       => "-q",
         Long        => "--query",
         Arg_Type    => Unbounded_String,
         Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String,
         Help        => "Fuzzing query using the following syntax:        "
         & "Function: (First_Type, Second_Type,...) "
         & "-> Returned_Type "
         & "Procedure: (First_Type, Second_Type,...)");

      package Limit_Percentage_Option is new Parse_Option
        (Parser,
         Long        => "--limit-percentage",
         Arg_Type    => Unbounded_String,
         Default_Val => Ada.Strings.Unbounded.To_Unbounded_String ("10"),
         Help        => "Display matches with scores above the given"
         & " percentage of the top score. Default value is 10%");

      package Name_Only_Flag is new Parse_Flag
        (Parser,
         Short => "",
         Long  => "--name-only",
         Help  => "Print only subprogram name");

      package Version_Flag is new Parse_Flag
        (Parser,
         Short => "",
         Long  => "--version",
         Help  => "Print version");

      package Statistics_Option is new Parse_Option
        (Parser,
         Long        => "--statistics",
         Arg_Type    => Unbounded_String,
         Default_Val => Ada.Strings.Unbounded.Null_Unbounded_String,
         Help        => "Statistics file name");

      package Project_File_Option is new Parse_Option
        (Parser, "-P", "--project",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Project file to use");

      package Files_List_Option is new Parse_Option_List
        (Parser, "-f", "--files",
         Arg_Type    => Unbounded_String,
         Help        => "Files to analyze");

      package Recursive_Flag is new Parse_Flag
        (Parser, "-U", "--recursive",
         Help =>
            "Process all units in the project tree, excluding externally"
         & " built projects");

   end Opt_Args;

   ---------------------
   -- Parse_Arguments --
   ---------------------

   function Parse_Arguments return Parsed_Arguments
   is
   begin
      if Opt_Args.Parser.Parse then
         declare
            Array_Files : constant Opt_Args.Files_List_Option.Result_Array
              := Opt_Args.Files_List_Option.Get;
            Vector_Files : Filename_Vectors.Vector := Filename_Vectors.Empty;
         begin
            for F of Array_Files loop
               Vector_Files.Append (F);
            end loop;

            return
              (Success => True,
               Args =>
                 (Query            => Opt_Args.Query.Get,
                  Limit_Percentage => Opt_Args.Limit_Percentage_Option.Get,
                  Name_Only        => Opt_Args.Name_Only_Flag.Get,
                  Version          => Opt_Args.Version_Flag.Get,
                  Statistics       => Opt_Args.Statistics_Option.Get,
                  Project          => Opt_Args.Project_File_Option.Get,
                  Files            => Vector_Files,
                  Recursive        => Opt_Args.Recursive_Flag.Get));
         end;
      else
         return (Success => False);
      end if;
   end Parse_Arguments;

   ---------------------
   -- Parse_Arguments --
   ---------------------

   function Help return String
   is
   begin
      return Opt_Args.Parser.Help;
   end Help;

end Asff_Args;
