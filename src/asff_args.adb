with Ada.Text_IO;
with GNATCOLL.OS.FS;
with GNATCOLL.Opt_Parse;
with Ada.Containers;
with Ada.Command_Line;

package body Asff_Args is

   package Arguments is
      use GNATCOLL.Opt_Parse;
      use Ada.Strings.Unbounded;

      Parser : Argument_Parser := Create_Argument_Parser
        (Help => "Asff: Ada subprogram fuzzy finder");

      package Query is new Parse_Positional_Arg
        (Parser,
         Name        => "Query",
         Arg_Type    => Unbounded_String,
         Help        => "Function  : ""(First_Arg_Type, Second_Arg_Type, ...)"
         & "-> Returned_Type"" "
         & "Procedure : ""(First_Arg_Type, Second_Arg_Type, ...)""");

      package Limit_Percentage_Option is new Parse_Option
        (Parser,
         Long        => "--limit-percentage",
         Arg_Type    => Unbounded_String,
         Default_Val => Ada.Strings.Unbounded.To_Unbounded_String ("20"),
         Help        => "Display matches with scores above the given"
         & " percentage of the top score. Default value is 20%.");

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
         Help        => "Statistics file name.");

      package Project_File_Option is new Parse_Option
        (Parser, "-P", "--project",
         Arg_Type    => Unbounded_String,
         Default_Val => Null_Unbounded_String,
         Help        => "Project file to use");

      package Files_List_Option is new Parse_Option_List
        (Parser, "-f", "--files",
         Arg_Type    => Unbounded_String,
         Help        => "Files to analyze");

   end Arguments;

   ---------------------
   -- Parse_Arguments --
   ---------------------

   function Parse_Arguments (Success : out Boolean) return Argument_Record_Type
   is
      Null_FD : Ada.Text_IO.File_Type;
      Current_FD : constant Ada.Text_IO.File_Type :=
        Ada.Text_IO.Current_Output;
      Parsed_Args : GNATCOLL.Opt_Parse.Parsed_Arguments;
   begin

      if (for some I in 1 .. Ada.Command_Line.Argument_Count =>
            Ada.Command_Line.Argument (I) = "--help"
          or else Ada.Command_Line.Argument (I) = "-h")
      then
         return (Query => Ada.Strings.Unbounded.Null_Unbounded_String,
                 Limit_Percentage =>
                   Ada.Strings.Unbounded.Null_Unbounded_String,
                 Name_Only => False,
                 Version => False,
                 Help => True,
                 Statistics => Ada.Strings.Unbounded.Null_Unbounded_String,
                 Project => Ada.Strings.Unbounded.Null_Unbounded_String,
                 Files => Filename_Vectors.Empty);
      end if;

      Ada.Text_IO.Open (File => Null_FD,
                        Mode => Ada.Text_IO.Out_File,
                        Name => GNATCOLL.OS.FS.Null_File);
      Ada.Text_IO.Set_Output (Null_FD);
      Success := Arguments.Parser.Parse (Result => Parsed_Args);

      if Success then

         declare
            Array_Files : constant Arguments.Files_List_Option.Result_Array :=
              Arguments.Files_List_Option.Get (Parsed_Args);
            Vector_Files : Filename_Vectors.Vector := Filename_Vectors.Empty;
         begin

            for F of Array_Files loop
               Vector_Files.Append (F);
            end loop;

            Ada.Text_IO.Set_Output (Current_FD);
            Ada.Text_IO.Close (Null_FD);

            return (Query => Arguments.Query.Get (Parsed_Args),
                    Limit_Percentage =>
                      Arguments.Limit_Percentage_Option.Get (Parsed_Args),
                    Name_Only => Arguments.Name_Only_Flag.Get (Parsed_Args),
                    Version => Arguments.Version_Flag.Get (Parsed_Args),
                    Help => False,
                    Statistics =>
                      Arguments.Statistics_Option.Get (Parsed_Args),
                    Project => Arguments.Project_File_Option.Get (Parsed_Args),
                    Files => Vector_Files);
         end;
      else
         for I in 1 .. Ada.Command_Line.Argument_Count loop
            Ada.Text_IO.Put_Line (Ada.Command_Line.Argument (I));
         end loop;
         Ada.Text_IO.Set_Output (Current_FD);
         Ada.Text_IO.Close (Null_FD);
         return (Query => Ada.Strings.Unbounded.Null_Unbounded_String,
                 Limit_Percentage => Ada.Strings.Unbounded.Null_Unbounded_String,
                 Name_Only => False,
                 Version => Arguments.Version_Flag.Get (Parsed_Args),
                 Help => False,
                 Statistics => Ada.Strings.Unbounded.Null_Unbounded_String,
                 Project => Ada.Strings.Unbounded.Null_Unbounded_String,
                 Files => Filename_Vectors.Empty);
      end if;
   end Parse_Arguments;

   ---------------------
   -- Parse_Arguments --
   ---------------------

   function Help return String
   is
   begin
      return Arguments.Parser.Help;
   end Help;

end Asff_Args;
