with Ada.Text_IO;
with Langkit_Support.Text;
with Libadalang.Analysis;
with Ada.Strings.Fixed;
with Ada.Exceptions;
with Ada.Command_Line;

package body Pretty_Print_Result is

   package LAL renames Libadalang.Analysis;

   function Image (E : Fuzzy_Matcher.Entry_Type) return String
   is
   begin
      return Langkit_Support.Text.Image (LAL.Full_Sloc_Image (E.Subp)) &
        Langkit_Support.Text.Encode (E.Subp.Text, "ASCII");
   end Image;

   function Image_Name_Only (E : Fuzzy_Matcher.Entry_Type) return String
   is
      Subp_Name : constant LAL.Defining_Name := E.Subp.F_Subp_Spec.F_Subp_Name;
   begin
      return Langkit_Support.Text.Image (LAL.Full_Sloc_Image (Subp_Name)) &
        Langkit_Support.Text.Encode (Subp_Name.Text, "ASCII");
   end Image_Name_Only;

   ------------------
   -- Print_Result --
   ------------------

   procedure Print_Result (Results    : Fuzzy_Matcher.Entries_Vectors.Vector;
                           Name_Only  : Boolean;
                           Percentage : Natural)
   is
      Percentage_Float : constant Float := 1.0 + (Float (Percentage) / 100.0);
      use type Fuzzy_Matcher.Fitness_Type;
      Last : constant Fuzzy_Matcher.Fitness_Type :=
        Fuzzy_Matcher.Fitness_Type (Float (Results.First_Element.Fitness)
                                    * Percentage_Float);
   begin
      for R of Results loop
         if R.Fitness > Last then
            exit;
         end if;
         if Name_Only then
            Ada.Text_IO.Put_Line (Image_Name_Only (R));
         else
            Ada.Text_IO.Put_Line (Image (R));
         end if;
      end loop;
   end Print_Result;

   ---------------------
   -- Print_Statistic --
   ---------------------

   procedure Print_Statistic (Results   : Fuzzy_Matcher.Entries_Vectors.Vector;
                              File_Name : String)
   is
      F : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Create (File => F,
                          Mode => Ada.Text_IO.Out_File,
                          Name => File_Name);
      for R of Results loop
         declare
            Fitness : constant String := Ada.Strings.Fixed.Trim
              (R.Fitness'Img, Ada.Strings.Both);
         begin
            Ada.Text_IO.Put_Line (F, Fitness & " " & Image_Name_Only (R));
         end;
      end loop;
      Ada.Text_IO.Close (F);
   exception
      when E : others =>
         Ada.Text_IO.Put_Line ("Fail to write statistics in " & File_Name &
                                 ": " & Ada.Exceptions.Exception_Message (E));
         Ada.Command_Line.Set_Exit_Status (1);
   end Print_Statistic;

end Pretty_Print_Result;
