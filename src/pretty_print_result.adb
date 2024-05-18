with Ada.Text_IO;
with Langkit_Support.Text;
with Libadalang.Analysis;

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

   procedure Print_Result (Result     : Fuzzy_Matcher.Entries_Vectors.Vector;
                           Name_Only  : Boolean;
                           Percentage : Natural)
   is
      Percentage_Float : constant Float := 1.0 + (Float (Percentage) / 100.0);
        use type Fuzzy_Matcher.Fitness_Type;
      Last : constant Fuzzy_Matcher.Fitness_Type :=
        Fuzzy_Matcher.Fitness_Type (Float (Result.First_Element.Fitness)
                                    * Percentage_Float);
   begin
      for R of Result loop
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

   procedure Print_Statistic (Result : Fuzzy_Matcher.Entries_Vectors.Vector)
   is
      F : Ada.Text_IO.File_Type;
      FileName : constant String := "data.txt";
   begin
      Ada.Text_IO.Create (File => F,
                          Mode => Ada.Text_IO.Out_File,
                          Name => FileName);
      for R of Result loop
         Ada.Text_IO.Put_Line (F, R.Fitness'Img);
      end loop;
      Ada.Text_IO.Close (F);
   end Print_Statistic;

end Pretty_Print_Result;
