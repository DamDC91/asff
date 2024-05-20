with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Asff_Args is

   package Filename_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "="          => Ada.Strings.Unbounded."=");

   type Argument_Record_Type is record
      Query : Ada.Strings.Unbounded.Unbounded_String;
      Limit_Percentage : Ada.Strings.Unbounded.Unbounded_String;
      Name_Only : Boolean;
      Version : Boolean;
      Help : Boolean;
      Statistics : Ada.Strings.Unbounded.Unbounded_String;
      Project : Ada.Strings.Unbounded.Unbounded_String;
      Files   : Filename_Vectors.Vector;
   end record;

   function Parse_Arguments (Success : out Boolean)
                             return Argument_Record_Type;

   function Help return String;

end Asff_Args;
