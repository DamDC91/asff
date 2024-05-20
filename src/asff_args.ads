with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Asff_Args is

   package Filename_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "="          => Ada.Strings.Unbounded."=");

   type Arguments is record
      Query            : Ada.Strings.Unbounded.Unbounded_String;
      Limit_Percentage : Ada.Strings.Unbounded.Unbounded_String;
      Name_Only        : Boolean;
      Version          : Boolean;
      Statistics       : Ada.Strings.Unbounded.Unbounded_String;
      Project          : Ada.Strings.Unbounded.Unbounded_String;
      Files            : Filename_Vectors.Vector;
   end record;

   type Parsed_Arguments (Success : Boolean) is record
      case Success is
         when True =>
            Args : Arguments;
         when False =>
            null;
      end case;
   end record;

   function Parse_Arguments return Parsed_Arguments;

   function Help return String;

end Asff_Args;
