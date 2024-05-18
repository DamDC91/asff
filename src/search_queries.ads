with Ada.Strings.Unbounded;
with Ada.Containers.Vectors;

package Search_Queries is

   package Arguments_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Ada.Strings.Unbounded.Unbounded_String,
      "="          => Ada.Strings.Unbounded."=");

   type Search_Query_Type is record
      Arguments_Type      : Arguments_Vectors.Vector;
      Returned_Type       : Ada.Strings.Unbounded.Unbounded_String;
      Use_Fully_Qualified : Boolean;
   end record;

   type Search_Query_Result_Type (Valid : Boolean) is record
      case Valid is
         when True =>
            Query : Search_Query_Type;
         when False =>
            Error_Msg : Ada.Strings.Unbounded.Unbounded_String;
      end case;
   end record;

   function Parse_Query (Query : String)
                         return Search_Query_Result_Type;

end Search_Queries;
