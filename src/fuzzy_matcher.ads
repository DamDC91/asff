with Search_Queries;
with Ada.Containers.Vectors;
with Libadalang.Analysis;
with Libadalang.Project_Provider;

package Fuzzy_Matcher is

   type Similarity_Probability_Type is new Float range 0.0 .. 1.0;

   type Entry_Type is record
      Subp       : Libadalang.Analysis.Subp_Decl;
      Similarity : Similarity_Probability_Type;
   end record;

   function Image (E : Entry_Type) return String;

   package Entries_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Entry_Type);

   function Match
     (Files        : Libadalang.Project_Provider.Filename_Vectors.Vector;
      Context      : Libadalang.Analysis.Analysis_Context;
      Search_Query : Search_Queries.Search_Query_Type)
      return Entries_Vectors.Vector;

end Fuzzy_Matcher;
