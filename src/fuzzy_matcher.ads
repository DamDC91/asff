with Search_Queries;
with Ada.Containers.Vectors;
with Libadalang.Analysis;
with Libadalang.Project_Provider;

package Fuzzy_Matcher is

   type Fitness_Type is new Integer;

   type Entry_Type is record
      Subp    : Libadalang.Analysis.Subp_Decl;
      Fitness : Fitness_Type;
   end record;

   function Image (E : Entry_Type) return String;

   package Entries_Vectors is new Ada.Containers.Vectors
     (Index_Type => Positive,
      Element_Type => Entry_Type);

   function Match
     (Files        : Libadalang.Project_Provider.Filename_Vectors.Vector;
      Context      : Libadalang.Analysis.Analysis_Context;
      Search_Query : Search_Queries.Search_Query_Type;
      Nb_Of_Match  : Positive)
      return Entries_Vectors.Vector;

end Fuzzy_Matcher;
