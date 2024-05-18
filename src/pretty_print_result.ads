with Fuzzy_Matcher;

package Pretty_Print_Result is

   procedure Print_Result (Results    : Fuzzy_Matcher.Entries_Vectors.Vector;
                           Name_Only  : Boolean;
                           Percentage : Natural);

   procedure Print_Statistic
     (Results   : Fuzzy_Matcher.Entries_Vectors.Vector;
      File_Name : String);

end Pretty_Print_Result;
