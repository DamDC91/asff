with Fuzzy_Matcher;

package Pretty_Print_Result is

   type Percentage_Type is new Integer range 0 .. 100;

   procedure Print_Result (Results    : Fuzzy_Matcher.Entries_Vectors.Vector;
                           Name_Only  : Boolean;
                           Percentage : Percentage_Type);

   procedure Print_Statistic
     (Results   : Fuzzy_Matcher.Entries_Vectors.Vector;
      File_Name : String);

end Pretty_Print_Result;
