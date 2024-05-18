with Fuzzy_Matcher;

package Pretty_Print_Result is

   procedure Print_Result (Result     : Fuzzy_Matcher.Entries_Vectors.Vector;
                           Name_Only  : Boolean;
                           Percentage : Natural);

   procedure Print_Statistic
     (Result : Fuzzy_Matcher.Entries_Vectors.Vector);

end Pretty_Print_Result;
