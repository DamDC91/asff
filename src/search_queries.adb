with Ada.Strings.Fixed;
with GNATCOLL.Utils;
with Ada.Characters.Handling;

package body Search_Queries is

   Args_Beg : constant Character := '(';
   Args_End : constant Character := ')';
   Agrs_Sep : constant Character := ',';
   Ret_Sep  : constant String := "->";

   function To_Lower (S : String) return String renames
     Ada.Characters.Handling.To_Lower;

   function Is_Ada_Identifier (S : Ada.Strings.Unbounded.Unbounded_String)
                               return Boolean
   is
      function Predicate (C : Character) return Boolean
        is (C = '.' or else GNATCOLL.Utils.Is_Identifier (C));
   begin
      return GNATCOLL.Utils.Predicate
        (Ada.Strings.Unbounded.To_String (S), Predicate'Access);
   end Is_Ada_Identifier;

   function Contains_Dot (S : Ada.Strings.Unbounded.Unbounded_String)
                          return Boolean
   is (Ada.Strings.Unbounded.Count (S, ".") > 0);

   ---------------------
   -- Parse_Arguments --
   ---------------------

   function Parse_Arguments (Args    : String;
                             Success : out Boolean)
                             return Arguments_Vectors.Vector
   is
      Splited : constant GNATCOLL.Utils.Unbounded_String_Array :=
        GNATCOLL.Utils.Split (Str => To_Lower (Args),
                              On => Agrs_Sep,
                              Omit_Empty_Lines => False);
      Arguments : Arguments_Vectors.Vector := Arguments_Vectors.Empty
        (Splited'Length);
      use Ada.Strings.Unbounded;
   begin
      Success := True;
      for Arg of Splited loop
         if Arg = Null_Unbounded_String
           or else not Is_Ada_Identifier (Arg)
         then
            Success := False;
            exit;
         else
            Arguments_Vectors.Append (Arguments, Arg);
         end if;
      end loop;
      return Arguments;
   end Parse_Arguments;

   ------------------
   -- Parse_Query --
   ------------------

   function Parse_Query (Query : String)
                                return Search_Query_Result_Type
   is
      Arguments           : Arguments_Vectors.Vector;
      Returned            : Ada.Strings.Unbounded.Unbounded_String;
      Use_Fully_Qualified : Boolean;

      No_Space_Query : constant String := GNATCOLL.Utils.Replace
        (S => Query,
         Pattern => " ",
         Replacement => "");

      Start_Args : constant Integer := Ada.Strings.Fixed.Index
        (Source => No_Space_Query,
         Pattern => Args_Beg & "");
      End_Args : constant Integer := Ada.Strings.Fixed.Index
        (Source => No_Space_Query,
         Pattern => Args_End & "");
      Start_Ret : constant Integer := End_Args + Ret_Sep'Length + 1;
      Args_Success : Boolean;

   begin
      if Start_Args /= No_Space_Query'First or else End_Args = 0 then
         return (Valid => False,
                 Error_Msg => Ada.Strings.Unbounded.To_Unbounded_String
                   ("Query must start by '" & Args_Beg
                    & "' and having a closing '" & Args_End & "'"));
      end if;

      Arguments :=
        Parse_Arguments
          (Args => No_Space_Query (Start_Args + 1 .. End_Args - 1),
           Success => Args_Success);

      if not Args_Success then
         return (Valid => False,
                Error_Msg => Ada.Strings.Unbounded.To_Unbounded_String
                   ("Arguments type must be an Ada identifier. No 'access',"
                    & " ''class', 'in out'...)"));
      end if;

      if No_Space_Query'Last >= Start_Ret then
         if No_Space_Query (End_Args + 1 .. Start_Ret - 1) = Ret_Sep then
            declare
               Type_Name : constant String :=
                 To_Lower (No_Space_Query (Start_Ret .. No_Space_Query'Last));
            begin
               Returned := Ada.Strings.Unbounded.To_Unbounded_String
                 (Type_Name);
               if not Is_Ada_Identifier (Returned) then
                  return (Valid     => False,
                          Error_Msg =>
                            Ada.Strings.Unbounded.To_Unbounded_String
                              ("Returned type must be an Ada identifier."
                               & " No 'access', ''class'...)"));
               end if;
            end;
         else
            return (Valid => False,
                    Error_Msg => Ada.Strings.Unbounded.To_Unbounded_String
                      ("Only '" & Ret_Sep & "' can be found after ')'"));
         end if;
      elsif No_Space_Query'Last /= End_Args then
         return (Valid => False,
                 Error_Msg => Ada.Strings.Unbounded.To_Unbounded_String
                   ("Only '" & Ret_Sep & "' can be found after ')'"));
      end if;

      Use_Fully_Qualified := (for some Arg of Arguments =>
                                Contains_Dot (Arg))
        or else Contains_Dot (Returned);

      return (Valid => True,
              Query => (Arguments_Type => Arguments,
                        Returned_Type => Returned,
                        Use_Fully_Qualified => Use_Fully_Qualified));

   end Parse_Query;

end Search_Queries;
