--  SPDX-License-Identifier: Apache-2.0
--
--  Copyright (c) 2018 - 2019 Joakim Strandberg <joakim@mequinox.se>
--
--  Licensed under the Apache License, Version 2.0 (the "License");
--  you may not use this file except in compliance with the License.
--  You may obtain a copy of the License at
--
--      http://www.apache.org/licenses/LICENSE-2.0
--
--  Unless required by applicable law or agreed to in writing, software
--  distributed under the License is distributed on an "AS IS" BASIS,
--  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--  See the License for the specific language governing permissions and
--  limitations under the License.

with Ada.Characters.Latin_1;

--  Known unsupported issues: Escaping of text (for example &amp;)
--  The stack roof may be hit if the comments and texts in the XML are HUGE.
--  It should not be an issue in general.
procedure Aida.XML_SAX_Parse
  (Argument    : in out Argument_Type;
   Contents    : String;
   Call_Result : in out Aida.Call_Result)
is
   package L1 renames Ada.Characters.Latin_1;

   type Initial_State_Id is
     (Less_Sign,
      Question_Mark,
      X,
      XM,
      XML,
      XML_S,
      XML_S_V,
      XML_S_VE,
      XML_S_VER,
      XML_S_VERS,
      XML_S_VERSI,
      XML_S_VERSIO,
      XML_S_VERSION,
      XML_S_VERSION_E,
      XML_S_VERSION_E_Q,
      XML_S_VERSION_E_Q_1,
      XML_S_VERSION_E_Q_1_P,
      XML_S_VERSION_E_Q_1_P_0,
      XML_S_VERSION_E_Q_1_P_0_Q,
      XML_S_VERSION_E_Q_1_P_0_Q_S,
      XML_S_VERSION_E_Q_1_P_0_Q_S_E,
      XML_S_VERSION_E_Q_1_P_0_Q_S_EN,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q,
      XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM,
      --  QM is short for Question Mark

      End_State);

   type State_Id_Type is
     (Expecting_NL_Sign_Or_Space_Or_Less_Sign, -- NL = New Line
      Init_Found_Less_Sign,
      --  First start tag has not yet been found

      Init_Found_Less_Followed_By_Exclamation_Sign,
      --  First start tag has not yet been found

      Init_Found_Less_Followed_By_Exclamation_And_Dash_Sign,
      --  First start tag has not yet been found

      Extracting_Start_Tag_Name,
      Expecting_G_Sign_Or_Attributes,
      Expecting_G_Sign_Or_Attributes_And_Found_Slash,
      Extracting_Attribute_Name,
      Expecting_Attribute_Value_Quotation_Mark,
      Extracting_Attribute_Value,
      Expecting_New_Tag_Or_Extracting_Tag_Value,
      --  Or start of comment or start- tag or end-tag

      Expecting_New_Tag_Or_Tag_Value_And_Found_L,
      Expecting_Only_Trailing_Spaces, Extracting_End_Tag_Name,
      New_Tag_Or_Tag_Value_And_L_And_Excl_And_Dash,
      --  ED is short for Exclamation and Dash
      --
      --  Enumeration values introduced to handle <!CDATA[--]]>

      New_Tag_Or_Tag_Value_And_Found_L_And_Exclamation,
      New_Tag_Or_Tag_Value_But_Expecting_C,
      New_Tag_Or_Tag_Value_But_Expecting_CD,
      New_Tag_Or_Tag_Value_But_Expecting_CDA,
      New_Tag_Or_Tag_Value_But_Expecting_CDAT,
      New_Tag_Or_Tag_Value_But_Expecting_CDATA,
      New_Tag_Or_Tag_Value_But_Expecting_CDATA_And_SB,
      --  SB is short for Square bracket

      Extracting_CDATA, Extracting_CDATA_Found_Square_Bracket,
      Extracting_CDATA_Found_Two_Square_Brackets, Init_Extracting_Comment,
      --  First start tag has not yet been found

      Init_Extracting_Comment_And_Found_Dash,
      --  First start tag has not yet been found

      Init_Extracting_Comment_And_Found_Dash_Dash,
      --  First start tag has not yet been found

      Extracting_Comment,
      Extracting_Comment_And_Found_Dash,
      Extracting_Comment_And_Found_Dash_Dash);

   type Expected_Quotation_Symbol_T is
     (Single_Quotes, -- Example: 'hello'
      Double_Quotes  -- Example: "hello"
     );

   function Is_Special_Symbol (CP : Character) return Boolean is
     (CP in '<' | '>' | '/' | '"');

   XML_IDENTIFIER_ERROR_1 : constant Integer := -1913564897;
   XML_IDENTIFIER_ERROR_2 : constant Integer := -0537097086;

   subtype P_T is Integer range Contents'First .. Contents'Last + 4;
   subtype Prev_P_T is Integer range Contents'First + 1 .. Contents'Last;

   procedure Analyze_XML (P : in out P_T) with
     Global => (In_Out => (Call_Result, Argument),
                Input => Contents),
     Pre =>
       (not Call_Result.Has_Failed and P > Contents'First and
          P <= Contents'Last and Contents'Last < Integer'Last - 4);

   procedure Analyze_XML (P : in out P_T) is
      Depth : Natural := 0;

      State_Id : State_Id_Type := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

      subtype Prev_Prev_P_T is Integer range
        Contents'First + 0 .. Contents'Last;

      subtype Contents_Index_T is Integer range
        Contents'First .. Contents'Last;

      CP : Character;

      Prev_P      : Prev_P_T := P;
      Prev_Prev_P : Prev_Prev_P_T; -- := Prev_P;

      Start_Tag_Name_First_Index : Contents_Index_T := Prev_P;
      Start_Tag_Name_Last_Index  : Contents_Index_T := Prev_P;

      Tag_Value_First_Index : Contents_Index_T := Contents'First;
      Tag_Value_Last_Index  : Contents_Index_T := Contents'First;

      End_Tag_Name_First_Index : Contents_Index_T := Contents'First;
      End_Tag_Name_Last_Index  : Contents_Index_T;

      Attribute_First_Index : Contents_Index_T := Prev_P;
      Attribute_Last_Index  : Contents_Index_T := Prev_P;

      Attribute_Value_First_Index : Contents_Index_T := Prev_P;
      Attribute_Value_Last_Index  : Contents_Index_T;

      Comment_First_Index : Contents_Index_T := Prev_P;

      --            Shall_Ignore_Tag_Value : Boolean := False;
      --            Shall_Ignore_Tag_Value : Boolean;

      --            Shall_Ignore_Until_Next_Quotation_Mark : Boolean := False;

      Expected_Quotation_Symbol : Expected_Quotation_Symbol_T := Double_Quotes;
   begin
      if P in Contents'Range
      then
         CP := Contents (P);
         P := P + 1;

         if CP = '>' then
            while P <= Contents'Last loop
               Prev_Prev_P := Prev_P;

               Prev_P := P;

               if P not in Contents'Range then
                  Call_Result.Initialize (1434797854, -0068724898);
                  exit;
               end if;

               CP := Contents (P);
               P := P + 1;

               pragma Loop_Variant (Increases => P);
               pragma Loop_Invariant (not Call_Result.Has_Failed);
               pragma Loop_Invariant (P <= Contents'Last + 4);
               pragma Loop_Invariant (Prev_Prev_P < Prev_P and Prev_P < P);
               pragma Loop_Invariant
                 (State_Id /= Extracting_Attribute_Name or
                    (State_Id = Extracting_Attribute_Name
                     and then (Attribute_First_Index < P)));

               --  Aida.Text_IO.Put ("Extracted:");
               --  Aida.Text_IO.Put (Image (CP));
               --  Aida.Text_IO.Put (", state ");
               --  Aida.Text_IO.Put_Line
               --     (String_T (State_Id_Type'Image (State_Id)));
               --  Aida.Text_IO.Put (Image (CP));

               case State_Id is
                  when Expecting_NL_Sign_Or_Space_Or_Less_Sign =>
                     if CP in ' ' | L1.LF | L1.CR | L1.HT then
                        null; -- Normal
                     elsif CP = '<' then
                        State_Id := Init_Found_Less_Sign;
                     else
                        Call_Result.Initialize (1003548980, 1714289304);
                        exit;
                     end if;
                  when Init_Found_Less_Sign =>
                     if CP = '!' then
                        State_Id :=
                          Init_Found_Less_Followed_By_Exclamation_Sign;
                     elsif CP = '/' then
                        if Depth = 0 then
                           Call_Result.Initialize (-1797161339, -1801650669);
                           exit;
                        end if;

                        if P > Contents'Last then
                           Call_Result.Initialize (0386434633, -1112825058);
                           exit;
                        end if;

                        Text (Argument, "", Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        State_Id                 := Extracting_End_Tag_Name;
                        End_Tag_Name_First_Index := P;
                     elsif not Is_Special_Symbol (CP) then
                        State_Id := Extracting_Start_Tag_Name;
                        Start_Tag_Name_First_Index := Prev_P;

                        pragma Assert (Start_Tag_Name_First_Index < P);
                     else
                        Call_Result.Initialize (1448645964, 0183871387);
                        exit;
                     end if;
                  when Init_Found_Less_Followed_By_Exclamation_Sign =>
                     if CP = '-' then
                        State_Id :=
                         Init_Found_Less_Followed_By_Exclamation_And_Dash_Sign;
                     else
                        Call_Result.Initialize (1915807131, 1377704704);
                        exit;
                     end if;
                  when Init_Found_Less_Followed_By_Exclamation_And_Dash_Sign =>
                     if CP = '-' then
                        State_Id := Init_Extracting_Comment;

                        Comment_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);
                     else
                        Call_Result.Initialize (-1302785225, -0551230956);
                        exit;
                     end if;
                  when Extracting_Start_Tag_Name =>
                     if CP = ' ' then
                        Start_Tag_Name_Last_Index := Prev_Prev_P;

                        Start_Tag
                          (Argument,
                           Contents
                             (Start_Tag_Name_First_Index ..
                                  Start_Tag_Name_Last_Index),
                           Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth < Integer'Last then
                           Depth := Depth + 1;
                        else
                           Call_Result.Initialize (-0197127393, -1788002976);
                           exit;
                        end if;

                        State_Id := Expecting_G_Sign_Or_Attributes;
                     elsif CP = '>' then
                        Start_Tag_Name_Last_Index := Prev_Prev_P;

                        Start_Tag
                          (Argument,
                           Contents
                             (Start_Tag_Name_First_Index ..
                                  Start_Tag_Name_Last_Index),
                           Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth < Integer'Last then
                           Depth := Depth + 1;
                        else
                           Call_Result.Initialize (0133265230, -0905163379);
                           exit;
                        end if;

                        Tag_Value_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);

                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     elsif Is_Special_Symbol (CP) then
                        Call_Result.Initialize (-0192291225, -1709997324);
                        exit;
                     end if;
                  when Expecting_G_Sign_Or_Attributes =>
                     if CP in ' ' | L1.LF | L1.CR | L1.HT then
                        null; -- Normal
                     elsif CP = '>' then
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;

                        if P > Contents'Last then
                           Call_Result.Initialize (-1521899768, -0725554341);
                           exit;
                        end if;

                        Tag_Value_First_Index := P;
                     elsif CP = '/' then
                        State_Id :=
                    Expecting_G_Sign_Or_Attributes_And_Found_Slash;
                     elsif not Is_Special_Symbol (CP) then
                        Attribute_First_Index := Prev_P;
                        State_Id              := Extracting_Attribute_Name;
                     else
                        Call_Result.Initialize (-0429878843, 1344381718);
                        exit;
                     end if;
                  when Expecting_G_Sign_Or_Attributes_And_Found_Slash =>
                     if CP = '>' then
                        State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;

                        Text (Argument, "", Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        --  End_Tag_Name_Last_Index := Prev_Prev_P;

                        End_Tag
                          (Argument,
                           Contents
                             (Start_Tag_Name_First_Index ..
                                  Start_Tag_Name_Last_Index),
                           Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth > 0 then
                           Depth := Depth - 1;
                        else
                           Call_Result.Initialize (0766893447, -0197942014);
                           exit;
                        end if;

                        Tag_Value_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);
                     else
                        Call_Result.Initialize (1180086532, 1745903660);
                        exit;
                     end if;
                  when Extracting_Attribute_Name =>
                     if CP = '=' then
                        Attribute_Last_Index := Prev_Prev_P;
                        State_Id := Expecting_Attribute_Value_Quotation_Mark;
                     elsif CP = L1.LF or CP = L1.CR then
                        Call_Result.Initialize (-0986469701, -0000005525);
                        exit;
                     elsif not Is_Special_Symbol (CP) then
                        null; -- Normal
                     else
                        Call_Result.Initialize (0819713752, 1428867079);
                        exit;
                     end if;
                  when Expecting_Attribute_Value_Quotation_Mark =>
                     if CP = '"' then
                        Expected_Quotation_Symbol := Double_Quotes;

                        Attribute_Value_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);
                        State_Id := Extracting_Attribute_Value;
                     elsif CP = ''' then
                        Expected_Quotation_Symbol   := Single_Quotes;
                        Attribute_Value_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);
                        State_Id := Extracting_Attribute_Value;
                     else
                        Call_Result.Initialize (0240833721, 0455771309);
                        exit;
                     end if;
                  when Extracting_Attribute_Value =>
                     if
                       (CP = '"' and
                            Expected_Quotation_Symbol = Double_Quotes) or
                         (CP = ''' and
                                Expected_Quotation_Symbol = Single_Quotes)
                     then
                        Attribute_Value_Last_Index := Prev_Prev_P;
                        State_Id := Expecting_G_Sign_Or_Attributes;
                        declare
                           Name : constant String :=
                             Contents
                               (Attribute_First_Index .. Attribute_Last_Index);
                           Value : constant String :=
                             Contents
                               (Attribute_Value_First_Index ..
                                  Attribute_Value_Last_Index);
                        begin
                           Attribute
                             (Argument, Name, Value,
                              Call_Result);
                        end;

                        if Call_Result.Has_Failed then
                           exit;
                        end if;
                     elsif CP = '<' or CP = '&' then
                        --  https://www.w3.org/TR/REC-xml/#NT-AttValue
                        Call_Result.Initialize (0587945467, 1683764896);
                        exit;
                     end if;
                  when Expecting_New_Tag_Or_Extracting_Tag_Value =>
                     --  if CP = Character'Pos ('"') then
                     --     Shall_Ignore_Until_Next_Quotation_Mark :=
                     --         not Shall_Ignore_Until_Next_Quotation_Mark;
                     if CP = '<' then
                        --   if not Shall_Ignore_Until_Next_Quotation_Mark then
                        State_Id :=
                          Expecting_New_Tag_Or_Tag_Value_And_Found_L;
                        Tag_Value_Last_Index := Prev_Prev_P;

                        Text
                          (Argument,
                           Contents
                             (Tag_Value_First_Index .. Tag_Value_Last_Index),
                           Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        --                     end if;
                     end if;
                  when Expecting_New_Tag_Or_Tag_Value_And_Found_L =>
                     if CP = '/' then
                        if P > Contents'Last then
                           Call_Result.Initialize (-1635958681, 2091153567);
                           exit;
                        end if;

                        State_Id := Extracting_End_Tag_Name;

                        End_Tag_Name_First_Index := P;
                     elsif CP = '!' then
                        State_Id :=
                          New_Tag_Or_Tag_Value_And_Found_L_And_Exclamation;
                     elsif Is_Special_Symbol (CP) then
                        Call_Result.Initialize (-0115323975, -1084437773);
                        exit;
                     else
                        --  Will start parsing child tag!
                        State_Id := Extracting_Start_Tag_Name;
                        Start_Tag_Name_First_Index := Prev_P;
                     end if;
                  when New_Tag_Or_Tag_Value_And_Found_L_And_Exclamation =>
                     if CP = '[' then
                        State_Id :=
                          New_Tag_Or_Tag_Value_But_Expecting_C;
                     elsif CP = '-' then
                        State_Id :=
                          New_Tag_Or_Tag_Value_And_L_And_Excl_And_Dash;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_C =>
                     if CP = 'C' then
                        State_Id :=
                          New_Tag_Or_Tag_Value_But_Expecting_CD;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_CD =>
                     if CP = 'D' then
                        State_Id :=
                          New_Tag_Or_Tag_Value_But_Expecting_CDA;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_CDA =>
                     if CP = 'A' then
                        State_Id :=
                          New_Tag_Or_Tag_Value_But_Expecting_CDAT;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_CDAT =>
                     if CP = 'T' then
                        State_Id :=
                          New_Tag_Or_Tag_Value_But_Expecting_CDATA;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_CDATA =>
                     if CP = 'A' then
                        State_Id :=
                          New_Tag_Or_Tag_Value_But_Expecting_CDATA_And_SB;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when New_Tag_Or_Tag_Value_But_Expecting_CDATA_And_SB =>
                     if CP = '[' then
                        State_Id              := Extracting_CDATA;
                        Tag_Value_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Extracting_CDATA =>
                     if CP = ']' then
                        Tag_Value_Last_Index := Prev_Prev_P;
                        State_Id := Extracting_CDATA_Found_Square_Bracket;
                     end if;
                  when Extracting_CDATA_Found_Square_Bracket =>
                     if CP = ']' then
                        State_Id := Extracting_CDATA_Found_Two_Square_Brackets;
                     else
                        State_Id := Extracting_CDATA;
                     end if;
                  when Extracting_CDATA_Found_Two_Square_Brackets =>
                     if CP = '>' then
                        CDATA
                          (Argument,
                           Contents
                             (Tag_Value_First_Index .. Tag_Value_Last_Index),
                           Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Value_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     else
                        State_Id := Extracting_CDATA;
                     end if;
                  when Extracting_End_Tag_Name =>
                     if CP = '>' then

                        End_Tag_Name_Last_Index := Prev_Prev_P;

                        End_Tag
                          (Argument,
                           Contents
                             (End_Tag_Name_First_Index ..
                                  End_Tag_Name_Last_Index),
                           Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        if Depth > 0 then
                           Depth := Depth - 1;
                        else
                           Call_Result.Initialize (-0534201701, -0614895498);
                           exit;
                        end if;

                        if Depth = 0 then
                           State_Id := Expecting_Only_Trailing_Spaces;
                        else
                           State_Id :=
                             Expecting_New_Tag_Or_Extracting_Tag_Value;
                        end if;

                        Tag_Value_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);

                     elsif CP = L1.LF or CP = L1.CR then
                        Call_Result.Initialize (-1658791000, 1638125646);
                        exit;
                     elsif Is_Special_Symbol (CP) then
                        Call_Result.Initialize (1726646144, -0779212513);
                        exit;
                     end if;
                  when New_Tag_Or_Tag_Value_And_L_And_Excl_And_Dash =>
                     if CP = '-' then
                        Comment_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);
                        State_Id := Extracting_Comment;
                     else
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     end if;
                  when Init_Extracting_Comment =>
                     if CP = '-' then
                        State_Id := Init_Extracting_Comment_And_Found_Dash;
                     end if;
                  when Init_Extracting_Comment_And_Found_Dash =>
                     if CP = '-' then
                        State_Id :=
                          Init_Extracting_Comment_And_Found_Dash_Dash;
                     else
                        State_Id := Init_Extracting_Comment;
                     end if;
                  when Init_Extracting_Comment_And_Found_Dash_Dash =>
                     if CP = '>' then
                        Comment
                          (Argument,
                           Value => Contents (Comment_First_Index .. (P - 4)),
                           Call_Result => Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Value_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);
                        State_Id := Expecting_NL_Sign_Or_Space_Or_Less_Sign;
                     else
                        State_Id := Init_Extracting_Comment;
                     end if;
                  when Extracting_Comment =>
                     if CP = '-' then
                        State_Id := Extracting_Comment_And_Found_Dash;
                     end if;
                  when Extracting_Comment_And_Found_Dash =>
                     if CP = '-' then
                        State_Id := Extracting_Comment_And_Found_Dash_Dash;
                     else
                        State_Id := Extracting_Comment;
                     end if;
                  when Extracting_Comment_And_Found_Dash_Dash =>
                     if CP = '>' then
                        Comment
                          (Argument,
                           Value => Contents (Comment_First_Index .. (P - 4)),
                           Call_Result => Call_Result);

                        if Call_Result.Has_Failed then
                           exit;
                        end if;

                        Tag_Value_First_Index :=
                          (if P <= Contents'Last then P else Contents'Last);
                        State_Id := Expecting_New_Tag_Or_Extracting_Tag_Value;
                     else
                        State_Id := Init_Extracting_Comment;
                     end if;
                  when Expecting_Only_Trailing_Spaces =>
                     if CP = ' ' or CP = L1.LF or CP = L1.CR then
                        null; -- Trailing spaces are OK
                     else
                        Call_Result.Initialize (1777504526, -1635825641);
                        exit;
                     end if;
               end case;
            end loop;

            if not Call_Result.Has_Failed
              and then State_Id /= Expecting_Only_Trailing_Spaces
            then
               Call_Result.Initialize (-1968500370, -1627762655);
            end if;
         else
            Call_Result.Initialize
              (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
         end if;
      else
         Call_Result.Initialize (-1672429119, -1233854200);
      end if;
   end Analyze_XML;

   State_Id : Initial_State_Id := Less_Sign;

   P : P_T := Contents'First;

   CP : Character;
begin
   while P <= Contents'Last loop
      exit when State_Id = End_State;

      if P not in Contents'Range then
         Call_Result.Initialize (-0356399774, -0280059910);
         exit;
      end if;

      CP := Contents (P);
      P := P + 1;

      pragma Loop_Variant (Increases => P);
      pragma Loop_Invariant (not Call_Result.Has_Failed);
      pragma Loop_Invariant
        (State_Id /= Less_Sign or
           (State_Id = Less_Sign
            and then (P > Contents'First and
                  Contents'Last >= Contents'First)));
      pragma Loop_Invariant
        (State_Id /= Question_Mark or
           (State_Id = Question_Mark
            and then
              (P > Contents'First + 1 and
                   Contents'Last >= Contents'First + 1)));
      pragma Loop_Invariant
        (State_Id /= X or
           (State_Id = X
            and then
              (P > Contents'First + 2 and
                   Contents'Last >= Contents'First + 2)));
      pragma Loop_Invariant
        (State_Id /= XM or
           (State_Id = XM
            and then
              (P > Contents'First + 3 and
                   Contents'Last >= Contents'First + 3)));
      pragma Loop_Invariant
        (State_Id /= XML or
           (State_Id = XML
            and then
              (P > Contents'First + 4 and
                   Contents'Last >= Contents'First + 4)));
      pragma Loop_Invariant
        (State_Id /= XML_S or
           (State_Id = XML_S
            and then
              (P > Contents'First + 5 and
                   Contents'Last >= Contents'First + 5)));
      pragma Loop_Invariant
        (State_Id /= XML_S_V or
           (State_Id = XML_S_V
            and then
              (P > Contents'First + 6 and
                   Contents'Last >= Contents'First + 6)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VE or
           (State_Id = XML_S_VE
            and then
              (P > Contents'First + 7 and
                   Contents'Last >= Contents'First + 7)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VER or
           (State_Id = XML_S_VER
            and then
              (P > Contents'First + 8 and
                   Contents'Last >= Contents'First + 8)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VERS or
           (State_Id = XML_S_VERS
            and then
              (P > Contents'First + 9 and
                   Contents'Last >= Contents'First + 9)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VERSI or
           (State_Id = XML_S_VERSI
            and then
              (P > Contents'First + 10 and
                   Contents'Last >= Contents'First + 10)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VERSIO or
           (State_Id = XML_S_VERSIO
            and then
              (P > Contents'First + 11 and
                   Contents'Last >= Contents'First + 11)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VERSION or
           (State_Id = XML_S_VERSION
            and then
              (P > Contents'First + 12 and
                   Contents'Last >= Contents'First + 12)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VERSION_E or
           (State_Id = XML_S_VERSION_E
            and then
              (P > Contents'First + 13 and
                   Contents'Last >= Contents'First + 13)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VERSION_E_Q or
           (State_Id = XML_S_VERSION_E_Q
            and then
              (P > Contents'First + 14 and
                   Contents'Last >= Contents'First + 14)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VERSION_E_Q_1 or
           (State_Id = XML_S_VERSION_E_Q_1
            and then
              (P > Contents'First + 15 and
                   Contents'Last >= Contents'First + 15)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VERSION_E_Q_1_P or
           (State_Id = XML_S_VERSION_E_Q_1_P
            and then
              (P > Contents'First + 16 and
                   Contents'Last >= Contents'First + 16)));
      pragma Loop_Invariant
        (State_Id /= XML_S_VERSION_E_Q_1_P_0 or
           (State_Id = XML_S_VERSION_E_Q_1_P_0
            and then
              (P > Contents'First + 17 and
                   Contents'Last >= Contents'First + 17)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q or
             (State_Id = XML_S_VERSION_E_Q_1_P_0_Q
              and then
                (P > Contents'First + 18 and
                       Contents'Last >= Contents'First + 18)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S
              and then
                (P > Contents'First + 19 and
                       Contents'Last >= Contents'First + 19)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_E or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_E
              and then
                (P > Contents'First + 20 and
                       Contents'Last >= Contents'First + 20)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_EN or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_EN
              and then
                (P > Contents'First + 21 and
                       Contents'Last >= Contents'First + 21)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENC or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENC
              and then
                (P > Contents'First + 22 and
                       Contents'Last >= Contents'First + 22)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO
              and then
                (P > Contents'First + 23 and
                       Contents'Last >= Contents'First + 23)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD
              and then
                (P > Contents'First + 24 and
                       Contents'Last >= Contents'First + 24)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI
              and then
                (P > Contents'First + 25 and
                       Contents'Last >= Contents'First + 25)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN
              and then
                (P > Contents'First + 26 and
                       Contents'Last >= Contents'First + 26)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING
              and then
                (P > Contents'First + 27 and
                       Contents'Last >= Contents'First + 27)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E
              and then
                (P > Contents'First + 28 and
                       Contents'Last >= Contents'First + 28)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q
              and then
                (P > Contents'First + 29 and
                       Contents'Last >= Contents'First + 29)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U
              and then
                (P > Contents'First + 30 and
                       Contents'Last >= Contents'First + 30)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT
              and then
                (P > Contents'First + 31 and
                       Contents'Last >= Contents'First + 31)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF
              and then
                (P > Contents'First + 32 and
                       Contents'Last >= Contents'First + 32)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D
              and then
                (P > Contents'First + 33 and
                       Contents'Last >= Contents'First + 33)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8 or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8
              and then
                (P > Contents'First + 34 and
                       Contents'Last >= Contents'First + 34)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q
              and then
                (P > Contents'First + 35 and
                       Contents'Last >= Contents'First + 35)));
      pragma Loop_Invariant
        (State_Id /=
           XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM or
             (State_Id =
                  XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM
              and then
                (P > Contents'First + 36 and
                       Contents'Last >= Contents'First + 36)));

      --  Aida.Text_IO.Put ("Extracted:");
      --  Aida.Text_IO.Put (Image (CP));
      --  Aida.Text_IO.Put (", state ");
      --  Aida.Text_IO.Put_Line (State_Id_T'Image (State_Id));
      --  Aida.Text_IO.Put (Image (CP));

      case State_Id is
         when End_State =>
            null;
         when Less_Sign =>
            if CP = ' ' then
               null;
            elsif CP = '<' then
               State_Id := Question_Mark;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when Question_Mark =>
            if CP = '?' then
               State_Id := X;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when X =>
            if CP = 'x' then
               State_Id := XM;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XM =>
            if CP = 'm' then
               State_Id := XML;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML =>
            if CP = 'l' then
               State_Id := XML_S;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S =>
            if CP = ' ' then
               State_Id := XML_S_V;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_V =>
            if CP = 'v' then
               State_Id := XML_S_VE;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VE =>
            if CP = 'e' then
               State_Id := XML_S_VER;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VER =>
            if CP = 'r' then
               State_Id := XML_S_VERS;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERS =>
            if CP = 's' then
               State_Id := XML_S_VERSI;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSI =>
            if CP = 'i' then
               State_Id := XML_S_VERSIO;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSIO =>
            if CP = 'o' then
               State_Id := XML_S_VERSION;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION =>
            if CP = 'n' then
               State_Id := XML_S_VERSION_E;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E =>
            if CP = '=' then
               State_Id := XML_S_VERSION_E_Q;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q =>
            if CP = '"' then
               State_Id := XML_S_VERSION_E_Q_1;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1 =>
            if CP = '1' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P =>
            if CP = '.' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0 =>
            if CP = '0' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q =>
            if CP = '"' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S =>
            if CP = ' ' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_E;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_E =>
            if CP = 'e' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_EN;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_EN =>
            if CP = 'n' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENC;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC =>
            if CP = 'c' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCO =>
            if CP = 'o' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCOD =>
            if CP = 'd' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODI =>
            if CP = 'i' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODIN =>
            if CP = 'n' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING =>
            if CP = 'g' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E =>
            if CP = '=' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q =>
            if CP = '"' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_U =>
            if CP = 'u' or CP = 'U' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENCODING_E_Q_UT =>
            if CP = 't' or CP = 'T' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF =>
            if CP = 'f' or CP = 'F' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D =>
            if CP = '-' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8 =>
            if CP = '8' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q =>
            if CP = '"' then
               State_Id :=
                 XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
         when XML_S_VERSION_E_Q_1_P_0_Q_S_ENC_E_Q_UTF_D_8_Q_QM =>
            if CP = '?' then
               if P <= Contents'Last then
                  State_Id := End_State;

                  pragma Assert (P > Contents'First + 36);

                  Analyze_XML (P);
               else
                  Call_Result.Initialize (-0645831530, 1132432555);
                  exit;
               end if;
            else
               Call_Result.Initialize
                 (XML_IDENTIFIER_ERROR_1, XML_IDENTIFIER_ERROR_2);
               exit;
            end if;
      end case;
   end loop;
end Aida.XML_SAX_Parse;
