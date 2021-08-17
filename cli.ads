------------------------------------------------------------------------------
--                                                                          --
--                    Command Line Interface Toolkit                        --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019-2020, ANNEXI-STRAYLINE Trans-Human Ltd.              --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Ensi Martini (ANNEXI-STRAYLINE)                                       --
--                                                                          --
--  Redistribution and use in source and binary forms, with or without      --
--  modification, are permitted provided that the following conditions are  --
--  met:                                                                    --
--                                                                          --
--      * Redistributions of source code must retain the above copyright    --
--        notice, this list of conditions and the following disclaimer.     --
--                                                                          --
--      * Redistributions in binary form must reproduce the above copyright --
--        notice, this list of conditions and the following disclaimer in   --
--        the documentation and/or other materials provided with the        --
--        distribution.                                                     --
--                                                                          --
--      * Neither the name of the copyright holder nor the names of its     --
--        contributors may be used to endorse or promote products derived   --
--        from this software without specific prior written permission.     --
--                                                                          --
--  THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS     --
--  "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT       --
--  LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A --
--  PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT      --
--  OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,   --
--  SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT        --
--  LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,   --
--  DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY   --
--  THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT     --
--  (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE   --
--  OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.    --
--                                                                          --
------------------------------------------------------------------------------

package CLI is
   
   -- Note: CLI is line-oriented. Any widgets updated after New_Line will
   -- be rendered on the new line
   
   type Text_Style is private;
   
   function "+" (Left, Right : Text_Style) return Text_Style;
   -- This operator works like an "overwrite"
   -- Will keep any attributes of Left, adding to it the attributes of Right
   -- that it does not currently have
   -- NOTE: The color for the new Text_Style will always be the same as the
   -- color of the Right cursor, so if a user wishes to make a cursor that is
   -- the same as their current one, just with a different color, they should
   -- use: New_Style : Text_Style := Current_Style + New_Desired_Color_Style
   -- NOT the other way around
   
   function "-" (Left, Right : Text_Style) return Text_Style;
   -- This operator works like a "clear" for all attributes that appear in both
   -- Will retain all Left attributes that are not in Right cursor
   -- NOTE: The color for the new Text_Style will always be the same as the
   -- color of the Left cursor, because this is the one we are subtracting from
   
   -- With the preceding two operators and the following presets, any style
   -- and color combination cursor can be made
   
   
   -- COLOR PRESET CURSORS --
   Neutral    : constant Text_Style;
   -- Follow the text-style set from prior output
   
   -- Foreground Colors --
   Black_FG   : constant Text_Style;
   Red_FG     : constant Text_Style;
   Green_FG   : constant Text_Style;
   Yellow_FG  : constant Text_Style;
   Blue_FG    : constant Text_Style;
   Magenta_FG : constant Text_Style;
   Cyan_FG    : constant Text_Style;
   White_FG   : constant Text_Style;
   
   -- Background Colors --
   Black_BG   : constant Text_Style;
   Red_BG     : constant Text_Style;
   Green_BG   : constant Text_Style;
   Yellow_BG  : constant Text_Style;
   Blue_BG    : constant Text_Style;
   Magenta_BG : constant Text_Style;
   Cyan_BG    : constant Text_Style;
   White_BG   : constant Text_Style;
   
   
   -- ATTRIBUTE PRESET CURSORS --
   Bold        : constant Text_Style;
   Underline   : constant Text_Style;
   Blink       : constant Text_Style;
   Reverse_Vid : constant Text_Style;
   
   function  Terminal_Width return Positive;
   -- Width of the terminal in columns. If the output is not a terminal,
   -- a value of 80 is always returned
   
   procedure Set_Column (Col: in Positive);
   function  Current_Column return Positive;
   -- Sets or returns the current column number of the cursor.
   
   procedure Clear_Screen;
   procedure Clear_Line;
   procedure Clear_To_End;
   
   -- Note: all Wide_Wide_Strings/Characters are encoded in UTF-8 prior to
   -- output
   
   procedure Put (Char : in Character;
                  Style: in Text_Style := Neutral);
   
   procedure Put (Message: in String;
                  Style  : in Text_Style := Neutral);
   
   procedure Wide_Wide_Put (Char : in Wide_Wide_Character;
                            Style: in Text_Style := Neutral);
   
   procedure Wide_Wide_Put (Message: in Wide_Wide_String;
                            Style  : in Text_Style := Neutral);

   
   procedure Put_At (Char   : in Character;
                     Column : in Positive;
                     Style  : in Text_Style := Neutral);
   
   procedure Put_At (Message: in String;
                     Column : in Positive;
                     Style  : in Text_Style := Neutral);
   
   procedure Wide_Wide_Put_At (Char  : in Wide_Wide_Character;
                               Column: in Positive;
                               Style : in Text_Style := Neutral);
   
   procedure Wide_Wide_Put_At (Message: in Wide_Wide_String;
                               Column : in Positive;
                               Style  : in Text_Style := Neutral);
   
   -- Put_At opeartions do not change the cursor position
   
   procedure Put_Line (Message: String;
                       Style  : Text_Style := Neutral);
   
   procedure Wide_Wide_Put_Line (Message: Wide_Wide_String;
                                 Style  : Text_Style := Neutral);
   
   procedure New_Line;
   
   procedure Get (Item: out String);
   procedure Get (Item: out Character);
   procedure Get_Line (Item: out String; Last: out Natural);
   procedure Get_Immediate (Item: out Character; Available: out Boolean);
   
   function Output_Is_Terminal return Boolean;
   -- True if Standard_Output is a terminal, and False otherwise.
   -- If False, all ANSI code output is supressed.
   --
   -- The use should avoid using widgets if Is_TTY is False
   
private
   
   -- Clears to the end of the line from the current column
   
   type Color is (Black, 
                  Red, 
                  Green, 
                  Yellow, 
                  Blue, 
                  Magenta, 
                  Cyan, 
                  White, 
                  Default);
   
   type Text_Style is
      record
         Bold        : Boolean := False;
         Underscore  : Boolean := False;
         Blink       : Boolean := False;
         Reverse_Vid : Boolean := False;
         Concealed   : Boolean := False;
         
         Foreground  : Color   := Default;
         Background  : Color   := Default;
         
      end record;
   
   Neutral    : constant Text_Style := (others => <>);
   
   Black_FG   : constant Text_Style := (Foreground  => Black,   others => <>);
   Red_FG     : constant Text_Style := (Foreground  => Red,     others => <>);
   Green_FG   : constant Text_Style := (Foreground  => Green,   others => <>);
   Yellow_FG  : constant Text_Style := (Foreground  => Yellow,  others => <>);
   Blue_FG    : constant Text_Style := (Foreground  => Blue,    others => <>);
   Magenta_FG : constant Text_Style := (Foreground  => Magenta, others => <>);
   Cyan_FG    : constant Text_Style := (Foreground  => Cyan,    others => <>);
   White_FG   : constant Text_Style := (Foreground  => White,   others => <>);
   
   Black_BG   : constant Text_Style := (Background  => Black,   others => <>);
   Red_BG     : constant Text_Style := (Background  => Red,     others => <>);
   Green_BG   : constant Text_Style := (Background  => Green,   others => <>);
   Yellow_BG  : constant Text_Style := (Background  => Yellow,  others => <>);
   Blue_BG    : constant Text_Style := (Background  => Blue,    others => <>);
   Magenta_BG : constant Text_Style := (Background  => Magenta, others => <>);
   Cyan_BG    : constant Text_Style := (Background  => Cyan,    others => <>);
   White_BG   : constant Text_Style := (Background  => White,   others => <>);
  
   Bold       : constant Text_Style := (Bold        => True,    others => <>);
   Underline  : constant Text_Style := (Underscore  => True,    others => <>);
   Blink      : constant Text_Style := (Blink       => True,    others => <>);
   Reverse_Vid: constant Text_Style := (Reverse_Vid => True,    others => <>);
   
   procedure Apply_Style (Style: Text_Style);
   procedure Clear_Style;
   
end CLI;
