------------------------------------------------------------------------------
--                                                                          --
--                    Command Line Interface Toolkit                        --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2019-2021, ANNEXI-STRAYLINE Trans-Human Ltd.              --
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

with Ada.Text_IO;       use Ada;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
with Interfaces.C;      use Interfaces.C;

with Ada.Strings.UTF_Encoding.Wide_Wide_Strings;

pragma External_With ("tty_ifo.c");

package body CLI is
   
   --
   -- Helper functions, state, and constants
   --
   
   Column_Actual: Positive;
   -- This is initialized by the package elaboration, which actually ensures
   -- the cursor is at this position by calling Clear_Line
   
   Control_Preamble: constant Character := Character'Val (8#33#); -- '\033'
   -- Needs to be output at the beginning of all ANSI terminal control codes
   
   CSI: constant String := Control_Preamble & '[';
   
   function Trim_Num (Num : Integer) return String is 
        (Ada.Strings.Fixed.Trim(Integer'Image(Num), Ada.Strings.Left));
   -- Helper function to ensure our codes with ordinals don't have extra spaces
   
   --------------------
   -- Terminal_Width --
   --------------------
   
   function Terminal_Width return Positive is
      function term_width return Interfaces.C.unsigned_short with 
        Import        => True,
        Convention    => C,
        External_Name => "tty_ifo__term_width";   
   begin
      if Output_Is_Terminal then
         return Positive (term_width);
      else
         return 80;
      end if;
   end Terminal_Width;
   
   -----------------------
   -- Ouput_Is_Terminal --
   -----------------------
   
   use type Interfaces.C.int;
   
   function isatty return Interfaces.C.int with
     Import        => True,
     Convention    => C,
     External_Name => "tty_ifo__isatty";
   
   Is_TTY: constant Boolean := (isatty > 0);
   
   function Output_Is_Terminal return Boolean is
     (Is_TTY);
      
   ----------------
   -- Set_Column --
   ----------------
   
   procedure Set_Column (Col: in Positive) is
   begin
      if Is_TTY then
         Text_IO.Put (CSI & Trim_Num (Col) & 'G');
      end if;
      Column_Actual := Col;
   end Set_Column;
   
   --------------------
   -- Current_Column --
   --------------------
   
   function Current_Column return Positive is (Column_Actual);
   
   ------------------
   -- Clear_Screen --
   ------------------
   
   procedure Clear_Screen is   
      Clear_Screen_Sequence : constant String 
        := CSI & "2J" & -- Clear entire screen
           CSI & ";H";  -- Home cursor to top-left
   begin
      if Is_TTY then
         Text_IO.Put (Clear_Screen_Sequence);
      end if;
      
      Column_Actual := 1;
   end Clear_Screen;
   
   ----------------
   -- Clear_Line --
   ----------------
   
   procedure Clear_Line is
   begin
      -- Move cursor to column 1 and clear line
      Set_Column (1);
      Clear_To_End;
   end Clear_Line;
   
   ------------------
   -- Clear_To_End --
   ------------------
   
   procedure Clear_To_End is
   begin
      if Is_TTY then
         Text_IO.Put (CSI & 'K'); -- Clear to end
      end if;
   end Clear_To_End;
   
   ---------
   -- Put --
   ---------
   
   procedure Put (Char   : Character;
                  Style  : Text_Style := Neutral)
   is begin
      Put (Message => String'(1 .. 1 => Char),
           Style   => Style);
   end Put;
   
   --------------------------------------------------
   procedure Put (Message: String;
                  Style  : Text_Style := Neutral)
   is begin
      Clear_Style;
      Apply_Style(Style);
      Text_IO.Put(Message);
      Clear_Style;
      Column_Actual := Column_Actual + Message'Length;
   end Put;
   
   -------------------
   -- Wide_Wide_Put --
   -------------------
   
   procedure Wide_Wide_Put (Char : in Wide_Wide_Character;
                            Style: in Text_Style := Neutral)
   is begin
      Wide_Wide_Put (Message => Wide_Wide_String'(1 .. 1 => Char),
                     Style   => Style);
   end Wide_Wide_Put;
   
   --------------------------------------------------
   procedure Wide_Wide_Put (Message: in Wide_Wide_String;
                            Style  : in Text_Style := Neutral)
   is 
      use Ada.Strings.UTF_Encoding;
      use Ada.Strings.UTF_Encoding.Wide_Wide_Strings;
      
      Encoded_Message: constant UTF_8_String := Encode (Message);
   begin
      Clear_Style;
      Apply_Style(Style);
      Text_IO.Put(Encoded_Message);
      Clear_Style;
      Column_Actual := Column_Actual + Message'Length;
   end Wide_Wide_Put;
   
   ------------
   -- Put_At --
   ------------
   
   procedure Put_At (Char   : in Character;
                     Column : in Positive;
                     Style  : in Text_Style := Neutral)
   is begin
      Put_At (Message => String'(1 .. 1 => Char),
              Column  => Column,
              Style   => Style);
   end Put_At;
   
   --------------------------------------------------
   procedure Put_At (Message: in String;
                     Column : in Positive;
                     Style  : in Text_Style := Neutral)
   is 
      Save_Col: constant Positive := Current_Column;
   begin
      Set_Column (Column);
      Put (Message => Message,
           Style   => Style);
      Set_Column (Save_Col);
   end Put_At;
   
   ----------------------
   -- Wide_Wide_Put_At --
   ----------------------
   
   procedure Wide_Wide_Put_At (Char  : in Wide_Wide_Character;
                               Column: in Positive;
                               Style : in Text_Style := Neutral)
   is begin
      Wide_Wide_Put_At (Message => Wide_Wide_String'(1 .. 1 => Char),
                        Column  => Column,
                        Style   => Style);
   end Wide_Wide_Put_At;
   
   --------------------------------------------------
   procedure Wide_Wide_Put_At (Message: in Wide_Wide_String;
                               Column : in Positive;
                               Style  : in Text_Style := Neutral)
   is 
      Save_Col: constant Positive := Current_Column;
   begin
      Set_Column (Column);
      Wide_Wide_Put (Message => Message,
                     Style   => Style);
      Set_Column (Save_Col);
   end Wide_Wide_Put_At;
   
   --------------
   -- Put_Line --
   --------------
   
   procedure Put_Line (Message: String;
                       Style  : Text_Style := Neutral)
   is begin
      Put (Message, Style);
      New_Line;
   end Put_Line;
   
   ------------------------
   -- Wide_Wide_Put_Line --
   ------------------------
   
   procedure Wide_Wide_Put_Line (Message: Wide_Wide_String;
                                 Style  : Text_Style := Neutral)
   is begin
      Wide_Wide_Put (Message, Style);
      New_Line;
   end Wide_Wide_Put_Line;
   
   --------------
   -- New_Line --
   --------------
   
   procedure New_Line is
   begin
      Text_IO.New_Line;
      Set_Column (1);
   end New_Line;
   
   -----------------------
   -- Get/Get_Immediate --
   -----------------------
   
   procedure Get (Item: out String) renames Text_IO.Get;
   procedure Get (Item: out Character) renames Text_IO.Get;
   procedure Get_Line (Item: out String; Last: out Natural)
     renames Text_IO.Get_Line;
   procedure Get_Immediate (Item: out Character; Available: out Boolean)
     renames Text_IO.Get_Immediate;
   
   --------------------------
   -- Text_Style Operators --
   --------------------------
   
   -- "+" --
   
   function "+" (Left, Right : Text_Style) return Text_Style is
      -- This function will always take the color of the right Cursor, because
      -- addition works like an overwrite for all the attributes that are not 
      -- currently turned on for Left cursor
      
      FG : Color := (if (Right.Foreground /= Default) 
                     then Right.Foreground 
                     else Left.Foreground);
      
      BG : Color := (if (Right.Background /= Default) 
                     then Right.Background 
                     else Left.Background);
   begin
      return (Text_Style'(Foreground  => FG,
                     Background  => BG,
                     Bold        => Left.Bold        or Right.Bold,
                     Underscore  => Left.Underscore  or Right.Underscore,
                     Blink       => Left.Blink       or Right.Blink,
                     Reverse_Vid => Left.Reverse_Vid or Right.Reverse_Vid,
                     Concealed   => Left.Concealed   or Right.Concealed));
   end "+";
   
   -- "-" --
   
   function "-" (Left, Right : Text_Style) return Text_Style is
      -- This function will always take the color of the left Cursor, unless 
      -- they are the same
      -- This is because subtracting the right Cursor means we cannot have that
      -- property of it
      
      FG : Color := (if (Left.Foreground = Right.Foreground)
                     then Default
                     else Left.Foreground);
                    
      BG : Color := (if (Left.Background = Right.Background)
                    then Default
                    else Left.Background);
            
   begin
        return (Text_Style'(Foreground  => FG,
                       Background  => BG,
                       Bold        => Left.Bold and (Left.Bold xor Right.Bold),
                       Underscore  => 
                         Left.Underscore and 
                         (Left.Underscore xor Right.Underscore),
                       Blink       => 
                         Left.Blink and (Left.Blink xor Right.Blink),
                       Reverse_Vid => 
                         Left.Reverse_Vid 
                         and (Left.Reverse_Vid xor Right.Reverse_Vid),
                       Concealed   => Left.Concealed 
                         and (Left.Concealed xor Right.Concealed)));
        
   end "-";
   
   -----------------
   -- Apply_Style --
   -----------------
   
   procedure Apply_Style (Style : Text_Style) is 
      
      Attribute_Sequence : String :=  CSI & "0m";
      
      Change_Foreground_Sequence : String :=
        CSI & Trim_Num(Color'Pos(Style.Foreground) + 30) & 'm';
      
      Change_Background_Sequence : String :=
        CSI & Trim_Num(Color'Pos(Style.Background) + 40) & 'm';
      
   begin
      if not Is_TTY then return; end if;
      
      if Style.Foreground /= Default then
         Text_IO.Put(Change_Foreground_Sequence);
      end if;
      
      if Style.Background /= Default then
         Text_IO.Put(Change_Background_Sequence);
      end if;
         
      declare
         
         procedure Apply_Attribute (Attribute_Num : Integer) with Inline is
         begin
            -- Take first index of Trim_Num return because it is a string but
            -- we need a char
            Attribute_Sequence(3) := Trim_Num(Attribute_Num)(1);
            Text_IO.Put(Attribute_Sequence);
         end Apply_Attribute;
         
      begin
  
         if Style.Bold then
            Apply_Attribute(1);
         end if;
      
         if Style.Underscore then
            Apply_Attribute(4);
         end if;
         
         if Style.Blink then
            Apply_Attribute(5);
         end if;
         
         if Style.Reverse_Vid then
            Apply_Attribute(7);
         end if;
         
         if Style.Concealed then
            Apply_Attribute(8);
         end if;
         
      end;
   end Apply_Style;
   
   -----------------
   -- Clear_Style --
   -----------------
   
   procedure Clear_Style is
   begin
      if not Is_TTY then return; end if;
      
      Text_IO.Put (CSI & "0m"); -- "Reset attributes";
   end Clear_Style;
   
begin
   Clear_Line;
   
end CLI;
