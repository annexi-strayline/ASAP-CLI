------------------------------------------------------------------------------
--                                                                          --
--                    Command Line Interface Toolkit                        --
--                                                                          --
-- ------------------------------------------------------------------------ --
--                                                                          --
--  Copyright (C) 2020, ANNEXI-STRAYLINE Trans-Human Ltd.                   --
--  All rights reserved.                                                    --
--                                                                          --
--  Original Contributors:                                                  --
--  * Richard Wai (ANNEXI-STRAYLINE)                                        --
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

package CLI.Widgets.Progress_Bars is 
    
   type Percentage is range 0 .. 100;
   
   type Progress_Bar (Delimited: Boolean := True) is
      record
         Percent: Percentage := 0;
         -- The percent value of the bar
         
         Width: Positive := 10;
         -- Width in chracters of the progress bar, not including
         -- delimiters
         
         Column: Natural := 1;
         -- Which column the bar should be rendered at
         
         Fill_Char : Wide_Wide_Character := '=';
         Fill_Style: Text_Style         := Neutral;
         -- The character used to fill the "completed" area
         
         Empty_Char : Wide_Wide_Character := ' ';
         Empty_Style: Text_Style          := Neutral;
         
         case Delimited is
            when True =>
               Left_Delimiter: Wide_Wide_Character  := '[';
               Left_Delimiter_Style: Text_Style     := Neutral;
            
               Right_Delimiter: Wide_Wide_Character := ']';
               Right_Delimiter_Style: Text_Style    := Neutral;
            
            when False =>
               null;
         end case;
         
         -- All Wide_Wide_Characters will be encoded as UTF-8 when
         -- output to the terminal
      end record;
   
   procedure Render (Bar   : in out Progress_Bar; 
                     Column: in     Positive := Current_Column);
   -- Sets the Bar's Column to Column and then Renders the Bar. The cursor is
   -- moved (ends one column past the end of the bar)
   
   procedure Update (Bar: in out Progress_Bar);
   -- Re-renders an existing bar (typically when the progress property is
   -- updated). This does not update delimiters.
   
   
end CLI.Widgets.Progress_Bars;
