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

package body CLI.Widgets.Progress_Bars is
   
   ------------------
   -- Render_Inner --
   ------------------
   -- Render the bar except for the delimiters. Column must be set appropriatly
   -- first
   
   procedure Render_Inner (Bar: in Progress_Bar) is
      Progress_Length: constant Natural := Natural 
        (Float'Rounding ((Float (Bar.Percent) / 100.0) * Float (Bar.Width)));
      
      Fill_String: constant Wide_Wide_String (1 .. Progress_Length)
           := (others => Bar.Fill_Char);
         
      Empty_String: constant Wide_Wide_String 
           (1 .. (Bar.Width - Progress_Length))
           := (others => Bar.Empty_Char);
   begin
      Wide_Wide_Put (Message => Fill_String,  Style => Bar.Fill_Style );
      Wide_Wide_Put (Message => Empty_String, Style => Bar.Empty_Style);
   end Render_Inner;
   
   ------------
   -- Render --
   ------------
   
   procedure Render (Bar   : in out Progress_Bar; 
                     Column: in     Positive := Current_Column)
   is begin
      Bar.Column := Column;
      Set_Column (Bar.Column);
      
      if Bar.Delimited then
         -- Left delimiter         
         Wide_Wide_Put (Char  => Bar.Left_Delimiter, 
                        Style => Bar.Left_Delimiter_Style);
      end if;
      
      Render_Inner (Bar);
      
      if Bar.Delimited then
         Wide_Wide_Put (Char  => Bar.Right_Delimiter, 
                        Style => Bar.Right_Delimiter_Style);
      end if;
   end Render;
   
   ------------
   -- Update --
   ------------
   
   procedure Update (Bar: in out Progress_Bar) is 
      Saved_Column: constant Positive := Current_Column;
   begin
      Set_Column (Bar.Column + (if Bar.Delimited then 1 else 0));
      Render_Inner (Bar);
      Set_Column (Saved_Column);
   end Update;
      
   
end CLI.Widgets.Progress_Bars;
