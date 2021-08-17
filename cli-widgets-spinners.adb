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

with Ada.Text_IO; use Ada;

package body CLI.Widgets.Spinners is
   
   --------------------
   -- Custom_Spinner --
   --------------------
   
   function Custom_Spinner (Components: Component_String;
                            Style     : Text_Style := Neutral)
                           return Spinner
   is begin
      return Spinner'
        (Components => Components,
         Style      => Style,
         others     => <>);
   end Custom_Spinner;
   
   ---------------
   -- Set_Style --
   ---------------
   
   procedure Set_Style (S    : in out Spinner;
                        Style: in     Text_Style)
   is begin
      S.Style := Style;
   end Set_Style;
   
   --------------------
   -- Render_Spinner --
   --------------------
   
   procedure Render (Spin  : in out Spinner;
                     Column: in     Positive := Current_Column)
   is begin
      Spin.Column := Column;
      Set_Column (Column);
      Wide_Wide_Put (Char => Element (Source => Spin.Components,
                                      Index  => Spin.State),
                     Style   => Spin.Style);
      
      if Spin.State >= Length (Spin.Components) then
         Spin.State := 1;
      else
         Spin.State := Spin.State + 1;
      end if;
   end Render;
   
   ------------
   -- Update --
   ------------
   
   procedure Update (Spin: in out Spinner) is
      Saved_Column: constant Positive := Current_Column;
   begin
      Render (Spin   => Spin,
              Column => Spin.Column);
      Set_Column (Saved_Column);
   end Update;
   
end CLI.Widgets.Spinners;
