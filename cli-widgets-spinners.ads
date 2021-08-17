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

with Ada.Strings.Wide_Wide_Bounded;

package CLI.Widgets.Spinners is
   
   type Spinner is private;
   
   package Component_Strings is 
     new Ada.Strings.Wide_Wide_Bounded.Generic_Bounded_Length (20);
   
   subtype Component_String is Component_Strings.Bounded_Wide_Wide_String;
   
   function Custom_Spinner (Components: Component_String;
                            Style     : Text_Style := Neutral)
                           return Spinner;
   -- Creates a custom spinner style. Components will be converted into
   -- UTF-8 on output
   
   procedure Set_Style (S    : in out Spinner;
                        Style: in     Text_Style);
   -- Set the style of the spinner. The default value is Neutral
   
   
   procedure Render (Spin  : in out Spinner;
                     Column: in     Positive := Current_Column);
   -- Initially renders a Spinner, and set's it column to Column. Subsequent
   -- Update operations do not need to specify the column. The cursor is moved,
   -- and will end one column past the column of the spinner.
   
   procedure Update (Spin: in out Spinner);
   -- Updates the spinner in place (draws and advances). The cursor is not
   -- moved.
   
private
   
   use all type Component_String;
   
   type Spinner
      is record
         Column     : Positive         := 1;  -- Column of the spinner
         Components : Component_String := To_Bounded_Wide_Wide_String ("-\|/");
         State      : Positive         := 1;
         Style      : Text_Style       := Neutral;
      end record;
   
end CLI.Widgets.Spinners;
