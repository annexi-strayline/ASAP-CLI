
-- CLI AURA Configuration Manifest

package CLI.AURA is
   
   package Build is
      package Ada is
         package Compiler_Options is
            Ignore_Unknown_Pragmas: constant String := "-gnatwG";
            -- Disable warnings from GNAT for the use of pragma External_With
            
         end Compiler_Options;
      end Ada;
   end Build;
end CLI.AURA;
