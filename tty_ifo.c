#include <stdio.h>
#include <sys/ioctl.h>
#include <unistd.h>


/* function term_width return Interfaces.C.unsigned_short with */
/*   Import        => True,                                    */
/*   Convention    => C,                                       */
/*   External_Name => "tty_ifo__term_width";                   */
   
unsigned short tty_ifo__term_width ( void )
{
     int ret_val;
     struct winsize ws;

     ret_val = ioctl ( STDOUT_FILENO, TIOCGWINSZ, &ws );

     if ((ret_val < 0) || (ws.ws_row == 0 || ws.ws_col == 0))
          /* In case of a failed call, assume the standard size */
          /* "Standard" tty size */
          return ( 80 );
     else
          return ( ws.ws_col );
}


/* function Is_TTY return int with       */
/*   Import => True,                     */
/*   Convention => C,                    */
/*   External_Name => "tty_ifo__isatty"; */

int tty_ifo__isatty ( void )
{
     return isatty ( STDOUT_FILENO );
}
