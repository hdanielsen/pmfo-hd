
/*------------------------------------------------------------------------
    File        : Monitor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Aug 01 18:45:00 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.IMonitorTarget from propath.
using Pmfo.Tools.Gui.Model.IMonitor from propath.

class Pmfo.Tools.Gui.Model.Monitor implements IMonitor: 
    define public property Targets as IMonitorTarget extent no-undo get. set.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	destructor public Monitor ( ):
       define variable i as integer no-undo.
       do i = 1 to extent(Targets):
           if valid-object(Targets[i]) then
               delete object(Targets[i]).
       end.    
	end destructor.
    
    
end class.