/*------------------------------------------------------------------------
    File        : IWidgetHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Feb 15 17:20:23 EST 2019
    Notes       : listens to widget events from a eventhandler.p
                  Classes cannot have gui triggers  
  ----------------------------------------------------------------------*/
using Progress.Lang.*.

interface Pmfo.Tools.Gui.View.IWidgetHandler:  
    method public void WidgetEvent(pcEvent as char). 
end interface.
 