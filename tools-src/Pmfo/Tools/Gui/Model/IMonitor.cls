
/*------------------------------------------------------------------------
    File        : IMonitor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Aug 01 18:45:00 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.IMonitorTarget from propath.

interface Pmfo.Tools.Gui.Model.IMonitor: 
    define public property Targets as IMonitorTarget extent no-undo get. set.
end interface.