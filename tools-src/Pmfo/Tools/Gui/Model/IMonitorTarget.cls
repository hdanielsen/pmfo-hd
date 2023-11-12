
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

interface Pmfo.Tools.Gui.Model.IMonitorTarget: 
    define public property Text as character no-undo get. set.
    define public property Total as int64 no-undo get. set.
    define public property Canceled as logical no-undo get. set.
    
    method public void Increment().
    method public void Increment(pCount as int64).
    method public void Complete(). 
    
end interface.