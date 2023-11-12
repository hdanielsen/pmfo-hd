
/*------------------------------------------------------------------------
    File        : ICodeTableView
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Mar 05 21:52:47 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.View.IView from propath.

interface Pmfo.Tools.Gui.View.ICodeTableView inherits IView:  
    method public void DisplayCodeTable(input table-handle phCodeTable). 
    method public void Create(input phParent as handle). 
    
end interface.