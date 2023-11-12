
/*------------------------------------------------------------------------
    File        : IContainer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Feb 16 18:29:01 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/
using Pmfo.Tools.Gui.View.IView.

interface Pmfo.Tools.Gui.View.IContainer inherits IView:  
     method public logical Add(pChild as IView).
end interface.