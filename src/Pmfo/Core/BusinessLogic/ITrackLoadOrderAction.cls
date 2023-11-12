
/*------------------------------------------------------------------------
    File        : ITrackLoadOrderAction
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Feb 08 16:19:38 EST 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.IAction.
using OpenEdge.Core.Collections.IList from propath.

interface Pmfo.Core.BusinessLogic.ITrackLoadOrderAction inherits IAction:  
    define public property LoadSequences as IList no-undo get. 
end interface.