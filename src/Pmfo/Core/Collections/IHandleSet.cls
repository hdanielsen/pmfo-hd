
/*------------------------------------------------------------------------
    File        : IHandleSet
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed May 10 15:03:32 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.Collections.ISet.

interface Pmfo.Core.Collections.IHandleSet inherits ISet:  
     method public handle extent ToHandleArray(). 
end interface.