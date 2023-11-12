
/*------------------------------------------------------------------------
    File        : IIntegerSet
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed May 10 15:03:32 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.Collections.ISet.

interface Pmfo.Core.Collections.IIntegerSet inherits ISet:  
     method public integer extent ToIntegerArray(). 
end interface.