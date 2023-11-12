
/*------------------------------------------------------------------------
    File        : IConflictService
    Purpose     : Refine and manage response for locking conflicts
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Nov 25 08:59:45 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Core.DataLayer.Util.IConflictService:  
    method public character GetChangesFormatted(phDatafields as handle extent,phsourcefields as handle extent).
    method public character GetChangeLabels(phDatafields as handle extent).
      
end interface.