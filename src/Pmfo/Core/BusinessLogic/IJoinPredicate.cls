
/*------------------------------------------------------------------------
    File        : IJoinPredicate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 10 11:02:18 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Core.BusinessLogic.IJoinPredicate:  
    define public property ParentField as character no-undo get.
    define public property ChildField as character no-undo get.
end interface.