
/*------------------------------------------------------------------------
    File        : ICompareQueryPredicate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Jun 24 10:45:56 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.IQueryPredicate.
using Pmfo.Core.BusinessLogic.CompareStrengthEnum from propath.

interface Pmfo.Core.BusinessLogic.ICompareQueryPredicate inherits IQueryPredicate:  
    define public property Strength as CompareStrengthEnum get. set.
end interface.