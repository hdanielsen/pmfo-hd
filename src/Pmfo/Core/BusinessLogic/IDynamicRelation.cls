
/*------------------------------------------------------------------------
    File        : IDynamicRelation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Mar 16 11:02:16 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.IJoinPredicate from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.

interface Pmfo.Core.BusinessLogic.IDynamicRelation inherits IRelation:  
    method public IJoinPredicate extent CreateJoinPredicates(pParentField as character extent, pChildField as character extent).
   
end interface.