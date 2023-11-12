 
 /*------------------------------------------------------------------------
    File        : QuerySearchSort
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jun 18 19:45:27 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQuerySortEntry from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.List from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryWalker from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchSort inherits QueryWalker: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define protected property FieldNames as character extent no-undo get. set.
   
    define public property SortEntries as IList no-undo get. set.
      
    constructor public QuerySearchSort (pcFieldName as character):
        super ().
        extent(FieldNames) = 1.
        FieldNames[1] = pcFieldName.
        // use list to preserve order 
        SortEntries = new List().
    end constructor.
    
    constructor public QuerySearchSort (pcFieldNames as character extent):
        super ().
        FieldNames = pcFieldNames.
        // use list to preserve order 
        SortEntries = new List().
    end constructor.
     
    method override public void Traverse( pSort as IQuerySortEntry ):
        define variable i as integer no-undo.
        do i = 1 to extent(FieldNames):
            if pSort:FieldName = FieldNames[i] then
            do:
                SortEntries:Add(pSort).
            end.
        end.
    end method.
         
end class.