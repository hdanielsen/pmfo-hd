 
 /*------------------------------------------------------------------------
    File        : QuerySortEntry
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 03 08:05:39 EDT 2019
    Notes       : Does not inherit OpenEdge.BusinessLogic.QuerySortEntry since we need public accessor to fieldname
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.SortOrderEnum from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using OpenEdge.Core.Assert from propath.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.QuerySortEntry implements IQuerySortEntry: 
     /* Returns the name of the field for this query sort entry */
    
    define public property FieldName as character no-undo
        get.
        set. 
    
    /* Returns the sort order for this query sort entry */
    define public property SortOrder as SortOrderEnum no-undo get. set.
    
    /* Constructor
       
       @param character (mandatory) The field name
       @param SortOrderEnum (mandatory) The sort direction */
    constructor public QuerySortEntry (input pFieldName as character,
                                       input pSortDirection as SortOrderEnum):
        Assert:NotNullOrEmpty(pFieldName, 'Field name').
        Assert:NotNull(pSortDirection, 'Sort direction').
        
        assign this-object:FieldName = pFieldName
               this-object:SortOrder = pSortDirection
               .
    end constructor.
    
    /* Constructor
       
       @param character (mandatory) The field name  */
    constructor public QuerySortEntry (input pFieldName as character):
        this-object(pFieldName, SortOrderEnum:Ascending).
    end constructor.
    
 
    
    method override public Progress.Lang.Object Clone(  ):
        return new Pmfo.Core.BusinessLogic.QuerySortEntry(FieldName,SortOrder).
    end method.
    
    method override character ToString():
        return "by " + FieldName + if SortOrder:Equals(SortOrderEnum:Descending) then " " + lc(SortOrder:ToString()) else "".
    end method.     
end class.