 
 /*------------------------------------------------------------------------
    File        : QuerySearchQualifiedPredicateFields
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     :  
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedFields from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedPredicateFields inherits QuerySearchQualifiedFields: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    
    constructor public QuerySearchQualifiedPredicateFields (pcTableName as character):  
        super (pcTableName).  
    end constructor.
    
    constructor public QuerySearchQualifiedPredicateFields ():
        super ().
    end method.
    
     method override public void Traverse( pSort as IQuerySortEntry ):
        // do nothing
    end method.    
   
        
end class.