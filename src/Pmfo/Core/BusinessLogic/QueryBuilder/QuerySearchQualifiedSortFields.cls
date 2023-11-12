 
 /*------------------------------------------------------------------------
    File        : QuerySearchQualifiedSortFields
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     :  
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedFields from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedSortFields inherits QuerySearchQualifiedFields: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    
    constructor public QuerySearchQualifiedSortFields (pcTableName as character):  
        super (pcTableName).  
    end constructor.
    
    constructor public QuerySearchQualifiedSortFields ():
        super ().
    end method.
    
    method override public void Traverse( pPredicate as IQueryPredicate ):
        // do nothing
    end method.    
   
        
end class.