 
 /*------------------------------------------------------------------------
    File        : QuerySearchSortFields
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jun 18 19:45:27 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchFields from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchSortFields inherits QuerySearchFields: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    constructor public QuerySearchSortFields (pcTableName as character):  
        super (pcTableName).
    end constructor.
    
    constructor public QuerySearchSortFields ():
        super ().
    end method.
    
    method override public void Traverse( input pPredicate as IQueryPredicate ):
        // do nothing         
    end method.
    
        
end class.