 
 /*------------------------------------------------------------------------
    File        : QueryPredicate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Apr 02 15:51:12 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.QueryEntry from propath.

class Pmfo.Core.BusinessLogic.AbstractQueryPredicate abstract inherits QueryEntry implements IQueryPredicate, IQueryEntry: 
    define protected abstract property NotImplementedReasonTemplate as character no-undo get. 
      
    define public property FieldName    as character         no-undo get. set. 
    
    
    define public property Operator        as QueryOperatorEnum no-undo get. set. 
    define public abstract property Value  as IPrimitiveHolder no-undo get. protected set. 
    define public abstract property Values as IPrimitiveArrayHolder no-undo get. protected set. 
    
    constructor protected AbstractQueryPredicate (input pFieldName as character,
                                                  input pOperator as QueryOperatorEnum):
        
        Assert:NotNullOrEmpty(pFieldName, 'Field name').
        Assert:NotNull(pOperator, 'Operator').
        assign FieldName   = pFieldName
               Operator    = pOperator
               .
          
    end constructor.
    
    constructor protected AbstractQueryPredicate (input pFieldName as character):
        
        Assert:NotNullOrEmpty(pFieldName, 'Field name').
        assign FieldName   = pFieldName
               .
          
    end constructor.
    
end class.