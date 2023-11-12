 
 /*------------------------------------------------------------------------
    File        : QuerySearchPredicates
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     :12/8/2021
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryWalker from propath.
using Pmfo.Core.Common.CharacterHolder from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QueryWildcardConverter inherits QueryWalker: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    constructor public QueryWildcardConverter ( ):  
        super ().
    end constructor.
    
    method override public void Traverse( input pTableRequest as IGetDataTableRequest):
        super:Traverse(pTableRequest). 
    end method.
    
    method override public void Traverse( input pPredicate as IQueryPredicate ):
        define variable cValue as character no-undo.
        if pPredicate:Operator = QueryOperatorEnum:Eq then 
        do: 
            if type-of(pPredicate,Pmfo.Core.BusinessLogic.IQueryPredicate) then 
            do: 
                cValue = cast(pPredicate,Pmfo.Core.BusinessLogic.QueryPredicate):StringValue. 
                if cValue begins "*" then 
                do: 
                    cast(pPredicate,Pmfo.Core.BusinessLogic.QueryPredicate):Operator = QueryOperatorEnum:Matches. 
                end.
                else if length(cValue) > 0 and substr(cValue,length(cValue),1) = "*" then 
                do:
                    cast(pPredicate,Pmfo.Core.BusinessLogic.QueryPredicate):Stringvalue = substr(cValue,1,length(cValue) - 1).
                    cast(pPredicate,Pmfo.Core.BusinessLogic.QueryPredicate):Operator = QueryOperatorEnum:Begins.
                end.        
            end.       
            else if type-of(pPredicate:Value,CharacterHolder) // and cast(pPredicate,CharacterHolder):Value begins "*" 
                 or type-of(pPredicate:Value,String)  //        and cast(pPredicate,String):Value begins "*"
            then  
                undo, throw new UnsupportedOperationError(subst("Wildcard filter for class &1",pPredicate:GetClass():TypeName)).
        end.                   
    end method.
    
     
    
    
    method override public void Traverse( pSort as IQuerySortEntry ):
                        
    end method.
 
        
end class.