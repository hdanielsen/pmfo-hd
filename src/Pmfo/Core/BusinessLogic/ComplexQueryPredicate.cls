 
 /*------------------------------------------------------------------------
    File        : ComplexQueryPredicate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Apr 02 15:51:12 EDT 2019
    Notes       : Use for complex queries 
                  The excpression must be a compilable expression in the query
                  ToStrings adds it inside parenthesis after the operator 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.BusinessLogic.AbstractQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.ComplexQueryPredicate from propath.
using Pmfo.Util.QueryUtil from propath.

class Pmfo.Core.BusinessLogic.ComplexQueryPredicate inherits AbstractQueryPredicate: 
    define override property NotImplementedReasonTemplate as character no-undo 
        init "&1 property is not implemented. Use &2 instead. The predicate is only used for dynamic queries and string is sufficient and ensures safe dates and decimals."
    get.   
    
    define public property Expression   as character         no-undo get. protected set. 

    define public override property Value as IPrimitiveHolder no-undo 
        get():
            undo, throw new NotImplementedError(subst(NotImplementedReasonTemplate,"Value","Expression")).
        end.
        protected set(pValue as IPrimitiveHolder):
            undo, throw new NotImplementedError(subst(NotImplementedReasonTemplate,"Value","Expression")).
        end. 

    define public override property Values as IPrimitiveArrayHolder no-undo 
      get( ):
          undo, throw new NotImplementedError(subst(NotImplementedReasonTemplate,"Values","Expression or multiple QueryPredicates")).
      end.
      protected set(pValue as IPrimitiveArrayHolder):
          undo, throw new NotImplementedError(subst(NotImplementedReasonTemplate,"Values","Expression or multiple QueryPredicates")).
      end. 
   
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param character value */
    constructor public ComplexQueryPredicate (input pFieldName  as character,
                                              input pOperator   as QueryOperatorEnum,
                                              input pExpression as character):
        
        super(pFieldName,pOperator).
        
        if    pOperator = QueryOperatorEnum:InList
           or pOperator = QueryOperatorEnum:InRange
        then
            undo, throw new IllegalArgumentError(substitute('The operator cannot be &1 for a Complex Expression',
                                                    string(pOperator))).
        
        assign Expression = pExpression.
    end constructor.
    
    
    method override public Progress.Lang.Object Clone(  ):
        return new ComplexQueryPredicate(FieldName,Operator,Expression).
    end method.
    
    
    // join is not included in tostring - it is used in QueryGroup when more than one predicate is shown 
    method override public character ToString():
        return ToString(?,false). 
    end method.
    
    method public override character ToString(phBuffer as handle,plUseSerializeName as logical):
        return ToString(phBuffer,plUseSerializeName,false).
    end method.     
    
    method public override character ToString(phBuffer as handle,plUseSerializeName as logical, plStripParenthesis as logical):
        define variable cSerializeName as character no-undo.
        define variable cOperator      as character no-undo.
        // buffer is for message use 
        if valid-handle(phBuffer) then
        do: 
            if plUseSerializeName then
            do:
                cSerializeName = phBuffer:buffer-field(FieldName):serialize-name no-error.
                if cSerializeName = ? or cSerializeName = "" then
                    cSerializeName = FieldName.
            end.
            cOperator = QueryUtil:GetOperatorText(Operator:ToString()).       
        end.
        else
            cOperator = lc(Operator:ToString()).
        return subst('&1 &2 (&3)',if cSerializeName > "" then cSerializeName else FieldName,cOperator,Expression).
    end method.    
end class.