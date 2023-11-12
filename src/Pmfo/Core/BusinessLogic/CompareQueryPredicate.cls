 
 /*------------------------------------------------------------------------
    File        : CompareQueryPredicate
    Purpose     : Query preficate for character fields that uses Compare 
                  Property Strength controls the stregth paramter to the compare function and defaukts to case-sensitive   
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Jun 24 10:28:56 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/
 

block-level on error undo, throw.

using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Core.BusinessLogic.CompareStrengthEnum from propath.
using Pmfo.Core.BusinessLogic.ICompareQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Core.BusinessLogic.CompareQueryPredicate from propath.

class Pmfo.Core.BusinessLogic.CompareQueryPredicate inherits QueryPredicate implements ICompareQueryPredicate: 
    
    define public property Strength as CompareStrengthEnum 
        get():
            if not valid-object(Strength) then   
                return CompareStrengthEnum:CASE-SENSITIVE.
            else 
                return Strength.     
        end. 
        set.
    
    constructor public CompareQueryPredicate ( input pFieldName as character, input pOperator as Ccs.BusinessLogic.QueryOperatorEnum, input pValue as character ):
        super (input pFieldName, input pOperator, input pValue).
    end constructor.
    
    constructor public CompareQueryPredicate ( input pFieldName as character, input pOperator as Ccs.BusinessLogic.QueryOperatorEnum, input pValues as character extent ):
        super (input pFieldName, input pOperator, input pValues).
    end constructor.

    constructor public CompareQueryPredicate ( input pjoin as JoinEnum, input pFieldName as character, input pOperator as Ccs.BusinessLogic.QueryOperatorEnum, input pValues as character extent ):
        super (input pjoin, input pFieldName, input pOperator, input pValues).
    end constructor.

    constructor public CompareQueryPredicate ( input pjoin as JoinEnum, input pFieldName as character, input pOperator as Ccs.BusinessLogic.QueryOperatorEnum, input pValue as character ):
        super (input pjoin, input pFieldName, input pOperator, input pValue).
        
    end constructor.
    
    method override public Progress.Lang.Object Clone(  ):
        define variable oPredicate as CompareQueryPredicate no-undo.
        if extent(StringValues) <> ? then
            oPredicate = new CompareQueryPredicate(FieldName,Operator,StringValues).
        else      
            oPredicate = new CompareQueryPredicate(FieldName,Operator,StringValue).
        oPredicate:Strength = Strength.
        return oPredicate.   
    end method.
    
    // join is not included in tostring - it is used in QueryGroup when more than one predicate is shown 
    method public override character ToString(phBuffer as handle,plUsePublicLabel as logical):
        define variable i as integer no-undo.
        define variable cExpression    as character no-undo.
      
        define variable cDataType      as character no-undo.
        define variable cExpTemplate   as character init 'compare(&1,"&2","&3","&4")' no-undo.
        define variable cSerializeName as character no-undo.
        define variable hFld           as handle no-undo.
        // buffer is passed to return expression for messages 
        if valid-handle(phBuffer) then
        do:
            cDataType = phBuffer:buffer-field(FieldName):data-type no-error.
            if plUsePublicLabel then
            do:
                hFld = phBuffer:buffer-field(FieldName) no-error.
                if valid-handle(hFld) then 
                    cSerializeName = BufferUtil:GetFieldLabel(hFld).
                
                if cSerializeName = ? or cSerializeName = "" then
                    cSerializeName = FieldName.
            end.
           // lUseQuoter = cDataType = "character" or cDataType begins "date". 
        end.
       
            
        if Operator = QueryOperatorEnum:InList then
        do: 
            
            do i = 1 to extent(StringValues):
                cExpression = cExpression 
                            + (if i > 1 then " or " else "")
                            + subst(cExpTemplate,
                                    if plUsePublicLabel then cSerializeName else FieldName,
                                    if valid-handle(phBuffer) then QueryUtil:GetOperatorText("eq") else "eq",
                                    StringValues[i],
                                    Strength:ToString()
                              ).            
            end.
            cExpression = subst("(&1)",cExpression).
        end.    
        else if Operator = QueryOperatorEnum:InRange then 
        do i = 1 to extent(StringValues):
            cExpression = cExpression 
                        + (if i > 1 then " and " else "")
                        + subst(cExpTemplate,
                                if plUsePublicLabel then cSerializeName else FieldName,
                                if valid-handle(phBuffer) then QueryUtil:GetOperatorText(if i = 1 then "ge" else "le") else if i = 1 then "ge" else "le",
                                StringValues[i],
                                Strength:ToString()
                          ).            
        end.    
        else do:
            cExpression = subst(cExpTemplate,
                                if plUsePublicLabel then cSerializeName else FieldName,
                                if valid-handle(phBuffer) then QueryUtil:GetOperatorText(Operator:ToString()) else lc(Operator:ToString()),
                                StringValue,
                                Strength:ToString()
                          ).
        end.
       
        return cExpression. 
    end.
    

end class.