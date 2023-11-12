 
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

using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using Pmfo.Core.BusinessLogic.AbstractQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.QueryUtil from propath.
 

class Pmfo.Core.BusinessLogic.QueryPredicate inherits AbstractQueryPredicate implements IQueryPredicate: 
    define override property NotImplementedReasonTemplate as character no-undo 
        init "&1 property is not implemented. Use &2 instead. The predicate is only used for dynamic queries and string is sufficient and ensures safe dates and decimals."
    get.   
    
    define public property StringValue  as character         no-undo get. set. 
    define public property StringValues as character extent  no-undo get. set. 

    define public override property Value as IPrimitiveHolder no-undo 
        get():
            undo, throw new NotImplementedError(subst(NotImplementedReasonTemplate,"Value","StringValue")).
        end.
        protected set(pValue as IPrimitiveHolder):
            undo, throw new NotImplementedError(subst(NotImplementedReasonTemplate,"Value","StringValue")).
        end. 

    define public override property Values as IPrimitiveArrayHolder no-undo 
      get( ):
          undo, throw new NotImplementedError(subst(NotImplementedReasonTemplate,"Values","StringValues")).
      end.
      protected set(pValue as IPrimitiveArrayHolder):
          undo, throw new NotImplementedError(subst(NotImplementedReasonTemplate,"Values","StringValues")).
      end. 

    /* use QueryUtil GetQueryPredicate  
    constructor public QueryPredicate (input pExpression as character):
        define variable cWork  as character no-undo.
        define variable iStart as integer   no-undo.
        define variable iEnd   as integer no-undo.
        
        assign
            pExpression = trim(pExpression)
            cWork       = StringUtil:MaskQuotes(pExpression,"@")
            iStart      = index(cWork,"@")
            iEnd        = r-index(cWork,"@")
            .
        
        do while index(cWork,StringConstant:SPACE + StringConstant:SPACE) > 0:
            cWork = replace(cWork,StringConstant:SPACE + StringConstant:SPACE,StringConstant:SPACE).
        end.    
        
        if num-entries(cWork,StringConstant:SPACE) <> 3 then 
            undo, throw new IllegalArgumentError(subst("Invalid expression",pExpression)).
        
        assign
            FieldName   = entry(1,cWork,StringConstant:SPACE)
            Operator    = QueryUtil:GetOperatorEnum(entry(2,cWork,StringConstant:SPACE))
            .
        if iStart > 0 then    
            StringValue = substr(pExpression,iStart + 1,iEnd - 1 - iStart).
        else 
            StringValue = entry(3,cWork,StringConstant:SPACE).    
            
    end constructor.
    */
   
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param character value */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator as QueryOperatorEnum,
                                       input pValue    as character):
        
        super(pFieldName,pOperator).
        
        if    pOperator = QueryOperatorEnum:InList
           or pOperator = QueryOperatorEnum:InRange
        then
            undo, throw new IllegalArgumentError(substitute('The operator cannot be &1 for a scalar value',
                                                    string(pOperator))).
        
        assign StringValue = pValue.
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param integer value */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValue     as integer):
        
        this-object(pFieldName,pOperator,string(pValue)).
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param integer values  extent */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues     as integer extent):
        
        this-object(pFieldName,pOperator,Array:ToStringArray(pValues)).
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param int64 value */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValue     as int64):
        
        this-object(pFieldName,pOperator,string(pValue)).
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param int64 values extent */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues   as int64 extent):
        
        this-object(pFieldName,pOperator,Array:ToStringArray(pValues)).
    end constructor.
    
     /* @param character field name 
       @param QueryOperatorEnum operator 
       @param decimal value */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValue     as decimal):
        
        this-object(pFieldName,pOperator,string(pValue)).
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param decimal values extent */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues   as decimal extent):
        
        this-object(pFieldName,pOperator,Array:ToStringArray(pValues)).
    end constructor.
    
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param logical value */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValue     as logical):
        
        this-object(pFieldName,pOperator,string(pValue)).
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param logical values extent */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues     as logical extent):
        this-object(pFieldName,pOperator,Array:ToStringArray(pValues)). 
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param date value */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValue     as date):
        
        this-object(pFieldName,pOperator,string(pValue)).
    end constructor.
    
     /* @param character field name 
       @param QueryOperatorEnum operator 
       @param date value extent */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues     as date extent):
        
        this-object(pFieldName,pOperator,Array:ToStringArray(pValues)).
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param datetime value */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValue     as datetime):
        
        this-object(pFieldName,pOperator,string(pValue)).
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param datetime value extent */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues     as datetime extent):
        
        this-object(pFieldName,pOperator,Array:ToStringArray(pValues)).
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param datetime-tz value */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValue     as datetime-tz ):
        
        this-object(pFieldName,pOperator,string(pValue)).
    end constructor.
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param datetime-tz value */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues     as datetime-tz extent):
        
        this-object(pFieldName,pOperator,Array:ToStringArray(pValues)).
    end constructor.
    
     
    
    /* @param join JoinEnum
       @param character field name 
       @param QueryOperatorEnum operator 
       @param character value */
    constructor public QueryPredicate (input pjoin as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator as QueryOperatorEnum,
                                       input pValue    as character):
        
        this-object(pFieldName,pOperator,pValue).
        assign this-object:Join = pJoin.
    end constructor.
    
    /* @param join JoinEnum
       @param character field name 
       @param QueryOperatorEnum operator 
       @param integer value */
    constructor public QueryPredicate (input pjoin as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator as QueryOperatorEnum,
                                       input pValue    as integer):
        
        this-object(pJoin,pFieldName,pOperator,string(pValue)).
    end constructor.
    
     /* @param join JoinEnum
       @param character field name 
       @param QueryOperatorEnum operator 
       @param integer value */
    constructor public QueryPredicate (input pjoin as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator as QueryOperatorEnum,
                                       input pValue    as int64):
        
        this-object(pJoin,pFieldName,pOperator,string(pValue)).
    end constructor.
    
     /* @param join JoinEnum
       @param character field name 
       @param QueryOperatorEnum operator 
       @param decimal value */
    constructor public QueryPredicate (input pjoin as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator as QueryOperatorEnum,
                                       input pValue    as decimal):
        
        this-object(pJoin,pFieldName,pOperator,string(pValue)).
    end constructor.
    
     /* @param join JoinEnum
       @param character field name 
       @param QueryOperatorEnum operator 
       @param date value */
    constructor public QueryPredicate (input pjoin as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator as QueryOperatorEnum,
                                       input pValue    as date):
        
        this-object(pJoin,pFieldName,pOperator,string(pValue)).
    end constructor.
    
    /* @param join JoinEnum
       @param character field name 
       @param QueryOperatorEnum operator 
       @param date value */
    constructor public QueryPredicate (input pjoin as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator as QueryOperatorEnum,
                                       input pValue    as datetime):
        
        this-object(pJoin,pFieldName,pOperator,string(pValue)).
    end constructor.
    
       /* @param join JoinEnum
       @param character field name 
       @param QueryOperatorEnum operator 
       @param date value */
    constructor public QueryPredicate (input pjoin as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator as QueryOperatorEnum,
                                       input pValue    as datetime-tz):
        
        this-object(pJoin,pFieldName,pOperator,string(pValue)).
    end constructor.
    
    
     /* @param join JoinEnum
       @param character field name 
       @param QueryOperatorEnum operator 
       @param logical value */
    constructor public QueryPredicate (input pjoin as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator as QueryOperatorEnum,
                                       input pValue    as logical):
        
        this-object(pJoin,pFieldName,pOperator,string(pValue)).
    end constructor.
    
    
    
    /* @param character field name 
       @param QueryOperatorEnum operator 
       @param character extent values */
    constructor public QueryPredicate (input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues    as character extent):
         super(pFieldName,pOperator).
        
        if pOperator = QueryOperatorEnum:InRange and extent(pValues) <> 2 then
            undo, throw new IllegalArgumentError('The values must have exactly 2 extents for the InRange operator').
                                                  
        if     pOperator <> QueryOperatorEnum:InList
           and pOperator <> QueryOperatorEnum:InRange
        then
            undo, throw new IllegalArgumentError(substitute('Predicate operator cannot be &1 for an array value',
                                                    string(pOperator))).        
        assign StringValues = pValues.
    end constructor.
    
    constructor public QueryPredicate (input pjoin      as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues    as character extent):
        
        this-object(pFieldName,pOperator,pValues).
        assign this-object:Join = pJoin.
    end constructor.
    
    constructor public QueryPredicate (input pjoin      as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues    as int64 extent):
        
        this-object(pFieldName,pOperator,pValues).
        assign this-object:Join = pJoin.
    end constructor.
    
    constructor public QueryPredicate (input pjoin      as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues    as logical extent):
        
        this-object(pFieldName,pOperator,pValues).
        assign this-object:Join = pJoin.
    end constructor.
    
    constructor public QueryPredicate (input pjoin      as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues    as integer extent):
        
        this-object(pFieldName,pOperator,pValues).
        assign this-object:Join = pJoin.
    end constructor.
    
    constructor public QueryPredicate (input pjoin      as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues    as decimal extent):
        
        this-object(pFieldName,pOperator,pValues).
        assign this-object:Join = pJoin.
    end constructor.
    
    constructor public QueryPredicate (input pjoin      as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues    as date extent):
        
        this-object(pFieldName,pOperator,pValues).
        assign this-object:Join = pJoin.
    end constructor.
    
    constructor public QueryPredicate (input pjoin      as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues    as datetime extent):
        
        this-object(pFieldName,pOperator,pValues).
        assign this-object:Join = pJoin.
    end constructor.
    
    constructor public QueryPredicate (input pjoin      as JoinEnum,
                                       input pFieldName as character,
                                       input pOperator  as QueryOperatorEnum,
                                       input pValues    as datetime-tz extent):
        
        this-object(pFieldName,pOperator,pValues).
        assign this-object:Join = pJoin.
    end constructor.
    
    constructor public QueryPredicate (input pFieldName as character, input pOperator as QueryOperatorEnum, input pValues as IPrimitiveArrayHolder ):
        this-object (input pFieldName, input pOperator, input Array:ToStringArray(pValues)).
    end constructor.
    
    constructor public QueryPredicate( input pjoin as JoinEnum, input pFieldName as character, input pOperator as QueryOperatorEnum, input pValues as IPrimitiveArrayHolder ):
        this-object (input pjoin, input pFieldName, input pOperator, input Array:ToStringArray(pValues)).
    end constructor.

    constructor public QueryPredicate( input pjoin as JoinEnum, input pFieldName as character, input pOperator as QueryOperatorEnum, input pValue as IPrimitiveHolder):
        this-object (input pjoin, input pFieldName, input pOperator, input pValue:ToString()).
    end constructor.
    
    constructor public QueryPredicate ( input pFieldName as character, input pOperator as QueryOperatorEnum, input pValue as IPrimitiveHolder):
        this-object (input pFieldName, input pOperator, input pValue:ToString()).
    end constructor.
    
    method override public Progress.Lang.Object Clone(  ):
        if extent(StringValues) <> ? then
            return new QueryPredicate(FieldName,Operator,StringValues).
        else      
            return new QueryPredicate(FieldName,Operator,StringValue).
    end method.

    // join is not included in tostring - it is used in QueryGroup when more than one predicate is shown 
    method override public character ToString():
        return ToString(?,false). 
    end.
    
    method public override character ToString(phBuffer as handle,plUsePublicLabel as logical):
        return ToString(phbuffer,plUsePublicLabel,false).
    end method.     
    
    // join is not included in tostring - it is used in QueryGroup when more than one predicate is shown 
    method public override character ToString(phBuffer as handle,plUsePublicLabel as logical, plTrimParenthesis as logical):
        define variable i as integer no-undo.
        define variable cExpression    as character no-undo.
        define variable lUseQuoter     as logical no-undo.
        define variable cDataType      as character no-undo.
        define variable cExpTemplate   as character init '&1 &2 "&3"' no-undo.
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
            lUseQuoter = cDataType = "character" or cDataType begins "date". 
        end.
        else 
            lUseQuoter = true.
            
        if Operator = QueryOperatorEnum:InList then
        do: 
            
            do i = 1 to extent(StringValues):
                cExpression = cExpression 
                            + (if i > 1 then " or " else "")
                            + subst('&1 &2 &3',
                                    if plUsePublicLabel then cSerializeName else FieldName,
                                    if valid-handle(phBuffer) then QueryUtil:GetOperatorText("eq") else "eq",
                                    if luseQuoter then quoter(StringValues[i]) else StringValues[i]).            
            end.
            if plTrimParenthesis = false then
                cExpression = subst("(&1)",cExpression).
        end.    
        else if Operator = QueryOperatorEnum:InRange then 
        do i = 1 to extent(StringValues):
            cExpression = cExpression 
                        + (if i > 1 then " and " else "")
                        + subst('&1 &2 &3',
                                if plUsePublicLabel then cSerializeName else FieldName,
                                if valid-handle(phBuffer) then QueryUtil:GetOperatorText(if i = 1 then "ge" else "le") else if i = 1 then "ge" else "le",
                                if luseQuoter then quoter(StringValues[i]) else StringValues[i]).            
        end.
        else do:
            cExpression = subst('&1 &2 &3',
                                if plUsePublicLabel then cSerializeName else FieldName,
                                if valid-handle(phBuffer) then QueryUtil:GetOperatorText(Operator:ToString()) else lc(Operator:ToString()),
                                if luseQuoter then quoter(StringValue) else StringValue).
        end.
        return cExpression. 
        
    end.
    
end class.