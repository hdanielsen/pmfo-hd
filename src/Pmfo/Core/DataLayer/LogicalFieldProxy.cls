
/*------------------------------------------------------------------------
    File        : IFieldProxy
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Sep 26 06:56:59 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.
 
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.
using Pmfo.Core.DataLayer.AbstractFieldProxy from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Pmfo.Core.BusinessLogic.IQueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.Error.NotImplementedError from propath.

class Pmfo.Core.DataLayer.LogicalFieldProxy inherits AbstractFieldProxy:  
    
    define public property TrueStringValue as character no-undo get. set. 
    define public property FalseStringValue as character no-undo get. set.
    
    // Set to true to inerprete bad data as FALSE on read while still using FalseStringValue on save of FALSE  
    //  
    define public property ErrorOnReadIsFalse as logical no-undo get. set.
     
    define public override property StringValue      as character no-undo 
        get():
            return if this-object:Field:buffer-value = true then TrueStringValue else FalseStringValue.
        end.
    
    constructor public LogicalFieldProxy (pcFieldName as character, pcSourceName as character, pcTrueFalse as character ):
        this-object(pcFieldName,pcSourceName,pcTrueFalse,false). 
    end constructor. 
    
    constructor public LogicalFieldProxy (pcFieldName as character, pcSourceName as character, pcTrueFalse as character, plReadOnly as logical):
        super(pcfieldname,pcSourceName,plReadOnly).
        
        if num-entries(pcTrueFalse,"/") <> 2 then
            undo, throw new IllegalArgumentError(subst("LogicalFieldProxy &1 TrueFalse parameter must be specified with 'true/false' values",pcFieldname)).
        TrueStringValue  = entry(1,pcTrueFalse,"/").    
        FalseStringValue = entry(2,pcTrueFalse,"/").
        if FalseStringValue  = "?" then FalseStringValue = ?. // all other values = false
        
    end constructor. 
    
    // Deprectate and use the above
    constructor public LogicalFieldProxy (phField as handle,phDbBuffer as handle, pcSourceName as character, pcTrueFalse as character ):
        super (phfield, phDbBuffer,pcSourceName).
        
        AssertField(phField).    
        
        if num-entries(pcTrueFalse,"/") <> 2 then
            undo, throw new IllegalArgumentError(subst("LogicalFieldProxy &1 Trueandfalse parameter must be specified with 'true/false' values",phField:name)).
        TrueStringValue  = entry(1,pcTrueFalse,"/").    
        FalseStringValue = entry(2,pcTrueFalse,"/").
        if FalseStringValue  = "?" then FalseStringValue = ?. // all other values = false
            
    end constructor.
    
    method protected override void AssertField(phField as handle):
        if phfield:data-type <> "logical" then
            undo, throw new IllegalOperationError(subst("Create of LogicalFieldProxy with &1 field '&2'. Use FieldProxy instead.",phField:data-type,phField:name)).
          
    end method.
    
    method public override logical Save():
        if CheckAndAssertSave() then 
        do:
            if this-object:Field:buffer-value = true then
            do:
                SourceField:buffer-value(SourceExtent) = TrueStringValue.
            end.     
            else if this-object:Field:buffer-value = false then 
            do:
                // if FalseStringValue ? then we store default value which requires no action\
                if FalseStringValue <> ? then 
                    SourceField:buffer-value(SourceExtent) = FalseStringValue.
            end.
            else // if we get here with null then save it.. 
                SourceField:buffer-value(SourceExtent) = ?.
                    
            return  true.
        end.
        else 
            return false.    
    end method.    
   
    method protected override logical Conflict(phField as handle):
        if phField:buffer-value = true then
        do:
            if SourceField:buffer-value(SourceExtent) <> TrueStringValue then 
                return true.  
        end.        
        else if phField:buffer-value = false  then
        do:
            // if FalseStringValue ? then we store default value which requires no action\
            if FalseStringValue <> ? 
            and ErrorOnReadIsFalse = false then
            do: 
               if SourceField:buffer-value(SourceExtent) <> FalseStringValue then 
                   return true.
            end.         
            // if if FalseStringValue ? or Erroronreadisfalse then any value except the truevalue is ok   
            // this is somewhat questionable, but if data is saved from the data soruce it will only have valid values
            else if SourceField:buffer-value(SourceExtent) = TrueStringValue then
            do:
                return true.
            end.     
        end.
        else do:
            if SourceField:buffer-value(SourceExtent) <> ? then 
                return true.
        end.
    end method.    
         
    method public override IQueryEntry ColumnExpression(input pPredicate as IQueryPredicate):
        define variable cValues as character extent no-undo.
        define variable i as integer no-undo.
        
        if pPredicate:Operator = QueryOperatorEnum:Eq or pPredicate:Operator = QueryOperatorEnum:Ne then
        do: 
            // if falseStrringvalue is any value (?) or Erroronread is allowed then flip the operator and search on true value   
            if (FalseStringValue = ? or ErrorOnReadIsFalse = true) and logical(pPredicate:StringValue) = false then  
            do: 
                case pPredicate:operator: 
                    when QueryOperatorEnum:Eq then 
                        return new QueryPredicate(SourceFullName,QueryOperatorEnum:NE,TrueStringValue).
                    otherwise
                        return new QueryPredicate(SourceFullName,QueryOperatorEnum:EQ,TrueStringValue).
                end.
            end.
            
            return new QueryPredicate(SourceFullName,pPredicate:Operator,if logical(pPredicate:StringValue) then TrueStringValue else FalseStringValue). 
        end.      
        else if extent(pPredicate:StringValues) <> ? and (FalseStringValue = ? or ErrorOnReadIsFalse = true) and pPredicate:Operator = QueryOperatorEnum:InList then 
        do:
            if pPredicate:Operator = QueryOperatorEnum:InList then
                return ConvertArrayToSingleEntries(pPredicate).
            else
                return new QueryPredicate(subst("logical(if &1='&2' then true else false)",
                                               SourceFullName,
                                               TrueStringValue),
                                               pPredicate:Operator,
                                               pPredicate:StringValues).       
        end.   
        
        else if extent(pPredicate:StringValues) <> ? and FalseStringValue <> ? and ErrorOnReadIsFalse = false then 
        do: 
            extent(cValues) = extent(pPredicate:StringValues).
            do i = 1 to extent(cValues):
                cValues[i] = if logical(pPredicate:StringValues[i]) then TrueStringValue else FalseStringValue. 
            end.    
            return new QueryPredicate(SourceFullName,pPredicate:Operator,cValues).
        end.   
        else do:
            return new QueryPredicate(subst("logical(if &1='&2' then true else false)",
                                            SourceFullName,
                                            TrueStringValue),
                                      pPredicate:Operator,
                                      pPredicate:StringValue).   
        end.    
    end.
    
    // when falseStringvalue = ? convert inlist to single entry predicates with true value and flipped operator instead of false
    // It is somewhat unlikley that inrange or inlisat will be used for logical values, but it should work  
    method protected IQueryEntry ConvertArrayToSingleEntries(input pPredicate as IQueryPredicate) :
        define variable i as integer no-undo.
        define variable oOperator as QueryOperatorEnum no-undo.
        define variable oQueryGroup as IQueryGroup no-undo.
        
        if pPredicate:Operator = QueryOperatorEnum:InList then
        do:
            oQueryGroup = new QueryGroup().      
            do i = 1 to extent(pPredicate:StringValues): 
                if logical(pPredicate:StringValues[i]) = false then 
                    oOperator = QueryOperatorEnum:NE.
                else        
                    oOperator = QueryOperatorEnum:EQ.
                    
                oQueryGroup:AddEntry(new QueryPredicate(if i > 1 then JoinEnum:or else JoinEnum:none,SourceFullName,oOperator,TrueStringValue)). 
            
            end.    
            return oQueryGroup.
        
        end.
        return pPredicate.  
    end.    
    
    method public override IQuerySortEntry ColumnSort(input pSort as IQuerySortEntry ):
        // if the true value is greater than false then just sort on the field 
        if TrueStringValue > FalseStringValue and FalseStringValue <> ? then
            return new QuerySortEntry(SourceFullName,pSort:SortOrder).
        else   
            return new QuerySortEntry(subst("logical(if &1='&2' then true else false)",SourceFullName,TrueStringValue),pSort:SortOrder).
    end.
  
    method override void SetBufferValue(pValue as character):
        
        if pValue = TrueStringValue then
            this-object:Field:buffer-value = true.
        else if pValue = FalseStringValue then
           this-object:Field:buffer-value = false.
        else do:
            if ErrorOnReadIsFalse then 
                this-object:Field:buffer-value = false.
            else    
                undo, throw new IllegalStateError(subst("Database field '&1' value '&2' does not match any of the defined logical values for &3",SourceFullName,pValue,this-object:Field:name)).
        end.      
    end.
    
    method override void SetBufferValue(phBuffer as handle):
        define variable hFld as handle no-undo.
        hfld = phBuffer:buffer-field(SourceName).
        SetBufferValue(hfld:buffer-value).
    end.  
        
    method override void SetBufferValue(pValue as integer):
        SetBufferValue(string(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as int64):
        SetBufferValue(string(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as logical):
        SetBufferValue(string(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as decimal):
        SetBufferValue(string(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as date):
        SetBufferValue(string(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as datetime):
        SetBufferValue(string(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as datetime-tz):
        SetBufferValue(string(pValue)).
    end.    
    
end class.