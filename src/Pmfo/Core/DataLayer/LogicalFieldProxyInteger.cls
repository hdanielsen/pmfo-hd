
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
 
using Ccs.Common.Application from propath.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.

using Pmfo.Core.BusinessLogic.IQueryGroup from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.
using Pmfo.Core.DataLayer.AbstractFieldProxy from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Common.NameService from propath.
using Pmfo.Core.Common.INameService from propath.

class Pmfo.Core.DataLayer.LogicalFieldProxyInteger inherits AbstractFieldProxy:  
    
    define public property TrueValue as integer no-undo init 1 get. set. 
    define public property FalseValue as integer no-undo get. set.
    
    // Set to true to inerprete bad data as FALSE on read while still using FalseStringValue on save of FALSE  
    define public property ReturnFalseOnError as logical no-undo get. set.
     
    define public override property StringValue      as character no-undo 
        get():
            return string(if this-object:Field:buffer-value = true then TrueValue else FalseValue).
        end.
    
    define protected property NameService as INameService no-undo 
       get():
           if not valid-object(NameService) then 
           do:
               NameService = cast(Application:ServiceManager:getService(get-class(INameService)),INameService).
           end.
           return NameService.
       end.        
       set. 
    
    constructor public LogicalFieldProxyInteger (pcFieldName as character, pcSourceName as character):
         super(pcfieldname,pcSourcename).
           
    end method.
    
    constructor public LogicalFieldProxyInteger (pcFieldName as character, pcSourceName as character, piSourceExtent as integer):
         super(pcfieldname,pcSourcename,piSourceExtent).
    end method.
         
    constructor public LogicalFieldProxyInteger (pcFieldName as character, pcSourceName as character, piTrue as integer , piFalse as integer ):
        super(pcfieldname,pcSourceName).
        
        TrueValue  = piTrue.    
        FalseValue = piFalse.
        if FalseValue  = ? then FalseValue = ?. // all other values = false
        
    end constructor. 
    
    method protected override void AssertField(phField as handle):
        if phfield:data-type <> "logical" then
            undo, throw new IllegalOperationError(subst("Create of LogicalFieldProxy with &1 field '&2'. Use FieldProxy instead.",phField:data-type,phField:name)).
          
    end method.
    
    method public override logical Save():
        define variable iOld as integer no-undo.
        iOLd =  SourceField:buffer-value(SourceExtent).
        if this-object:Field:buffer-value = true then
        do:
            SourceField:buffer-value(SourceExtent) = TrueValue.
        end.     
        else if this-object:Field:buffer-value = false then 
        do:
            // if FalseStringValue ? then we store default value which requires no action\
            if FalseValue <> ? then 
                SourceField:buffer-value(SourceExtent) = FalseValue.
            else do:
                if SourceField:buffer-value(SourceExtent) = TrueValue then
                do: 
                    undo, throw new UnsupportedOperationError(subst("Cannot set &1 to false. Update &2 to a value different than &3 instead",
                                                                    this-object:Field:serialize-name,
                                                                    // todo option to define the name 
                                                                    NameService:PublicFieldName(SourceField:name),
                                                                    TrueValue 
                                                                    )
                                                              ). 
                end.
            end.        
        end.
        else // if we get here with null then save it.. 
            SourceField:buffer-value(SourceExtent) = ?.
                
        return iOld <> SourceField:buffer-value(SourceExtent).    
    end method.    
   
    method public override logical Conflict(phField as handle):
        if phField:buffer-value = true then
        do:
            if SourceField:buffer-value(SourceExtent) <> TrueValue then 
                return true.  
        end.        
        else if phField:buffer-value = false  then
        do:
            // if FalseStringValue ? then we store default value which requires no action\
            if FalseValue <> ? 
            and ReturnFalseOnError = false then
            do: 
               if SourceField:buffer-value(SourceExtent) <> FalseValue then 
                   return true.
            end.         
            // if if FalseStringValue ? or ReturnFalseOnError then any value except the truevalue is ok   
            // this is somewhat questionable, but if data is saved from the data soruce it will only have valid values
            else if SourceField:buffer-value(SourceExtent) = TrueValue then
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
            if (FalseValue = ? or ReturnFalseOnError = true) and logical(pPredicate:StringValue) = false then  
            do: 
                case pPredicate:operator: 
                    when QueryOperatorEnum:Eq then 
                        return new QueryPredicate(SourceFullName,QueryOperatorEnum:NE,TrueValue).
                    otherwise
                        return new QueryPredicate(SourceFullName,QueryOperatorEnum:EQ,TrueValue).
                end.
            end.
            
            return new QueryPredicate(SourceFullName,pPredicate:Operator,if logical(pPredicate:StringValue) then TrueValue else FalseValue). 
        end.      
        else if extent(pPredicate:StringValues) <> ? and (FalseValue = ? or ReturnFalseOnError = true) and pPredicate:Operator = QueryOperatorEnum:InList then 
        do:
            if pPredicate:Operator = QueryOperatorEnum:InList then
                return ConvertArrayToSingleEntries(pPredicate).
            else
                return new QueryPredicate(subst("logical(if &1=&2 then true else false)",
                                               SourceFullName,
                                               TrueValue),
                                               pPredicate:Operator,
                                               pPredicate:StringValues).       
        end.   
        
        else if extent(pPredicate:StringValues) <> ? and FalseValue <> ? and ReturnFalseOnError = false then 
        do: 
            extent(cValues) = extent(pPredicate:StringValues).
            do i = 1 to extent(cValues):
                cValues[i] = if logical(pPredicate:StringValues[i]) then string(TrueValue) else string(FalseValue). 
            end.    
            return new QueryPredicate(SourceFullName,pPredicate:Operator,cValues).
        end.   
        else do:
            return new QueryPredicate(subst("logical(if &1=&2 then true else false)",
                                            SourceFullName,
                                            TrueValue),
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
                    
                oQueryGroup:AddEntry(new QueryPredicate(if i > 1 then JoinEnum:or else JoinEnum:none,SourceFullName,oOperator,TrueValue)). 
            
            end.    
            return oQueryGroup.
        
        end.
        return pPredicate.  
    end.    
    
    method public override IQuerySortEntry ColumnSort(input pSort as IQuerySortEntry ):
        // if the true value is greater than false then just sort on the field 
        if TrueValue > FalseValue and FalseValue <> ? then
            return new QuerySortEntry(SourceFullName,pSort:SortOrder).
        else   
            return new QuerySortEntry(subst("logical(if &1=&2 then true else false)",SourceFullName,TrueValue),pSort:SortOrder).
    end.
  
    method override void SetBufferValue(pValue as integer):
        
        if pValue = TrueValue then
            this-object:Field:buffer-value = true.
        else if pValue = FalseValue then
           this-object:Field:buffer-value = false.
        else do:
            if ReturnFalseOnError or FalseValue = ? then 
                this-object:Field:buffer-value = false.
            else    
                undo, throw new IllegalStateError(subst("Database field '&1' value '&2' does not match any of the defined logical values for &3",SourceFullName,pValue,this-object:Field:name)).
        end.      
    end.
    
    method override void SetBufferValue(phBuffer as handle):
        define variable hFld as handle no-undo.
        hfld = phBuffer:buffer-field(SourceName).
        SetBufferValue(integer(hfld:buffer-value(SourceExtent))).
    end.  
        
    method override void SetBufferValue(pValue as character):
        SetBufferValue(integer(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as int64):
        SetBufferValue(integer(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as logical):
        SetBufferValue(integer(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as decimal):
        SetBufferValue(integer(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as date):
        SetBufferValue(integer(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as datetime):
        SetBufferValue(integer(pValue)).
    end.    
    
    method override void SetBufferValue(pValue as datetime-tz):
        SetBufferValue(integer(pValue)).
    end.    
    
end class.