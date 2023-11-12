
/*------------------------------------------------------------------------
    File        : ZeroAsNullProxy
    Purpose     : Manage 0 in db as ? - i.e Non progress UI typically shows null as blank which is better than 0 
                  for cases where 0 means none in input or dropdown or others)  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon May 15 2023 
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.
 
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.
using Pmfo.Core.DataLayer.CustomFieldProxy from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.StringUtil from propath.
 
class Pmfo.Core.DataLayer.ZeroAsNullProxy inherits CustomFieldProxy:  
    
     
    define private property SUPPORTED_DATATYPES as character init "integer,int64,decimal" no-undo get.
    
    define public override property StringExpression as character no-undo 
        get():
            return string(if sourcefield:buffer-value(SourceExtent) = 0 then ? else sourcefield:buffer-value((SourceExtent))).
        end.
    
    constructor public ZeroAsNullProxy (pcCommonFieldName as character):
         this-object(pcCommonFieldName,pcCommonFieldName).
    end constructor.
   
    constructor public ZeroAsNullProxy (pcFieldName as character, pcSourceFieldName as character):
         super(pcFieldName,pcSourceFieldName).
    end constructor.
    
    method protected override void  AssertField(phField as handle):
        if lookup(phField:data-type,SUPPORTED_DATATYPES) = 0 then
            undo, throw new UnsupportedOperationError(subst("Field '&1' is a '&2' field. Only &3 fields can be be defined as ZeroAsNullProxy",
                                                            phField:name,
                                                            phField:data-type,
                                                            StringUtil:GetMessageFromEntries(SUPPORTED_DATATYPES,"or",true) // true quote entries
                                                           )
                                                      ).
    end method.
    
    method protected override void  AssertSourceField(phField as handle):
        if lookup(phField:data-type,SUPPORTED_DATATYPES) = 0 then
            undo, throw new UnsupportedOperationError(subst("Database Field '&1.&2' is a '&3' field. Only &4 fields can be be defined as ZeroAsNullProxy",
                                                            phField:buffer-handle:name,
                                                            phField:name,
                                                            phField:data-type,
                                                            StringUtil:GetMessageFromEntries(SUPPORTED_DATATYPES,"or",true) // true quote entries
                                                           )
                                                      ).
    end method.
    
    method protected override logical Conflict(phField as handle):
        if phField:buffer-value = ? then
        do: 
            // if mandatory there cannot be ? , but the check is still ok 
            return SourceField:buffer-value(SourceExtent) <> ?
                   and
                   SourceField:buffer-value(SourceExtent) <> 0.
        end.
        else 
            return phField:buffer-value <> SourceField:buffer-value(SourceExtent).    
    end method.                                                  
    
    method public override logical Save():
        if CheckAndAssertSave() then 
        do:
           // Progress old magic runtime type conversion will box data type if possible 
            // a proxy should only be defined if the data type can be converted automatically 
            // override for other cases use customFieldProxy or just override this 
            SourceField:buffer-value(SourceExtent) = if this-object:Field:buffer-value = ? then 0 else this-object:Field:buffer-value.
            return  true.
        end.    
        else 
            return false.
    end method.
    
    method public override IQueryEntry ColumnExpression(input pPredicate as IQueryPredicate):
        define variable iNull as integer no-undo.
        define variable cValues as character extent no-undo.
        if extent(pPredicate:Stringvalues) <> ? then 
        do:  
            iNull = Array:Find(?,ppredicate:StringValues).            
            if iNull > 0 then 
            do:
                if SourceField:mandatory = false then
                    return super:ColumnExpression(pPredicate). // if then else expression using stringvalue to include both 0 and ? 
                else 
                    pPredicate:StringValues[iNull] = "0".
            end.
        end.             
        else if pPredicate:StringValue = ? then  
        do:
            if SourceField:mandatory = false then
                return super:ColumnExpression(pPredicate). // if then else expression using stringvalue to include both 0 and ?
            else         
                pPredicate:StringValue = "0".
        end.  
        return pPredicate.       
    end.
    
    method public override IQuerySortEntry ColumnSort(input pSort as IQuerySortEntry ):
        if SourceField:mandatory = false then
             return new QuerySortEntry(StringExpression,pSort:SortOrder).
        else 
            return pSort.           
    end.
    
    method override void SetBufferValue(pValue as integer):
         this-object:Field:buffer-value = (if pValue = 0 then ? else pValue).
    end.    
    
    method override void SetBufferValue(pValue as int64):
         this-object:Field:buffer-value = pValue.
    end.    
    
    method override void SetBufferValue(pValue as decimal):
         this-object:Field:buffer-value = pValue.
    end.    
   
    // remaining data types are not supported
      
    method override void SetBufferValue(pValue as character):
        undo, throw new NotImplementedError("SetBufferValue(character)"). 
    end.
         
    method override void SetBufferValue(pValue as logical):
        undo, throw new NotImplementedError("SetBufferValue(logical)"). 
    end.    
    
    
    method override void SetBufferValue(pValue as date):
        undo, throw new NotImplementedError("SetBufferValue(date)"). 
    end.    
    
    method override void SetBufferValue(pValue as datetime):
        undo, throw new NotImplementedError("SetBufferValue(datetime)"). 
    end.    
    
    method override void SetBufferValue(pValue as datetime-tz):
        undo, throw new NotImplementedError("SetBufferValue(datetime-tz)"). 
    end.    
    
end class.