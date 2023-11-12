
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
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.
using Pmfo.Core.DataLayer.AbstractFieldProxy from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
 
class Pmfo.Core.DataLayer.CustomFieldProxy abstract inherits AbstractFieldProxy:  
    
    define protected abstract property StringExpression as character no-undo get.
    
    define public override property StringValue as character no-undo 
        get():
            return string(this-object:Field:buffer-value).
        end.
    
    constructor public CustomFieldProxy (pcFieldName as character, pcSourceFieldName as character):
         super(pcFieldName,pcSourceFieldName).
    end constructor.
    
    method protected abstract override logical Conflict(phField as handle): 
    method public abstract override logical Save():
    
    method public override IQueryEntry ColumnExpression(input pPredicate as IQueryPredicate):
        cast(pPredicate,QueryPredicate):FieldName = StringExpression.
        return pPredicate.       
    end.
    
    method public override IQuerySortEntry ColumnSort(input pSort as IQuerySortEntry ):
        return new QuerySortEntry(StringExpression,pSort:SortOrder).       
    end.
    
    method override void SetBufferValue(pValue as character):
        undo, throw new NotImplementedError("SetBufferValue(character)"). 
    end.
         
    method override void SetBufferValue(pValue as integer):
        undo, throw new NotImplementedError("SetBufferValue(integer)"). 
    end.    
    
    method override void SetBufferValue(pValue as int64):
        undo, throw new NotImplementedError("SetBufferValue(int64)"). 
    end.    
    
    method override void SetBufferValue(pValue as logical):
        undo, throw new NotImplementedError("SetBufferValue(logical)"). 
    end.    
    
    method override void SetBufferValue(pValue as decimal):
        undo, throw new NotImplementedError("SetBufferValue(decimal)"). 
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