
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
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.DataLayer.IFieldProxy from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.OutOfBoundsError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Util.BufferUtil from propath.

class Pmfo.Core.DataLayer.AbstractFieldProxy abstract implements IFieldProxy:  
    define public property Name             as character no-undo get. protected set.
    define public property SourceName       as character no-undo get. protected set.
    define public property SourceDataType   as character no-undo get. protected set.
    define public property SourceBufferName as character no-undo get. protected set.
    // fake name for ColumnSource
    define public property ColumnSourceName as character no-undo get. protected set.
    define public property SourceFullName   as character no-undo get. protected set.
    define public property Field            as handle    no-undo get. protected set.
    define public property BeforeField      as handle    no-undo get. protected set.
    define public property SourceField      as handle    no-undo get. protected set.
    define public property IsMandatory      as logical   no-undo 
        get():
            return sourcefield:mandatory.
        end. 
        
        
    define public property ReadOnly  as logical   no-undo get. protected set.
      // keep track of old constructors  
    define public property Prepared  as logical   no-undo get. protected set.
    define protected property SourceExtent       as integer no-undo get. set.  
    
    constructor protected AbstractFieldProxy (pcFieldName as character,pcSourceName as character):
         Assert:NotEmpty(pcFieldName,"Field Name").
         Assert:NotEmpty(pcSourceName,"Source Field Name").
         
         Name       = pcFieldName.
         SourceName = pcSourceName.
         
    end constructor.
    
    constructor protected AbstractFieldProxy (pcFieldName as character,pcSourceName as character, plReadOnly as logical):
         this-object(pcFieldName,pcSourceName).
    end constructor.
    
    constructor protected AbstractFieldProxy (pcFieldName as character,pcSourceName as character, piSourceExtent as integer):
         Name       = pcFieldName.
         SourceName = pcSourceName.
         if (piSourceExtent < 1) then 
             undo, throw new IllegalArgumentError(subst("Extent &1. Use constructor(character,character) for non extent fields",piSourceExtent)).  
        SourceExtent = piSourceExtent.
    end constructor.
    
    constructor protected AbstractFieldProxy (pcFieldName as character,pcSourceName as character, piSourceExtent as integer, plReadOnly as logical):
         this-object(pcFieldName,pcSourceName,piSourceExtent).
         ReadOnly   = plReadOnly.
    end constructor.
    
    // allow subclasses to override and check field i.e. data type  
    method protected void AssertField(phField as handle):
    end method.    
    
    method protected void AssertSourceField(phField as handle):
    end method.    
    
    // allow subclasses to override to track other tt fields   
    method protected void PrepareBuffer(phDataBuffer as handle):
    end method.
    
    // allow subclasses to override to track other db fields   
    method protected void PrepareDatabaseBuffer(phDbBuffer as handle):
    end method.
    
    // did not need this ... 
/*    method public void ResetSourceName(pcname as character) :                            */
/*        SourceBufferName = pcname.                                                       */
/*        if SourceExtent > 0 then                                                         */
/*            SourceFullName = subst("&1.&2[&3]",SourceBufferName,SourceName,SourceExtent).*/
/*        else                                                                             */
/*            SourceFullName = subst("&1.&2",SourceBufferName,SourceName).                 */
/*        /* This reference will likely not exist, but is to be returned from              */
/*           ColumnSource to be added to the QueryPredicate and QuerySortEntry             */
/*           and ColumnExpression and ColumnSort will then use this fieldname              */
/*           to generate/return the final DB expression/predicate      */                  */
/*                                                                                         */
/*        ColumnSourceName = subst("&1.&2",SourceBufferName,Name).                         */
/*    end.                                                                                 */
    
    // final - this is set in stone. Admittedly optimistic assumption change of required 
    // but AssertField, AssertSourceField, Preparebuffer and PrepareDatabaseBuffer 
    // should allow subclasses to control behavior  
    method public final logical Prepare(phDataBuffer as handle, phDatabaseBuffer as handle):
        
        if this-object:Name = ?
        or this-object:Name = "" then 
            undo, throw new IllegalStateError("Name of Proxy is not defined").  
         
        if this-object:SourceName = ?
        or this-object:SourceName = "" then 
            undo, throw new IllegalStateError("SourceName of Proxy is not defined").
        
        if not valid-handle(phDataBuffer) then 
            undo, throw new IllegalArgumentError("Data Buffer is not valid").       
        
        if not valid-handle(phDatabaseBuffer) then 
            undo, throw new IllegalArgumentError("Database buffer is not valid").
            
        this-object:Field = phDataBuffer:buffer-field(Name) no-error.
           
        if not valid-handle(this-object:Field) then 
            undo, throw new IllegalStateError(subst("Field '&1' not found in Buffer '&2'.",Name,phDatabuffer:name)).  
        
        
        AssertField(this-object:Field).
        SourceField = phDatabaseBuffer:buffer-field(SourceName) no-error.
         
        if not valid-handle(SourceField) then 
            undo, throw new IllegalStateError(subst("Field '&1' not found in Database Buffer '&2'.",SourceName,phDatabaseBuffer:name)).  
        
        if SourceExtent > 0 then 
        do:
            if SourceField:extent = 0 then 
                undo, throw new IllegalStateError(subst("Extent is defined, but Field '&1' in Database Buffer '&2' has no extents",SourceName,phDatabaseBuffer:name)).  
             
            if SourceExtent gt SourceField:extent then 
                undo, throw new OutOfBoundsError("Extent",SourceField:extent).  
           
        end.
        else 
            if SourceField:extent > 0 then 
               undo, throw new IllegalStateError(subst("Extent is not defined, but Field '&1' in Database Buffer '&2' has extents",SourceName,phDatabaseBuffer:name)).  
                
        AssertSourceField(this-object:Field).
        
        if valid-handle(phDataBuffer:before-buffer) then
            BeforeField = phDataBuffer:before-buffer:buffer-field(Name).
        
        SourceBufferName = phDatabaseBuffer:name.
        SourceDataType = SourceField:data-type.
        
        if SourceExtent > 0 then 
            SourceFullName = subst("&1.&2[&3]",SourceBufferName,SourceName,SourceExtent).
        else 
            SourceFullName = subst("&1.&2",SourceBufferName,SourceName).
        /* This reference will likely not exist, but is to be returned from 
           ColumnSource to be added to the QueryPredicate and QuerySortEntry
           and ColumnExpression and ColumnSort will then use this fieldname 
           to generate/return the final DB expression/predicate      */
           
        ColumnSourceName = subst("&1.&2",SourceBufferName,Name).
        
        Preparebuffer(phDataBuffer).
        PrepareDatabaseBuffer(phDatabaseBuffer).
        Prepared = true.
        return true.
    end method.    
    
    
    // to be deparecated, 
    constructor public AbstractFieldProxy (phField as handle,phDbBuffer as handle, pcSourceName as character   ):
        super ().
        if not valid-handle(phField) then 
            undo, throw new IllegalArgumentError("Field handle is not valid").
        
        this-object:Field = phField.
        Name = this-object:Field:name.
        
        SourceName = pcSourceName.
        
        SourceField = phDbBuffer:buffer-field(SourceName) no-error.
         
        if not valid-handle(SourceField) then 
            undo, throw new IllegalStateError(subst("Field '&1' not found in Database Buffer '&2'.",SourceName,phDbBuffer:name)).  
     
        
        SourceBufferName = phDbBuffer:name.
        SourceDataType = SourceField:data-type.
        SourceFullName = subst("&1.&2",SourceBufferName,SourceName).
        
       /* This reference will likely not exist, but is to be returned from 
          ColumnSource to be added to the QueryPredicate and QuerySortEntry
          and ColumnExpression and ColumnSort will then use this fieldname 
          to generate/return the final DB expression/predicate      */
        ColumnSourceName = subst("&1.&2",SourceBufferName,phfield:name).
        
        if valid-handle(phField:buffer-handle:before-buffer) then
            BeforeField = phField:buffer-handle:before-buffer:buffer-field(Name).
            
        Prepared = true.
    end constructor.
      
    define public abstract property  StringValue    as character no-undo get. 
    method public abstract IQueryEntry ColumnExpression(input pPredicate as IQueryPredicate).
    method public abstract IQuerySortEntry ColumnSort(input pSort as IQuerySortEntry ).
    
    method public abstract logical Save().
    method protected abstract logical Conflict(phField as handle).
    
    method public logical NeedsSave().
         return this-object:field:buffer-handle:row-state = row-created
                or 
                this-object:Field:buffer-value <> this-object:BeforeField:buffer-value.
    end method.
    
    
    method public final logical Conflict():
        if this-object:Field:buffer-handle:row-state = row-created 
        or this-object:Field:buffer-handle:row-state = 0
        then 
            return Conflict(this-object:Field).
        else 
            return Conflict(BeforeField).
    end method.    
    
    method protected logical CheckAndAssertSave():
        if ReadOnly then 
        do:    
            if this-object:field:buffer-value  <> BeforeField:buffer-value then   
                undo, throw new IllegalOperationError(subst("&1 is read-only",BufferUtil:GetFieldLabel(this-object:Field))).
           
            return false.
        end.
       
        return true.
        
    end method.     
    
    method void SetBufferValue(phBuffer as handle):
        define variable hFld as handle no-undo.
        hfld = phBuffer:buffer-field(SourceName).
        case hFLd:data-type:
            // ABL does not find correct overload from buffer-value 
            when "character" then
                SetBufferValue(string(hfld:buffer-value(SourceExtent))).
            when "integer" then
                SetBufferValue(integer(hfld:buffer-value(SourceExtent))).
            when "int64" then
                SetBufferValue(int64(hfld:buffer-value(SourceExtent))).
            when "logical" then
                SetBufferValue(logical(hfld:buffer-value(SourceExtent))).
            when "decimal" then
                SetBufferValue(decimal(hfld:buffer-value(SourceExtent))).
            when "date" then
                SetBufferValue(date(hfld:buffer-value(SourceExtent))).
            when "datetime" then
                SetBufferValue(datetime(hfld:buffer-value(SourceExtent))).
            when "datetime-tz" then
                SetBufferValue(datetime-tz(hfld:buffer-value(SourceExtent))).
        end.
        
    end.  
     
    method public abstract void  SetBufferValue(pValue as character):
    method public abstract void  SetBufferValue(pValue as integer).
    method public abstract void  SetBufferValue(pValue as int64).
    method public abstract void  SetBufferValue(pValue as logical).
    method public abstract void  SetBufferValue(pValue as decimal).
    method public abstract void  SetBufferValue(pValue as date).
    method public abstract void  SetBufferValue(pValue as datetime).
    method public abstract void  SetBufferValue(pValue as datetime-tz).
    
    method public override character ToString() :
        define variable cToString as character no-undo.
        cToString = subst("&1.&2 &3 &4 &5",
                          this-object:field:buffer-handle:name,
                          name,
                          this-object:field:data-type, 
                          SourceFullName,
                          SourceDataType
                        ) no-error. // just oin case called before anything has happened
        if cTOString > " " then                 
            return cTostring.
        else 
            return super:ToString().    
    end.    
    
end class.