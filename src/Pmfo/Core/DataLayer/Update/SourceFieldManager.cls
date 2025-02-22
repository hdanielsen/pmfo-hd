 
 /*------------------------------------------------------------------------
    File        : SourceFieldManager
    Purpose     : Used to map more or less constant values  that is not exposed to the UI at all, 
                  they are typcially also managed as fixed filters in a BaseQuery, but the field 
                  object (ISourceField) is needed for FindSource where clause and to assign vaiues 
                  to the db on create    
                         
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Nov 04 13:40:55 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.DataLayer.IConstantField from propath.
using Pmfo.Core.DataLayer.IFieldManager from propath.
using Pmfo.Core.DataLayer.ISourceField from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.DataLayer.IHiddenSourceKey from propath.

class Pmfo.Core.DataLayer.Update.SourceFieldManager implements IFieldManager: 
    define variable CollectionInitialized     as logical no-undo.
    define protected property BufferName      as character  no-undo get. set.
    define private variable FieldMapList      as character no-undo.
    define private variable HiddenFieldList   as character no-undo.
    define public property HasKey as logical no-undo get. protected set.
    define public property HasMandatoryInitNull as logical no-undo get. protected set.
    
    define private variable DbKeys as character no-undo.
    
    define protected property Fields as ICollection no-undo 
        get():
             if not valid-object(this-object:Fields) then 
                 this-object:Fields = new Collection().
             return this-object:Fields.    
        end.     
        set(pProxy as ICollection):
            this-object:Fields = pProxy.
            CollectionInitialized = true.
        end.
    
    define public property Size as integer  
        get():
            if CollectionInitialized then
                return this-object:Fields:Size.
            else
                return 0.     
        end.
    
    constructor public SourceFieldManager (pcDbKeys as character ):
        super ().
        DbKeys = pcDbKeys.
     
    end constructor.
    
    method public ISourceField Add(pField as ISourceField):
        define variable lOk as logical no-undo.
        
        if Contains(pField) then 
            undo, throw new IllegalOperationError(subst("SourceField is already defined for &1",pField:sourcename)). 
       
        if this-object:Fields:Add(pField) then 
        do:
            // keep track for GetStringValue used in DataSource SourceWhere 
            if type-of(pField,IConstantField) then  
            do:
                FieldMapList = left-trim(subst("&1,&2.&3,&4",
                                     FieldMapList,
                                     pField:SourceBufferName,
                                     pField:SourceName,
                                     cast(pField,IConstantField):QueryExpressionValue // with quotes if applicable
                               ),",").
                if lookup(pField:SourceName,DbKeys) > 0 then 
                    HasKey = true.                                               
            end.                   
            else do: 
                HiddenFieldList = left-trim(subst("&1,&2.&3",
                                        FieldMapList,
                                        pField:SourceBufferName,
                                        pField:SourceName
                                  ),",").
                                  
                if lookup(pField:SourceName,DbKeys) > 0 then 
                do:
                    undo, throw new IllegalOperationError(subst("SourceField &1 of type &2 cannot be defined for a key field. The UpdateTarget GetDataSourceKeys, which currently returns '&3', must be overridden to return a key without this field or the Source Field must be implemented as a IConstantField with a fixed value.",
                                                               pField:Sourcename, 
                                                               pField:GetClass():TypeName,
                                                               DBKEys
                                                               )
                                                               ). 
                end.             
            end.
            return pField.
        end.    
    end method.      
    
    method public logical Contains(pField as ISourceField):
        return this-object:Fields:Contains(pField).
    end method.      
    
    // TODO move out of this interface
    method public character BeforeWhereExpression(phDbBuffer as handle, pcSourcename as character):
        undo, throw new IllegalOperationError("Call to BeforeWhereExpression in SourceFieldManger. Constant and Hidden fields are always the same and does not care about the state of the DataBUffer."). 
    end method.
    
    method public character WhereExpression(phDbBuffer as handle, pcSourcename as character):
        define variable cFullname as character no-undo.
        define variable cExpressionValue as character no-undo.
        cFullname = subst("&1.&2",phDbBuffer:name,pcSourcename). 
        cExpressionValue = StringUtil:GetMappedName(cFullname, FieldMapList).
        if cExpressionValue > "" then
            return subst("&1 = &2",pcSourceName,cExpressionValue).
        
        if lookup(cFullname,HiddenFieldList) > 0 then 
            undo, throw new IllegalStateError(subst("Database Field &1 is defined as hidden and cannot be accessed for queries or display. Make sure there is a GetDataSourceKeys override in the DataSource that returns fields that are in a unique index without this field and that the field is not returned in the DataSource GetFieldMap().",cFullname)). 
        else 
            return "".    
    end method.      
    
/*    method public character GetStringValue(pcDbField as character):                                                                */
/*        if num-entries(pcDbField,".") <> 2 then                                                                                    */
/*             undo, throw new IllegalArgumentError(subst("Database Field &1 is not valid or not qualified with db name",pcDbfield)).*/
/*                                                                                                                                   */
/*        return StringUtil:GetMappedName(pcDbField, FieldMapList).                                                                  */
/*    end method.                                                                                                                    */
    
    // hijack for prepare - do not need optimitic lock check since this is only called on create
    // prepare is an optional step for cases where a new key needs to acces the last record to 
    // get next valie. Doing this when the recor is being creared flushes the buffer and causes
    // error if this key is mandatory or needed to make the record unique
    // TODO: future if proxies that not are constants also needs this step, which is very possible
    //  then we will add new methods for this in the common IFieldManager interface and change this to that
    //  and add notsupported or not implemented heren    
    method public character CheckRow(pUpdateTarget as IUpdateTarget,phBeforeBuffer as handle):
        define variable oIterator   as IIterator    no-undo.
        define variable oField      as ISourceField no-undo.
         
        oIterator = this-object:Fields:Iterator().
        // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
        do while(oIterator:HasNext()):
            oField = cast(oIterator:Next(),ISourceField).
            if type-of(oField,IHiddenSourceKey) then 
                PrepareProxy(cast(oField,IHiddenSourceKey),pUpdateTarget:BufferNames,pUpdateTarget:BufferHandles).   
        end.      
    end method.   
    // no difference here  
    method public character CompareRow(pUpdateTarget as IUpdateTarget,phBeforeBuffer as handle):
        return CheckRow(pUpdateTarget,phBeforeBuffer).
    end method.
    
    method public logical SaveRow(pUpdateTarget as IUpdateTarget,phBeforeBuffer as handle): 
        define variable oIterator   as IIterator   no-undo.
        define variable oField      as ISourceField no-undo. 
        
        if not valid-handle(phBeforeBuffer) then
            undo, throw  new IllegalArgumentError("Before Buffer is null").
        
        if not valid-handle(phBeforeBuffer:after-buffer) then
            undo, throw new IllegalArgumentError("Invalid Buffer Buffer").
        
        if valid-object(this-object:Fields) and this-object:Fields:Size > 0 then 
        do: 
            
            phBeforeBuffer:after-buffer:find-by-rowid (phBeforeBuffer:after-rowid).
           
            // if we have a key field we cannot do a find here so the record must be available
            if phBeforeBuffer:row-state = row-created then 
            do:
                if pUpdateTarget:BufferHandle:avail = false then
                    undo, throw new IllegalStateError(subst("SaveRow called for create with database table &1 unavailable in ProxyManager with Database Key",pUpdateTarget:BufferHandle:name)).
            end.        
            else do: // TODO fix/ensure  DataSource:SaveRow always makes/has this avialable also       
                pUpdateTarget:FindSource(exclusive-lock).
            end.
            oIterator = this-object:Fields:Iterator().
            // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
            do while(oIterator:HasNext()):
                oField = cast(oIterator:Next(),ISourceField).
                SaveProxy(oField, phBeforebuffer,pUpdateTarget:BufferNames,pUpdateTarget:BufferHandles).
            end.    
        end.    
        return true.
    end method.     
    
    method protected void SaveProxy(pFieldProxy as ISourceField, phBeforebuffer as handle, pcBufferNames as character, phbufferHandles as handle extent): 
        define variable hDbBuffer    as handle              no-undo.
        define variable hDbFld       as handle              no-undo.
        hDbBuffer = phBufferHandles[lookup(pFieldProxy:SourceBufferName,pcBufferNames)].
        
        hDbFld = hDbbuffer:buffer-field(pFieldProxy:SourceName).
        if phBeforebuffer:row-state = row-created then 
        do: 
            
            pFieldProxy:Assign(hDbfld).
        end. 
        else  
            undo, throw new UnsupportedOperationError(subst("Save Constant Field for &1 row",BufferUtil:GetRowStateStringPast(phBeforebuffer:row-state))).      
    end method.
    
    method protected void PrepareProxy(pFieldProxy as IHiddenSourceKey, pcBufferNames as character, phbufferHandles as handle extent): 
        define variable hDbBuffer    as handle              no-undo.
        define variable hDbFld       as handle              no-undo.
        
        hDbBuffer = phBufferHandles[lookup(pFieldProxy:SourceBufferName,pcBufferNames)].
        hDbFld = hDbbuffer:buffer-field(pFieldProxy:SourceName).
        pFieldProxy:Prepare(hDbfld).
    end method.
  
      
  
end class.