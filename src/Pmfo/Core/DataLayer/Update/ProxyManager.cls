 
 /*------------------------------------------------------------------------
    File        : ProxyManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Nov 04 13:40:55 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.DataLayer.IFieldManager from propath.
using Pmfo.Core.DataLayer.IFieldProxy from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
 
class Pmfo.Core.DataLayer.Update.ProxyManager implements IFieldManager: 
    define variable CollectionInitialized as logical no-undo.
    define public property HasKey as logical no-undo get. protected set.
    define public property HasMandatoryInitNull as logical no-undo get. private set.
    
    define private variable DbKeys as character no-undo.
    define protected property BufferName      as character  no-undo get. set.
    
    define protected property ProxyFields     as ICollection no-undo 
        get():
             if not valid-object(ProxyFields) then 
                 Proxyfields = new Collection().
             return ProxyFields.    
        end.     
        set(pProxy as ICollection):
            ProxyFields = pProxy.
            CollectionInitialized = true.
        end.
    
    define public property Size as integer  
        get():
            if CollectionInitialized then
                return ProxyFields:Size.
            else
                return 0.     
        end.
         
    constructor public ProxyManager (pbufferName as character, pcDbKeys as character ):
        super ().
        BufferName = pBuffername.
        DbKeys = pcDbKeys.
    end constructor.
    
    method public logical Add(pProxy as IFieldProxy):
        define variable lOk as logical no-undo.
        if lookup(pProxy:SourceName,DBkeys) > 0 then 
            HasKey = true.
        return ProxyFields:Add(pProxy).
    end method.
    
    method public IFieldProxy Get(pcFullName as character):
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo.
        define variable cTablename as character no-undo.
        define variable cFieldname as character no-undo. 
        if num-entries(pcfullName,".") <> 2 then
            undo, throw new IllegalArgumentError(subst("&1 is not a qualified temp table field name",pcfullname)).
        cTablename = entry(1,pcFullName,".").
        cFieldname = entry(2,pcFullName,".").
        oIterator = ProxyFields:Iterator().
         // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
        do while(oIterator:HasNext()):
            ofieldProxy = cast(oIterator:Next(),IfieldProxy).
            if oFieldProxy:Field:name = cFieldname
            and oFieldProxy:Field:buffer-handle:name = cTablename  then
                return ofieldProxy.  
        end.
         
    end method.
    
    method public void Prepare(phDatabuffer as handle, phDatabaseTable as handle):
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo. 
        if ProxyFields:Size > 0 then 
        do:
            oIterator = ProxyFields:Iterator().
            // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
            do while(oIterator:HasNext()):
                ofieldProxy = cast(oIterator:Next(),IfieldProxy).
                
                if ofieldProxy:Prepared = false then
                do: 
                    ofieldProxy:Prepare(phDatabuffer,phDatabaseTable).
                end.
                // needed for save to use SaveUtil to avoid errors omn save-row-changes 
                if ofieldProxy:ismandatory and ofieldProxy:sourcefield:default-string = ? then   
                    HasMandatoryInitNull = true.     
            end.  
              
        end.    
    end method.          
    
    // build where expression for a source key. there may be many fields for the same key
    // Progress databases are not famous for good normalization and key may be overloaded 
    // to conactinate stuff for unqieness acrosss otehr fields or stuff...
    method public character WhereExpression(phDbBuffer as handle, pcSourcename as character):
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo. 
        define variable cWhere      as character no-undo.
        define variable cAnd         as character no-undo.
        define variable cExpression as character no-undo.
        if ProxyFields:Size > 0 then 
        do:
            oIterator = ProxyFields:Iterator().
            // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
            do while(oIterator:HasNext()):
                ofieldProxy = cast(oIterator:Next(),IfieldProxy).
                if  oFieldProxy:SourceBufferName = phDbBuffer:name 
                and oFieldProxy:SourceName       = pcSourcename then 
                do: 
                    assign
                        cExpression = oFieldProxy:ColumnExpression(new QueryPredicate(pcSourcename,QueryOperatorEnum:Eq,oFieldProxy:field:buffer-value)):ToString(). 
                        cWhere = cWhere 
                               + (if cWhere = "" then "" else " and ")
                               + cExpression. 
                    
                end.     
            end.    
        end.    
        
        return cWhere. 
    end method.  
    
    method public IQueryEntry ColumnExpression(pPredicate as IQueryPredicate):
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo. 
        
        if ProxyFields:Size > 0 then 
        do:
            oIterator = ProxyFields:Iterator().
            // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
            do while(oIterator:HasNext()):
                ofieldProxy = cast(oIterator:Next(),IfieldProxy).
                if pPredicate:FieldName = oFieldProxy:ColumnSourceName then
                do: 
                    return oFieldProxy:ColumnExpression(pPredicate).
                end.     
            end.    
        end.    
        
        return ?. 
    end method.  
    
    method public character ColumnSource (pcColumn as char):
        
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo. 
        
        /*  inner join reference to other buffers can be passed from client,
            so we use the buffer's dataset to get those datasources' 
            fieldmap. This assume that any buffer that has been attached to a 
            datasource is referencing the same physical storage, (keep in mind that all connected 
            progress databases are the same physical storage in this context), so all dataset mapping 
            is valid for this datasource. 
            Note that this secondary inner join is passed from the client as an option to reduce the 
            number of records to fill for THIS table. 
            The actual fill of the joined table is handled by another datasource.  */
        
        if ProxyFields:Size > 0 then 
        do:
            oIterator = ProxyFields:Iterator().
            // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
            do while(oIterator:HasNext()):
                ofieldProxy = cast(oIterator:Next(),IfieldProxy).
                if oFieldProxy:Field:Name = pcColumn then 
                    return oFieldProxy:ColumnSourceName.  
            end.    
        end.
        return ?.    
    end method.  
    
    method public IQuerySortEntry extent ColumnSort(pSort as IQuerySortEntry):
        define variable oSort as IQuerySortEntry extent no-undo.
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo. 
        
        if ProxyFields:Size > 0 then 
        do:
            oIterator = ProxyFields:Iterator().
            // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
            do while(oIterator:HasNext()):
                ofieldProxy = cast(oIterator:Next(),IfieldProxy).
                if pSort:FieldName = oFieldProxy:ColumnSourceName then
                do: 
                    extent(oSort) = 1.
                    osort[1] = oFieldProxy:ColumnSort(pSort).
                    return osort.
                end.     
            end.    
        end.    
 
        return oSort. // default to empty    
    end method.   
    
    // called from DataSource AfterRow    
    method public void AfterRow(input pcBufferNames as character, input phBufferHandles as handle extent):
        define variable oIterator   as IIterator   no-undo.
        define variable oFieldProxy as IFieldProxy no-undo.
        
        if ProxyFields:Size > 0 then 
        do:
            oIterator = ProxyFields:Iterator().
            do while(oIterator:HasNext()):
                oFieldProxy = cast(oIterator:Next(),IFieldProxy).
                ofieldProxy:SetBufferValue(phBufferHandles[lookup(oFieldProxy:SourceBufferName,pcBufferNames)]).  
            end.    
        end.    
    end method. 
    
    method public character CheckRow(pUpdateTarget as IUpdateTarget, phBuffer as handle): 
        define variable oIterator    as IIterator   no-undo.
        define variable oFieldProxy  as IFieldProxy no-undo. 
        define variable cChanges     as character   no-undo.
        define variable lChanged     as logical     no-undo.
 
        if not valid-handle(phBuffer) then
            undo, throw  new IllegalArgumentError("Buffer is null").
        
        if valid-object(ProxyFields) and ProxyFields:Size > 0 then 
        do:  
            pUpdateTarget:FindSource(phBuffer,pUpdateTarget:BufferHandle,exclusive-lock).
            oIterator = ProxyFields:Iterator().
            // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
            do while(oIterator:HasNext()):
                ofieldProxy = cast(oIterator:Next(),IfieldProxy).
                lChanged = oFieldProxy:Conflict().
                if lChanged then
                   cChanges = cChanges  
                            + (if cchanges = "" then "" else ",")
                            + ofieldProxy:Field:name. 
            end.    
        end.
        
        return cChanges.    
    end method.      
    
    method public logical SaveRow(pUpdateTarget as IUpdateTarget,phBeforeBuffer as handle): 
        define variable oIterator    as IIterator   no-undo.
        define variable oFieldProxy  as IFieldProxy no-undo. 
        define variable hAfterBuffer as handle      no-undo.
        
        if valid-object(ProxyFields) and ProxyFields:Size > 0 then 
        do: 
            if not valid-handle(phBeforeBuffer) then
                undo, throw  new IllegalArgumentError("Before Buffer is null").
            
            if not valid-handle(phBeforeBuffer:after-buffer) then
                undo, throw new IllegalArgumentError("Invalid Buffer Buffer").
            
            hAfterBuffer = phBeforebuffer:after-buffer.
            
            if hAfterBuffer:avail = false or hAfterBuffer:rowid <> phBeforebuffer:after-rowid then
                hAfterBuffer:find-by-rowid (phBeforebuffer:after-rowid). 
            
            // if we have a key field we cannot do a find here so the record must be available
            // TODO it seems better to be consistent - the DataSource should be fixed to always 
            // have record availsble also when using save-row-chnages - this is constent and morew efficient 
            // and better for hooks - but need some testing to ensure we can do a safe find (potential scoping issues with not all keys assigned?)     
            if phBeforeBuffer:row-state = row-created then
            do:  
                if pUpdateTarget:BufferHandle:avail = false then   
                    undo, throw new IllegalStateError(subst("SaveRow called for create with database table &1 unavailable in ProxyManager",pUpdateTarget:BufferHandle:name)).
            end.
            else // TODO fix/ensure  DataSource:SaveRow always makes/has this avialable also       
                pUpdateTarget:FindSource(exclusive-lock).
            oIterator = ProxyFields:Iterator().
            // the list is typcially small so the assumption is that a collection loop is cheaper than a map  
            do while(oIterator:HasNext()):
                ofieldProxy = cast(oIterator:Next(),IfieldProxy).
                if phBeforebuffer:row-state = row-created or oFieldProxy:Field:buffer-value <> oFieldProxy:BeforeField:buffer-value then 
                    oFieldProxy:Save().  
            end.    
        end.    
        return true.
    end method.     
    
   
    
end class.