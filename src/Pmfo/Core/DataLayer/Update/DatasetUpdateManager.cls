 
 /*------------------------------------------------------------------------
    File        : DatasetWalker
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 30 20:45:59 EDT 2019
    Notes       :  2/12/2020 - SES - Memory leak fix
----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.CommitScopeEnum from propath.
using OpenEdge.Core.Collections.IMap from propath.
using Pmfo.Core.DataLayer.Update.AbstractUpdateTargetManager from propath.
using Pmfo.Core.DataLayer.Update.IDatasetUpdateManager from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
 

class Pmfo.Core.DataLayer.Update.DatasetUpdateManager abstract inherits AbstractUpdateTargetManager implements IDatasetUpdateManager: 
     
    /* Set to true to only delete the current processed table (in ProcessTable)
       - This must be set to true for CommitScopeEnum:Table to work 
         Note that this option prevents cascading deletes to be done with 
         dataset buffers, since the parent is pocessed before the child. 
       False (default) do all deletions top-down before any other
       operations */
    define public property ShallowDelete as logical no-undo get. protected set.
    
    constructor protected DatasetUpdateManager (pUpdateTargets as IHandleDataSourceMap, pUpdateRequest as IUpdateDataRequest ):
        super (pUpdateTargets,pUpdateRequest).
        // scope is asserted as valid in super 
        if CommitScopeEnum:Nested:Equals(pUpdateRequest:CommitScope) then
            undo, throw new IllegalArgumentError(subst("CommitScope '&1' is not supported by this manager",pUpdateRequest:CommitScope:ToString())).
        // for table we must limit deletes to the processed table     
        ShallowDelete = CommitScopeEnum:Table:Equals(pUpdateRequest:CommitScope).
    end constructor.
     
    method public void ProcessDataset(phDataset as handle):
        define variable iBuffer as integer no-undo.
        define variable iNumTopBuffers as integer no-undo.
        define variable hBuffer as handle no-undo.
        
        AssertDataset(phDataset).       
        do iBuffer = 1 to phDataset:num-top-buffers:
            hBuffer = phDataset:get-top-buffer(iBuffer).
            if ShallowDelete = false 
            and IsBufferActive(hBuffer)  then
                ProcessDeletedRows(hBuffer,true).
            ProcessTable(hBuffer).
        end.
    end method.     
    
    
    
    method protected void ProcessTable(phBuffer as handle):
        if IsBufferActive(phBuffer) then
        do:
            if ShallowDelete then
                ProcessDeletedRows(phBuffer,false).
            
            AssertAndSynchBuffer(phbuffer).
            ProcessRows(phbuffer,0). // will call ProcessUnmodifiedRow 
            ProcessRows(phbuffer,row-modified).
            ProcessRows(phbuffer,row-created).
            ProcessChildren(phBuffer).
        end.   
    end method.
    
    method protected void ProcessRows(phBuffer as handle, piState as integer):
        define variable hBefore as handle no-undo.
        define variable hQuery  as handle no-undo.
        define variable lok as logical no-undo. 
        create query hQuery.
        hQuery:add-buffer(phBuffer).
        hBefore = phBuffer:before-buffer.
        // Use preselect since otherwise references to the buffer may mess up the query and cause an endless loop
        // known case - AfterRow assign of sequenceBuffer handle .. InventoryDS - PrintWeighTag > FillCurrent = probably bug  
        // Later: it is also a general precaution for cases where the primary key may change
        hQuery:query-prepare(subst("preselect each &1 where row-state(&1) = &2",phbuffer:name,piState)).
        hQuery:query-open().
        hQuery:get-first.
        do while phBuffer:avail:
            if phbuffer:error = false and phbuffer:rejected = false then
            do:
                if phBuffer:row-state <> 0 then
                do:
                    hBefore:find-by-rowid(phBuffer:before-rowid). 
                    ProcessBeforeRow(hBefore).
                end.
                else  
                    ProcessUnmodifiedRow(phBuffer).
            end.
            lok = hQuery:get-next.
        end.   
        finally:
            delete object hQuery no-error.
        end finally. 
    end method.
    
    method protected void ProcessDeletedRows(phBuffer as handle, plDeep as logical):
        define variable hQuery  as handle no-undo.
        define variable hBefore as handle no-undo.
        
        if IsBufferActive(phBuffer) then
        do:
            if plDeep then
                ProcessDeletedChildren(phBuffer).        
            
            AssertAndSynchBuffer(phBuffer).
            
            hBefore = phBuffer:before-buffer.
            create query hQuery.
            hQuery:add-buffer(hBefore).        
            hQuery:query-prepare(subst("for each &1 where row-state(&1) = row-deleted",hBefore:name)).
            hQuery:query-open().
            hQuery:get-first.
            do while hBefore:avail:
                if hBefore:error = false and hBefore:rejected = false then
                do:
                    ProcessBeforeRow(hBefore).
                end.
                hQuery:get-next. 
            end.    
        end.
        finally:
            if valid-object(hQuery) then
                delete object hQuery.
        end finally. 
        
    end method.
    
    method protected void ProcessDeletedChildren(phBuffer as handle):
        define variable hRelation as handle no-undo.
        define variable i as integer no-undo.
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phbuffer:get-child-relation(i).
            if hRelation:recursive = false then
                ProcessDeletedRows(hRelation:Child-buffer, true).
        end.     
    end method.
    
    method protected void ProcessChildren(phBuffer as handle):
        define variable hRelation as handle no-undo.
        define variable i as integer no-undo.
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phbuffer:get-child-relation(i).
            if hRelation:recursive = false then
               ProcessTable(hRelation:Child-buffer).
        end.     
    end method.
      
end class.