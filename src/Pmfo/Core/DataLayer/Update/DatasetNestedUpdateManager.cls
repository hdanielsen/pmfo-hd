 
 /*------------------------------------------------------------------------
    File        : DatasetUpdateRowWalker
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 30 20:45:59 EDT 2019
    Notes       :  
----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.DataLayer.Update.AbstractUpdateTargetManager from propath.
using Pmfo.Core.DataLayer.Update.IDatasetUpdateManager from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.

class Pmfo.Core.DataLayer.Update.DatasetNestedUpdateManager abstract inherits AbstractUpdateTargetManager implements IDatasetUpdateManager : 
    
     /* Always False for nested update operations */
    define public property ShallowDelete as logical no-undo get. 
    
    constructor protected DatasetNestedUpdateManager (pUpdateTargets as IHandleDataSourceMap,pUpdateRequest as IUpdateDataRequest):
        super (pUpdateTargets,pUpdateRequest).
    end constructor.
     
    method public void ProcessDataset(phDataset as handle):
        define variable iBuffer as integer no-undo.
        define variable iNumTopBuffers as integer no-undo.
        define variable hBuffer as handle no-undo.
        
        AssertDataset(phDataset).
               
        do iBuffer = 1 to phDataset:num-top-buffers:
            hBuffer = phDataset:get-top-buffer(iBuffer).
            ProcessTable(hBuffer).
        end.
    end method.     
    
    method protected void ProcessTable(phBuffer as handle):
        if IsBufferActive(phBuffer) then
        do:
            if valid-handle(phBuffer:parent-relation) = false then 
                ProcessDeletedRows(phBuffer).
            
            ProcessRows(phbuffer,0).
            ProcessRows(phbuffer,row-modified).
            ProcessRows(phbuffer,row-created).
        end.
    end method.
    
    method protected void ProcessRows(phBuffer as handle, piState as integer):
        
        define variable hQuery  as handle no-undo.
        define variable cJoin as character no-undo.
        
        if valid-handle(phBuffer:parent-relation) then 
            cJoin = JoinExpression(phBuffer:parent-relation,piState) + " ".
        
        create query hQuery.
        hQuery:add-buffer(phBuffer).
        
        if cJoin > "" then cJoin = cJoin + " and ".
        
        // preselect - see comment in DatasetUpdateManager ProcessRows  
        hQuery:query-prepare(subst("preselect each &1 where&3row-state(&1) = &2",phbuffer:name,piState," " + cJoin)).
        hQuery:query-open().
        hQuery:get-first.
        do while phBuffer:avail:
            if phBuffer:rejected = false and phBuffer:error = false then
                ProcessBuffer(phBuffer).
            hQuery:get-next. 
        end. 
        finally:
           if valid-handle(hQuery) then
                delete object hQuery.
        end.    
    end method.
    
    method protected void ProcessBuffer(phBuffer as handle):
        define variable hBefore as handle no-undo.
/* DEBUG  */
/*        define variable cKeys as character no-undo.*/
/*        define variable iKey as integer no-undo.               */
/*        define variable cValues as character extent no-undo.   */
/*        define variable cBefValues as character extent no-undo.*/
/*        define variable hFld as handle no-undo.*/
        if IsBufferActive(phBuffer) then
        do:
            ProcessDeletedChildren(phbuffer).
            
            AssertAndSynchBuffer(phBuffer).
            
            if phBuffer:row-state <> 0 then
            do:
    /* DEBUG */
    /*             cKeys = BufferUtil:GetKeys(phBuffer).   */
    /*             extent(cValues) = num-entries(Ckeys).   */
    /*             extent(cBefValues) = num-entries(Ckeys).*/
    /*                                                     */
                 hBefore = phBuffer:before-buffer.
                 hBefore:find-by-rowid(phBuffer:before-rowid). 
    /* keep for DEBUG */
    /*             do ikey = 1 to num-entries(Ckeys):                 */
    /*                hFld = phBuffer:buffer-field(entry(ikey,cKeys)).*/
    /*                cValues[iKey] = hFld:buffer-value.              */
    /*                hFld = hBEfore:buffer-field(entry(ikey,cKeys)). */
    /*                cBefValues[iKey] = hFld:buffer-value.           */
    /*             end.                                               */
                 ProcessBeforeRow(hBefore).
            end.
            else 
                ProcessUnmodifiedRow(phBuffer).
                
            ProcessChildren(phBuffer).
        end.     
    end method.
    
    method protected void ProcessDeletedBuffer(phBeforeBuffer as handle):
        // ProcessDeletedChildren checks if the pbuffer is deleted before buffer - 
        // JoinExpression will use the deleted before buffer  
        ProcessDeletedChildren(phBeforeBuffer).
        
        ProcessBeforeRow(phBeforeBuffer).
    end method.
    
    // dleted parent and child 
    method protected character JoinDeletedExpression(phParent as handle, phRelation as handle):
        define variable cChildName as character no-undo.
        define variable i          as integer no-undo. 
        define variable cValue     as character no-undo.
        define variable cExpression as character no-undo.
        
        if not phParent:avail then
        do:
            undo, throw new IllegalStateError(subst("Deleted parent '&1' not available in JoinExpression",phParent:name)).
        end.
        
        if not valid-handle(phRelation:child-buffer:before-buffer) then
             undo, throw new IllegalStateError(subst("Child '&1' not updateable in JoinExpression for delete",phRelation:child-buffer:name)).
        
        cChildName = phRelation:child-buffer:before-buffer:name.
                
        do i = 1 to num-entries(phRelation:relation-fields) by 2:
            cValue = phParent:buffer-field(entry(i,phRelation:relation-fields)):buffer-value.
            cExpression = cExpression 
                   + (if cExpression = "" then "" else " and ")
                   + subst("&1.&2 = '&3'",cChildName,entry(i + 1,phRelation:relation-fields),cValue). 
                   .
        end. 
        return cExpression.
    
    end method.    
    
    method protected character JoinExpression(phRelation as handle, piState as integer):
        define variable hChildBuffer  as handle    no-undo.
        
        if pistate = row-deleted then
        do:     
            hChildBuffer = phRelation:child-buffer.
            if not valid-handle(hChildBuffer:before-buffer) then
               undo, throw new IllegalStateError(subst("Child '&1' not updateable in JoinExpression for delete",hChildBuffer:name)).
        end.
        return BufferUtil:GetJoinExpression(phRelation,pistate = row-deleted).
    end method.    
    
    method protected void ProcessDeletedChildRows(phParent as handle, phBuffer as handle):
        define variable hQuery  as handle no-undo.
        define variable hBefore as handle no-undo.
        define variable cJoin as character no-undo.
        if IsBufferActive(phBuffer) then
        do:
            if phParent:row-state = row-deleted then 
                cJoin = JoinDeletedExpression(phParent,phBuffer:parent-relation) + " ".
            else 
                cJoin = JoinExpression(phBuffer:parent-relation,row-deleted) + " ".
                 
            AssertAndSynchBuffer(phBuffer).
            
            hBefore = phBuffer:before-buffer.
             
            create query hQuery.
            hQuery:add-buffer(hBefore).     
            
            if cJoin > "" then cJoin = cJoin + " and ".
            
            hQuery:query-prepare(subst("for each &1 where&2row-state(&1) = row-deleted",hBefore:name," " + cJoin)).
            hQuery:query-open().
            hQuery:get-first.
            do while hBefore:avail:
                if hBefore:rejected = false and hBefore:error = false then
                    ProcessDeletedBuffer(hBefore).
                hQuery:get-next. 
            end.
        end.    
        finally:
           if valid-handle(hQuery) then
                delete object hQuery.
        end.    
        
    end method.
    
    method protected void ProcessDeletedRows(phBuffer as handle):
        define variable hQuery  as handle no-undo.
        define variable hBefore as handle no-undo.
        define variable cJoin as character no-undo.
        if IsBufferActive(phBuffer) then
        do:
            AssertAndSynchBuffer(phBuffer).
            
            hBefore = phBuffer:before-buffer.
            create query hQuery.
            hQuery:add-buffer(hBefore).        
            hQuery:query-prepare(subst("for each &1 where row-state(&1) = row-deleted",hBefore:name)).
            hQuery:query-open().
            hQuery:get-first.
            do while hBefore:avail:
                if hBefore:rejected = false and hBefore:error = false then
                    ProcessDeletedBuffer(hBefore).
                hQuery:get-next. 
            end.
        end. 
        finally:
           if valid-handle(hQuery) then
                delete object hQuery.
        end.   
    end method.
    
    method protected void ProcessDeletedChildren(phBuffer as handle):
        
        define variable hRelation      as handle no-undo.
        define variable i              as integer no-undo.
        define variable hChildbuffer   as handle no-undo.
        define variable lParentDeleted as logical no-undo.
        define variable hParent        as handle no-undo.
        
        if phBuffer:row-state = row-deleted then
        do:
            lParentDeleted = true.
            hParent = phBuffer.
            // before buffer does not have children
            phBuffer = phbuffer:after-buffer.
        end.    
        else 
            hParent = phBuffer.
            
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phbuffer:get-child-relation(i).
            hChildBuffer = hRelation:Child-buffer.
            if valid-handle(hChildbuffer:before-buffer) then
            do:              
                ProcessDeletedChildRows(hParent,hRelation:Child-buffer).
            end.
        end.     
    end method.
    
    method protected void ProcessChildren(phBuffer as handle):
        define variable hRelation as handle no-undo.
        define variable i as integer no-undo.
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phbuffer:get-child-relation(i).
            ProcessTable(hRelation:Child-buffer).
        end.     
    end method.
      
end class.