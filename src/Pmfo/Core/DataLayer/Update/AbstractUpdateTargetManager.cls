 
 /*------------------------------------------------------------------------
    File        : AbstractUpdateTargetManager
    Purpose     : Common base class for managers that call ValidateRow and/or 
                  SaveRow in multiple IUpdateTargets 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Aug 01 18:13:13 EDT 2019
    Notes       :   
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using Ccs.BusinessLogic.CommitScopeEnum from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
 

class Pmfo.Core.DataLayer.Update.AbstractUpdateTargetManager abstract: 
    define public property UpdateTargets as IHandleDataSourceMap no-undo get. set.
    define public property CommitScope   as CommitScopeEnum      no-undo get. set.  
    define public property Action        as IAction              no-undo get. set.
    // Current target, must be kept in synh by subclasses by calling 
    // AssertAndSynchBuffer whenever the current buffer changes 
    // Since alloperations are top-dwon it is sufficient to call this BEFORE 
    // the buffer is proecessed/managed and it is typically not necessary to 
    // set when retrunging back 
    // to parent in the recursive calls
    define protected property UpdateTarget       as IUpdateTarget no-undo get. set.
    define protected property DatasetAsserted    as logical no-undo get. private set.
    define protected property NoTargetTables     as character no-undo get. set.
    define protected property UpdateRequest      as IUpdateDataRequest no-undo get. set. 
    // Subclasses must implement this to do the actual validateRow and/or saveRow operation 
    // in the current UpdateTarget 
    // The before buffer and after buffer (except for delete) is available when this is called 
    method protected abstract void DoBeforeRow(phBuffer as handle). 
    
    method protected void AssertAndSynchBuffer (phBuffer as handle):
        
        define variable oUpdateTarget as IUpdateTarget no-undo.
        
        if valid-handle(phBuffer:after-buffer) then
            phBuffer = phBuffer:after-buffer.
        
        if not valid-object(UpdateTarget) or UpdateTarget:DataBuffer <> phBuffer then    
        do:
            
            UpdateTarget = cast(UpdateTargets:Get(phBuffer),IUpdateTarget).
            if not valid-object(UpdateTarget) then 
                  undo, throw new IllegalOperationError(subst("Buffer '&1' does not have a corresponding UpdateTarget",
                                                            phBuffer:name,
                                                            phBuffer:dataset:name
                                                            )
                                                      ).
        end.
        
    end method.
    
    method protected logical IsBufferActive(phBuffer as handle):
        return (phbuffer:table-handle:has-records or (valid-handle(phbuffer:table-handle:before-table) and phbuffer:table-handle:before-table:has-records)) 
                and (NoTargetTables = "" or lookup(phbuffer:table-handle:name,NoTargetTables) = 0). 
    end method.    
    
    method protected logical HasChanges (phbuffer as handle).
        define variable i as integer no-undo.
        if valid-handle(phBuffer:before-buffer) and phBuffer:before-buffer:table-handle:has-records then
            return true.
        do i = 1 to phbuffer:num-child-relations:
            if HasChanges(phBuffer:get-child-relation (i):child-buffer) then
               return true.     
        end.    
        return false.             
    end method.
    
    method protected void SubscribeDatasetTransactionEnd(pUpdateTarget as IUpdateTarget, phdataset as handle).
         
    end method.
    
    constructor protected AbstractUpdateTargetManager (pUpdateTargets as IHandleDataSourceMap, pUpdateRequest as IUpdateDataRequest):
        super ().
        Assert:NotNull(pUpdateRequest,"UpdateRequest").
        Assert:NotNull(pUpdateRequest:CommitScope,"CommitScope").
        Assert:NotNull(pUpdateTargets,"UpdateTargets").
        Assert:isTrue(pUpdateTargets:size > 0,"UpdateTargets are started").
        UpdateTargets = pUpdateTargets.
        UpdateRequest = pUpdateRequest.
        CommitScope = pUpdateRequest:CommitScope.
        NoTargetTables = pUpdateRequest:NoTargetTables.
                 
        if type-of(pUpdateRequest:CustomRequest,IAction) then
            Action = cast(pUpdateRequest:CustomRequest,IAction).
        
    end constructor.
    
    method protected void AssertDataset(phDataset as handle):
        define variable oTargets  as IUpdateTarget extent no-undo.
        define variable i as integer no-undo.
        oTargets = cast(UpdateTargets:Sources(),IUpdateTarget).
        // validate that the targets are prepared and matches the dataset
        do i = 1 to extent(oTargets):
             
            if not valid-handle(oTargets[i]:DataBuffer) then
                undo, throw new IllegalOperationError(subst("UpdateTarget '&1' with EnityName '&2' is not prepared. The target's PrepareSave must be called before processing the transaction.",
                                                            oTargets[i]:GetClass():TypeName,
                                                            oTargets[i]:entityname
                                                            )
                                                      ).
               
            if not valid-handle(phDataset:get-buffer-handle(oTargets[i]:DataBuffer:name)) then
                undo, throw new IllegalOperationError(subst("UpdateTarget '&1' with EnityName '&2' and buffer '&3' does not match buffers in dataset &4",
                                                            oTargets[i]:GetClass():TypeName,
                                                            oTargets[i]:entityname,
                                                            oTargets[i]:DataBuffer:name,
                                                            phDataset:name
                                                            )
                                                      ).
                                                      
            if HasChanges(oTargets[i]:DataBuffer) and UpdateRequest:IsActionTarget(oTargets[i]:DataBuffer) = false then                                           
                SubscribeDatasetTransactionEnd(oTargets[i],phdataset).
            
            DatasetAsserted = true.                                                
        end.       
    end method.
    
    method protected void ProcessBeforeRow(phBeforeBuffer as handle):
        define variable hAfter as handle no-undo.
        if phBeforeBuffer:row-state <> row-deleted then
        do:
            hAfter = phBeforeBuffer:after-buffer.
            hAfter:find-by-rowid (phBeforebuffer:after-rowid).
        end.            
         
        DoBeforeRow(phBeforeBuffer).
        
        if phBeforeBuffer:row-state <> row-deleted then
        do:
            // use no-error in case the receord was deleted (could happen for action)
            // an error does not help and the loop continues 
            if hAfter:avail = false or hAfter:rowid <> phBeforebuffer:after-rowid then
                hAfter:find-by-rowid (phBeforebuffer:after-rowid) no-error.
        end.            
    end method.
    
     method protected void InvokeAction(phBuffer as handle):
        define variable oMethod as Method no-undo.
        define variable oParameterList as ParameterList no-undo.
        define variable hDataset  as handle no-undo.
        hDataset = if phBuffer:row-state = 0 or valid-handle(phBuffer:before-buffer) then phBuffer:dataset else phBuffer:after-buffer:dataset. 
        
        oMethod = UpdateTarget:GetActionMethod(Action,hDataset,output oParameterList). 
        
        if valid-object(oMethod) then
        do: 
            if Action:IsCreate = false then
            do: 
                if phBuffer:row-state = 0 then  
                    UpdateTarget:FindSource(exclusive-lock).
                else  
                    UpdateTarget:FindAndCompareSource(phBuffer,input Action:CompareFields).
            end.
            oMethod:Invoke(UpdateTarget,oParameterList).
        end.
        else // at this point a not found error will have been thrown already if the action is not defined in the BE, so we thtoe an application error
            undo, throw new ApplicationError(subst("Action '&1' was not found in data source for '&2'. Please Contact IT",Action:name,UpdateTarget:EntityName)).
    end method.    
    
    // check if data is current if specfied by UpdateRequest 
    method protected void ProcessUnmodifiedRow(phBuffer as handle):
        
        if lookup(phBuffer:table-handle:name,UpdateRequest:CheckReadOnlyTables) > 0 then 
        do:
            UpdateTarget:FindAndCompareSource(phBuffer).
        end.
        else 
            UpdateTarget:FindSourceForUnmodifiedRow().  
            
    end method.
     
            
end class.