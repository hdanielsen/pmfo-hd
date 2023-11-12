 
 /*------------------------------------------------------------------------
    File        : TransactionManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jul 31 08:49:13 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.DataLayer.Update.DatasetNestedUpdateManager from propath.
using Pmfo.Core.DataLayer.Update.ITransactionManager from propath.
using Pmfo.Core.DataLayer.Update.NestedValidationManager from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.DataLayer.ValidationHookEventType from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.

class Pmfo.Core.DataLayer.Update.NestedTransactionManager inherits DatasetNestedUpdateManager implements ITransactionManager: 
    define public event DatasetTransactionEnd signature void (input dataset-handle phDataset). 
    define public event EndTableTransaction signature void (input dataset-handle phDataset). 
    define public event ProcessInfrastructureData signature void (input phInfrastructureDataset as handle, input phApplicationDataset as handle). 
    
    define protected variable TransactionNum as integer no-undo.
    
    
    define protected variable InfrastructureDataset as handle no-undo.  
    
    define protected property ValidationManager as NestedValidationManager no-undo 
        get(). 
            if not valid-object(ValidationManager) then
            do:
                ValidationManager = new NestedValidationManager(UpdateTargets,UpdateRequest).
            end.
            return ValidationManager.  
        end.      
        set.
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public NestedTransactionManager (pUpdateTargets as IHandleDataSourceMap,pUpdateRequest as IUpdateDataRequest):
         super(pUpdateTargets,pUpdateRequest).
    end constructor.
    
    // return a message if not unique
    method protected character CheckTopBufferUnique(phDataset as handle):
         define variable hTopBuffer as handle no-undo.
         define variable iNum as integer no-undo.
         define variable cMsg as character no-undo.
         iNum = DatasetUtil:GetNumTopBuffers(phDataset). 
         if iNum = 1 then
         do:
             hTopBuffer = DatasetUtil:GetTopBuffer(phDataset,1). 
             do on error undo, throw:
                 hTopBuffer:find-unique.
                 catch e as Progress.Lang.Error :
                    if e:GetMessageNum(1) = 3166 then 
                         cMsg = "more than one record".
                    else if e:GetMessageNum(1) <> 138 then
                         cMsg = "error " + e:getMessage(1) + " on find". // should not be  possible    
                 end catch.
             end.
         end.
         else 
              cMsg = "more than one top buffer". 
         return cMsg.    
    end method.    
    /** 
     Subscribe to the dataset end transaction event 
     For nested transactions this is only allowed when the top buffer has one record 
     called from AbstractUpdateTargetManager
      @TODO some of this is duplicated in NestedTransactionManager (except the subscription)
      maybe move this to AbstractUpdateTargetManager 
     - but this would likely require empty overrides in ValidationTransactionManager
    **/ 
    method protected override void SubscribeDatasetTransactionEnd(pUpdateTarget as IUpdateTarget, phdataset as handle).
        define variable oMethod as Method no-undo.
        define variable oParamList as ParameterList  no-undo.
        define variable hHandle    as handle no-undo.
        define variable lOk as logical no-undo.
        define variable cErrorMsg as character no-undo.
        oParamList = new ParameterList(1).
        oParamList:SetParameter(1,"dataset-handle","input",hhandle).    
        oMethod = pUpdateTarget:GetClass():GetMethod("DatasetTransactionEnd",Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance or Progress.Reflect.Flags:DeclaredOnly,oParamList).
        if valid-object(oMethod) then 
        do:
             cErrormsg = CheckTopBufferUnique(phDataset).
             if cErrorMsg = ""  then
                 DatasetTransactionEnd:Subscribe(pUpdateTarget:DatasetTransactionEnd).
             else
                 undo, throw new IllegalOperationError(subst("Cannot use CommitScope &1 to update &2 when the update has &3 and the class has a 'DatasetTransactionEnd' override",
                                                            CommitScope:ToString(),
                                                            pUpdateTarget:GetClass():TypeName,
                                                            cErrorMsg
                                                            )
                                                       )
                                                       .
        end.     
    end method.
    
    method public void ProcessDataset(phDataset as handle, phInfrastructureDataset as handle):
         define variable hTopBuffer as handle no-undo.
         define variable iNum as integer no-undo.
         define variable cErrorMessage as character no-undo.
         if valid-handle(phInfrastructureDataset) then
         do: 
             cErrorMessage = CheckTopBufferUnique(phDataset).
             if cErrorMessage > "" then 
                 undo, throw new IllegalOperationError(subst("Cannot use CommitScope &1 with Alert data when the update has &2",
                                                            CommitScope:ToString(),
                                                            cErrorMessage
                                                            )
                                                       ).
             InfrastructureDataset = phInfrastructureDataset. // we will process this in the transaciton
         end.
         ProcessDataset(phDataset). 
      
    end method.     
             
    method protected override void ProcessBuffer(phBuffer as handle):
        define variable hdataset as handle no-undo.
        if not transaction then
        do:
            ValidationManager:ProcessBuffer(phBuffer, ValidationHookEventType:Pre).
            if not phBuffer:error then
            do transaction:
                hdataset = phbuffer:dataset.
                super:ProcessBuffer(phBuffer).
                ProcessBufferAfterSave(phBuffer).
                // TODO can these events be unified?   
                if valid-handle(InfrastructureDataset) then
                    ProcessInfrastructureData:publish(input InfrastructureDataset, input hdataset).
                DatasetTransactionEnd:publish(input dataset-handle hDataset by-reference).  
            end.
            if not phBuffer:error then
                ValidationManager:ProcessBuffer(phBuffer, ValidationHookEventType:Post).
        end.
        else do: 
            super:ProcessBuffer(phBuffer).
            ProcessBufferAfterSave(phBuffer).
        end.
    end method.
    
    method protected void ProcessBufferAfterSave(phBuffer as handle):
        define variable hBefore as handle no-undo.
        AssertAndSynchBuffer(phBuffer).
        if phBuffer:row-state <> 0 and not phBuffer:dataset:error then
        do:
             hBefore = phBuffer:before-buffer.
             hBefore:find-by-rowid(phBuffer:before-rowid). 
             if UpdateRequest:IsActionTarget(hBefore) then 
                 UpdateTarget:ValidateRowAction(hBEfore, cast(UpdateRequest:CustomRequest,IAction), ValidationHookEventType:End).
             else 
                 UpdateTarget:ValidateRow(hBefore, ValidationHookEventType:End).
             
        end.
             
    end method.
    
    method protected override void ProcessDeletedBuffer(phBeforeBuffer as handle ):
        define variable hdataset as handle no-undo.
        if not transaction then
        do:
            ValidationManager:ProcessDeletedBuffer(phBeforeBuffer, ValidationHookEventType:Pre).
            if not phBeforeBuffer:error then
            do transaction:
                hdataset = phBeforeBuffer:dataset.
                super:ProcessDeletedBuffer(phBeforeBuffer).
                DatasetTransactionEnd:publish(input dataset-handle hDataset by-reference).  
            end.
            if not phBeforeBuffer:error then
                ValidationManager:ProcessDeletedBuffer(phBeforeBuffer, ValidationHookEventType:Post).
        end.
        else 
            super:ProcessDeletedBuffer(phBeforeBuffer).
                
    end method.
    
    method protected override void DoBeforeRow (phBeforeBuffer as handle):
        if UpdateRequest:IsActionTarget(phBeforeBuffer) then
            InvokeAction(phBeforeBuffer).
        else do:   
            UpdateTarget:ValidateRow(phBeforeBuffer, ValidationHookEventType:Begin). 
            
            if not phBeforeBuffer:dataset:error then
            do:
                UpdateTarget:SaveRow(phBeforeBuffer). 
            end.    
        end.
    end method.
    
    method override protected void ProcessUnmodifiedRow( input phBuffer as handle ):
        // this is not in use when this was added, but it seems as an action could work with row-state 0
        if UpdateRequest:IsActionTarget(phBuffer) then
            InvokeAction(phBuffer). 
        else 
            super:ProcessUnmodifiedRow(phBuffer).
    end method.
    
end class.