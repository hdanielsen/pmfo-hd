 
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

using Ccs.BusinessLogic.CommitScopeEnum from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.DataLayer.Update.DatasetUpdateManager from propath.
using Pmfo.Core.DataLayer.Update.ITransactionManager from propath.
using Pmfo.Core.DataLayer.Update.ValidationManager from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.DataLayer.ValidationHookEventType from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.TransactionError from propath.
using Pmfo.Core.Error.ValidationError from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Util.DatasetUtil from propath.

class Pmfo.Core.DataLayer.Update.TransactionManager inherits DatasetUpdateManager implements ITransactionManager: 
    /* Event published before update starts. 
       DataSources may subscribe to this for example to change row-state  
    */
    define public event DatasetTransactionEnd signature void (input dataset-handle phDataset). 
    define public event EndTableTransaction signature void (input dataset-handle phDataset). 
    define public event ProcessInfrastructureData signature void (input phInfrastructureDataset as handle, input phApplicationDataset as handle). 
    
    define protected variable TransactionNum as integer no-undo.
    
    define variable hAlertDataset as handle no-undo.
    define protected property ValidationManager as ValidationManager no-undo 
        get(). 
            if not valid-object(ValidationManager) then
            do:
                ValidationManager = new ValidationManager(UpdateTargets,UpdateRequest).
            end.
            return ValidationManager.  
        end.      
        set.
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public TransactionManager (pUpdateTargets as IHandleDataSourceMap,pUpdateRequest as IUpdateDataRequest):
        super(pUpdateTargets,pUpdateRequest).
    end constructor.
    
    // called from AbstractUpdateTargetManager
    // @TODO some of this is duplicated in NestedTransactionManager (except the subscription)
    // maybe move this to AbstractUpdateTargetManager 
    // - but this would likely require empty overrideson ActionTransactionManager and ValidationTransactionManager
    method protected override void SubscribeDatasetTransactionEnd(pUpdateTarget as IUpdateTarget, phdataset as handle).
        define variable oMethod as Method no-undo.
        define variable oParamList as ParameterList  no-undo.
        define variable hHandle    as handle no-undo.
        oParamList = new ParameterList(1).
        oParamList:SetParameter(1,"dataset-handle","input",hhandle).    
        oMethod = pUpdateTarget:GetClass():GetMethod("DatasetTransactionEnd",Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance or Progress.Reflect.Flags:DeclaredOnly,oParamList).
        if valid-object(oMethod) then 
        do:
            if CommitScopeEnum:All:Equals(CommitScope) then
                DatasetTransactionEnd:Subscribe(pUpdateTarget:DatasetTransactionEnd).
            else 
                undo, throw new IllegalOperationError(subst("Cannot use CommitScope &1 with &2 that has a 'DatasetTransactionEnd' override",
                                                            CommitScope:ToString(),
                                                            pUpdateTarget:GetClass():TypeName))
                                                            .    
        end.     
    end method.
    
    method public override void ProcessDataset(phDataset as handle):
        ProcessDataset(phDataset,?). 
    end method.     
    
    method public void ProcessDataset(phDataset as handle, phInfrastructureDataset as handle):
        if CommitScopeEnum:All:Equals(CommitScope) then
        do: 
            ValidationManager:ProcessDataset(phDataset,ValidationHookEventType:Pre).
            if phDataset:error then
                undo, throw new ValidationError().
                 
            do transaction:
                
                TransactionNum = TransactionNum + 1.
                super:ProcessDataset(phDataset).
                // TODO can the two events be unified?   
                if not phdataset:error and valid-handle(phInfrastructureDataset) then    
                    ProcessInfrastructureData:publish(input phInfrastructureDataset, input phDataset).     
                if phdataset:error then 
                    undo, throw new TransactionError().   
                DatasetTransactionEnd:publish(input dataset-handle phDataset by-reference).
                     
            end.
            
            ValidationManager:ProcessDataset(phDataset,ValidationHookEventType:Post).
        end.
        else do:
            // this should already have been checked in constructor
            if valid-handle(phInfrastructureDataset) then 
            do: 
                if DatasetUtil:HasRecords(phInfrastructureDataset) then
                    undo, throw new IllegalOperationError(subst("Cannot submit Infrastructure data with CommitScope &1",CommitScope:ToString())).
            end.
            super:ProcessDataset(phDataset).
        end.  
    end method. 
    
    method override protected void ProcessTable( input phBuffer as handle ):
        if IsBufferActive(phBuffer) then
        do:
            if CommitScopeEnum:Table:Equals(CommitScope) then
            do:
                ValidationManager:ProcessTable(phBuffer,ValidationHookEventType:Pre).
                if not phBuffer:error then
                do transaction:
                    TransactionNum = TransactionNum + 1.
                    super:ProcessTable(input phBuffer).
                end.
                ValidationManager:ProcessTable(phBuffer,ValidationHookEventType:Post).
            end.
            else 
                super:ProcessTable(input phBuffer).
        end. 
    end method.
    
    method override protected void ProcessBeforeRow( input phBuffer as handle ):
        if CommitScopeEnum:Row:Equals(CommitScope) then
        do:
            ValidationManager:ProcessBeforeRow(phBuffer,ValidationHookEventType:Pre).
            if not phBuffer:error then
            do transaction:
                TransactionNum = TransactionNum + 1.
                super:ProcessBeforeRow(input phBuffer).
            end.
            if not phBuffer:error then
                ValidationManager:ProcessBeforeRow(phBuffer,ValidationHookEventType:Post).
        end.
        else 
            super:ProcessBeforeRow(input phBuffer).
         
    end method.
    
    method protected override void DoBeforeRow (phBeforeBuffer as handle):
        if UpdateRequest:IsActionTarget(phBeforeBuffer) then
        do:
            InvokeAction(phBeforeBuffer).
            if not phBeforeBuffer:error then
                UpdateTarget:ValidateRowAction(phBeforeBuffer, cast(UpdateRequest:CustomRequest,IAction),ValidationHookEventType:End).
        end.    
        else do:   
            UpdateTarget:ValidateRow(phBeforeBuffer, ValidationHookEventType:Begin). 
            if not phBeforeBuffer:error then
            do:
                UpdateTarget:SaveRow(phBeforeBuffer). 
                if not phBeforeBuffer:error then
                    UpdateTarget:ValidateRow(phBeforeBuffer, ValidationHookEventType:End). 
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