 
 /*------------------------------------------------------------------------
    File        : ValidationManager
    Purpose     : Manage Pre and Post transaction events  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jul 31 20:26:13 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.DataLayer.Update.DatasetUpdateManager from propath.
using Pmfo.Core.DataLayer.Update.IValidationManager from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.DataLayer.ValidationHookEventType from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.

class Pmfo.Core.DataLayer.Update.ValidationManager inherits DatasetUpdateManager implements IValidationManager: 
    define private variable TopLevel as character no-undo.
    
    define protected property PublicLevelErrorTemplate as char no-undo   
        init  "The ValidationManager is invoked at &1 level, but was already invoked at &2 level. It should only be invoked at one of ProcessDataset, ProcessTable or ProcessBeforeRow"   
        get. 
           
    define public property EventType as ValidationHookEventType no-undo 
        get. 
        set(pType as ValidationHookEventType):
            if not (ValidationHookEventType:Pre:Equals(pType) 
                    or
                    ValidationHookEventType:Post:Equals(pType)
                    ) then
               undo, throw new UnsupportedOperationError(subst("ValidationHookEventType '&1'. The ValidationManager only supports 'Pre' and 'Post' event hooks.",pType:ToString())).
            EventType = pType.       
        end.
    
    constructor public ValidationManager ( pUpdateTargets as IHandleDataSourceMap, pUpdateRequest as IUpdateDataRequest):
        super (pUpdateTargets,pUpdateRequest).
    end constructor.
    
    // Override to skip buffers that are action targets
    method override protected logical IsBufferActive( input phBuffer as handle ):
        define variable lActive as logical no-undo.
        lActive = super:IsBufferActive(input phBuffer).
        
        if lActive and ValidationHookEventType:Post:equals(EventType) = false and  valid-object(Action) then
            lActive = UpdateRequest:IsActionTarget(phBuffer) = false. 
        
         
        return lActive.        
    end method.

    method public void ProcessDataset(phDataset as handle, pType as ValidationHookEventType):
        EventType = pType.
        if TopLevel <> "" and TopLevel <> "dataset" then 
            undo, throw new IllegalOperationError(subst(PublicLevelErrorTemplate,"dataset",TopLevel)).   
        
        TopLevel = "dataset".  
        super:ProcessDataset(phDataset).
    end method.     
    
    method public void ProcessTable( input phBuffer as handle, pType as ValidationHookEventType ):
        EventType = pType. 
        if TopLevel <> "" and TopLevel <> "Table" then 
            undo, throw new IllegalOperationError(subst(PublicLevelErrorTemplate,"Table",TopLevel)).   
        TopLevel = "table". 
        if ShallowDelete = false then
            ProcessDeletedRows(phBuffer, true).
        if not DatasetAsserted then
            AssertDataset(phBuffer:dataset).
        EventType = pType.
        super:ProcessTable(input phBuffer).
    end method.
    
    method protected override void ProcessTable(phBuffer as handle):
        if IsBufferActive(phBuffer) then
        do:
            super:ProcessTable(phBuffer).
        end.
        else do:

            // validate child data if the super isactive  is true and we returned false due to an action of this buffer
            if super:isBufferActive(phBuffer) and valid-object(Action) and UpdateRequest:IsActionTarget(phBuffer) then
            do:
                ProcessChildren(phBuffer).
            end.
        end.
    end method.
    
    method public void ProcessBeforeRow( input phBuffer as handle, pType as ValidationHookEventType ):
        EventType = pType. 
        if TopLevel <> "" and TopLevel <> "row" then 
            undo, throw new IllegalOperationError(subst(PublicLevelErrorTemplate,"dataset",TopLevel)).   
        TopLevel = "row".
        if not DatasetAsserted then
            AssertDataset(phBuffer:dataset).
        super:ProcessBeforeRow(input phBuffer).
    end method.
    
    method protected override void DoBeforeRow(phBeforeBuffer as handle):
        if UpdateRequest:IsActionTarget(phBeforeBuffer:after-buffer) then
            UpdateTarget:ValidateRowAction(phBeforeBuffer:after-buffer,Action, EventType).
        else 
            UpdateTarget:ValidateRow(phBeforeBuffer, EventType).
    end method.
    
    // override to do nothing
    method override protected void ProcessUnmodifiedRow( input phBuffer as handle ):
    end method.
    
end class.