 
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
using Pmfo.Core.DataLayer.Update.DatasetNestedUpdateManager from propath.
using Pmfo.Core.DataLayer.Update.IValidationManager from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.DataLayer.ValidationHookEventType from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.

class Pmfo.Core.DataLayer.Update.NestedValidationManager inherits DatasetNestedUpdateManager implements IValidationManager: 
    define private variable TopLevel as character no-undo.
    
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
    
    constructor public NestedValidationManager ( pUpdateTargets as IHandleDataSourceMap,pUpdateRequest as IUpdateDataRequest):
        super (pUpdateTargets,pUpdateRequest).
    end constructor.
    
    // Override to skip buffers that are action targets
    method override protected logical IsBufferActive( input phBuffer as handle ):
        define variable lActive as logical no-undo.
        lActive = super:IsBufferActive(input phBuffer).
        if lActive and valid-object(Action) then
            return UpdateRequest:IsActionTarget(phBuffer) = false. 
        else 
            return lActive.    
    end method.
    
    method protected override void DoBeforeRow(phBeforeBuffer as handle):
        UpdateTarget:ValidateRow(phBeforeBuffer, EventType).
    end method.
   
    method public void ProcessBuffer(phBuffer as handle, pEventType as ValidationHookEventType):
        EventType = pEventType.
        super:ProcessBuffer(phBuffer).
    end method.
    
    method public void ProcessDeletedBuffer(phAfterBuffer as handle, pEventType as ValidationHookEventType):
        EventType = pEventType.
        super:ProcessDeletedBuffer(phAfterBuffer).
    end method.
    
    // override to do nothing
    method override protected void ProcessUnmodifiedRow( input phBuffer as handle ):
    end method.
 
    
end class.