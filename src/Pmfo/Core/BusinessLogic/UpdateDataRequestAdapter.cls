 /*------------------------------------------------------------------------
    File        : UpdateDataRequestAdapter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Sep 27 18:44:32 EDT 2019
    Notes       : Adapts a Ccs...UpdateDataRequest to Pmfo 
                  This mainly exists to ensure that we can call hooks
                  with Pmfo.Core.BusinessLogic.IUpdateDataRequest parameters 
                  in the odd case that the request is a Ccs request 
                  It can be argued that it should not happen, but to avoid
                  having to deal with some kind of error handling or getting
                  cast errors at runtime we just use this. 
                - It is also a good example of what an adapter is....     
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.CommitScopeEnum from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.DatasetUtil from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using Pmfo.Core.Common.IGetDataMessages from propath.
using Pmfo.Core.Common.GetDataMessages from propath.


class Pmfo.Core.BusinessLogic.UpdateDataRequestAdapter final implements IUpdateDataRequest: 
    define protected property UpdateDataRequest as Ccs.BusinessLogic.IUpdateDataRequest no-undo get. set.
   
    
    define public property NoTargetTables      as character no-undo get.   
    define public property CheckReadOnlyTables as character no-undo get. protected set. 
    
     define public property Messages as IGetDataMessages  no-undo 
        get():
            // todo - should  this return a place holder that prevents adding messages? 
            // very academic this adapter is probalby never used 
            if not valid-object(Messages) then 
                Messages = new GetDataMessages().
            return Messages.    
        end.    
        protected set.
    
    define public property NoRefresh           as logical no-undo get. set.
    
    define public property Action             as IAction no-undo  
        get():
             if valid-object(UpdateDataRequest:CustomRequest) and type-of(UpdateDataRequest:CustomRequest,IAction) then
                 return cast(UpdateDataRequest:CustomRequest,IAction).
             else
                 return ?.    
        end.  
      
    define public property ActionName          as character no-undo  
        get():
             if valid-object(Action) then
                 return Action:name.
             else
                 return "".    
        end.     
    
     define public property ServerType          as ServerTypeEnum no-undo 
        get():
            return ?.
        end. 
        set(pType as ServerTypeEnum ):
            undo, throw new UnsupportedOperationError("Ser ServerType in Adapter for Ccs").  
        end. 
     
          
    define public property CommitScope as CommitScopeEnum no-undo 
    get():
        return UpdateDataRequest:CommitScope.
    end.
    
    define public property Confirmations as character no-undo get.   
    
    define public property CustomRequest as Object no-undo 
    get():
        return UpdateDataRequest:CustomRequest.
    end.
    protected set.
    
    // set from constructor 
    define public property MainRequestedBuffer as character no-undo 
    get.
    set.      
     
    // set from constructor 
    define public property RequestType as RequestTypeEnum no-undo 
    get.
    set. 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public UpdateDataRequestAdapter (pUpdateDataRequest as Ccs.BusinessLogic.IUpdateDataRequest, phDataset as handle):
        super ().
        UpdateDataRequest = pUpdateDataRequest.
        
        if DatasetUtil:GetNumTopBuffers(phDataset) = 1 then
        do:
            MainRequestedBuffer = DatasetUtil:GetTopBuffer(phDataset, 1):name.
            RequestType = RequestTypeEnum:SingleTopTable.
        end.
        else do:
            RequestType = RequestTypeEnum:MultiTable.
        end.
        
    end constructor.
    
    // add a table name to the no target table list
    method public logical AddNoTargetTable (pTablename as character):
        undo, throw new UnsupportedOperationError("AddNoTargetTable in Adapter for Ccs").  
    end method.
      
    // remove a table name from the no target table list
    method public logical RemoveNoTargetTable (pTablename as character):
        undo, throw new UnsupportedOperationError("RemoveNoTargetTable in Adapter for Ccs").  
    end method.   
    
     // add a table name to the CheckReadOnlyTables list
    method public logical AddCheckReadOnlyTable (pTablename as character).  
        undo, throw new UnsupportedOperationError("AddCheckReadOnlyTable in Adapter for Ccs").  
    end method.
      
    // remove a table name from the CheckReadOnlyTables list
    method public logical RemoveCheckReadOnlyTable (pTablename as character).  
        undo, throw new UnsupportedOperationError("RemoveNoTargetTable in Adapter for Ccs").  
    end method.  
    
    //  The Adapter only supports return of false when there is no CustomRequest
    method public logical IsActionTarget(phHandle as handle):
        if valid-object(CustomRequest) and type-of(CustomRequest,IAction) then
            undo, throw new UnsupportedOperationError("IsActionTarget in Adapter with valid CustomRequest for Ccs").
        return false.      
    end method.    
    
    method public void RemoveAction(): 
        if valid-object(CustomRequest) and type-of(CustomRequest,IAction) then
            undo, throw new UnsupportedOperationError("RemoveAction in Adapter with valid CustomRequest for Ccs").
        
         undo, throw new IllegalOperationError("RemoveAction called with no valid action").
        
    end method.
        
    method public void ConvertActionToNamedQuery().
        if valid-object(CustomRequest) and type-of(CustomRequest,IAction) then
            undo, throw new UnsupportedOperationError("ConvertActionToNamedQuery in Adapter with valid CustomRequest for Ccs").
        
         undo, throw new IllegalOperationError("ConvertActionToNamedQuery called with no valid action").
    end method.
    
    method public void ConvertActionToNamedQuery(pcName as character).
        if valid-object(CustomRequest) and type-of(CustomRequest,IAction) then
            undo, throw new UnsupportedOperationError("ConvertActionToNamedQuery in Adapter with valid CustomRequest for Ccs").
        
         undo, throw new IllegalOperationError("ConvertActionToNamedQuery called with no valid action").
    end method.
    
    method public Progress.Reflect.Method GetClientMethod(pObject as Object, phDataset as handle,output pClientParameterList as ParameterList):
        // Ccs does not use this - may be possible to extract from TableRequests 
        return ?.     
    end method.
    
    method public character GetConfirmedQuestions  ( pcSourceEntityname as character):
       return "". // Confirmations does not have public setter so this cannot return any value  
    end.     
    
    method void RemoveClientMethod():
    end method. 
end class.