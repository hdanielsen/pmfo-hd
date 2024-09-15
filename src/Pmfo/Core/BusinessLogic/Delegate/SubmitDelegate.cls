
/*------------------------------------------------------------------------
    File        : SubmitDelegate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 03 06:26:58 EST 2024
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetUpdateManager from propath.
using Pmfo.Core.BusinessLogic.Delegate.ISubmitDelegate from propath.
using Pmfo.Core.BusinessLogic.Delegate.ISubmitDelegator from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IClientEntity from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.Common.IWarningMessageTableHolder from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.JsonDatasetErrorProcessor from propath.
using Pmfo.Util.DatasetUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.

class Pmfo.Core.BusinessLogic.Delegate.SubmitDelegate implements ISubmitDelegate:  
     
    method public Object SubmitData(pDelegator as ISubmitDelegator,pManager as IDatasetUpdateManager,  pDataAccess as IDataAccess, input poUpdateDataRequest as IUpdateDataRequest, input-output dataset-handle phDataset):
        define variable oResponse   as Object no-undo.
        define variable hWarning   as handle no-undo.
        define variable hRelation  as handle no-undo.
        define variable iRel       as integer no-undo.
        define variable iBuffer    as integer no-undo.
        define variable hBuffer    as handle no-undo.
        define variable cNavTables as character no-undo.
        define variable oBe        as IBusinessEntity no-undo.
        define variable hInfrastructureDataset as handle no-undo.
        
        // Use of BufferError outside managed hooks may end here if not managed  
        // (this is considered part of the framework, as it allows BufferError to be used also outside of 
        // the validate hooks
        if phDataset:error then
            ThrowDatasetError(phDataset).
        
        // do not start DS for inactive relations ir clientonly children
        // TODO - check recursive (difficult when non active) ?  
        do iRel = 1 to phDataset:num-relations:
            hRelation = phDataset:get-relation(IRel).
            if hRelation:active = false then 
                poUpdateDataRequest:AddNoTargetTable(hRelation:child-buffer:table-handle:name).
            else do:
                // et existing - we do not want to start the manager if this is not called from UpdateData (wil cause a leak)
              //  oDatasetUpdateManager = GetExistingDatasetUpdateManager(phDataset).
                if valid-object(pManager) then 
                    // GetBusinessEntity supresses not found from Service if the be is 
                    // not in its cache and does not exist
                    oBe = pManager:GetBusinessEntity(hRelation:child-buffer).
                else do on error undo, throw:
                    oBe = cast(Application:ServiceManager,IServiceManager):CreateBusinessEntity(hRelation:child-buffer:table-handle:serialize-name).
                    catch e as NotFoundError :
                      // this is not considered an error in this case 
                      // there may be temp tables that does not have a BE     
                    end catch.
                end.      
                if valid-object(oBe) and oBe:ClientOnly then 
                    poUpdateDataRequest:AddNoTargetTable(hRelation:child-buffer:table-handle:name).
            end.         
        end.
         
        if valid-object(poUpdateDataRequest:Action) = false then 
        do:  
            if poUpdateDataRequest:MainRequestedBuffer > "" then 
            do:
                hBuffer = phDataset:get-buffer-handle (poUpdateDataRequest:MainRequestedBuffer).
                // get nav tables (URL parent references with no data in the body ) 
                // as we do not want to check id current on server
                // (may not have all data)
                // don't throw error (for now?) if not valid  
                if valid-handle(hBuffer) then 
                     // collects parent names above MainRequestedbuffer 
                    cNavTables = DatasetUtil:GetParentTableNames(hBuffer). 
            end.
            // add read-only tables to update request list of tables to check are current. 
            // If fhey are part of the submit they are very likely important and need to be current  
            do iBuffer = 1 to phDataset:num-buffers:
                hBuffer = phDataset:get-buffer-handle(iBuffer).
                if valid-handle(hBuffer:before-buffer) 
                and hBuffer:table-handle:has-records
                and hBuffer:before-buffer:table-handle:has-records = false 
                and lookup(hBuffer:table-handle:name,cNavTables) = 0 then 
                    poUpdateDataRequest:AddCheckReadOnlyTable(hBuffer:table-handle:name).
            end.
        end. 
        
        if valid-object(pDelegator:WarningMessages) then
            hWarning = cast(pDelegator:WarningMessages,IWarningMessageTableHolder):TableHandle.
         
                                                                                
        // infrastructure data are data that are not managed as regular crud, but that
        // may track and hold infrastructure information for all regular crud   
        if pDelegator:IsInfrastructureServiceStarted then 
            hInfrastructureDataset = pDelegator:InfrastructureService:dataset. 
        
          // allow ClientOnly to handle save after all data is validated by DatasetUpdateManager
        if type-of(pDelegator,IClientEntity) then 
            oResponse = cast(pDelegator,IClientEntity):SubmitClientData(poUpdateDataRequest, input-output dataset-handle phDataset by-reference, input dataset-handle hInfrastructureDataset,output table-handle hWarning append ).        
        else
            oResponse = pDataAccess:SubmitData(poUpdateDataRequest, input-output dataset-handle phDataset, input dataset-handle hInfrastructureDataset, output table-handle hWarning append).        
        
        return oResponse.
        
        finally:
            if pDelegator:IsInfrastructureServiceStarted then 
            do:
                delete object pDelegator:InfrastructureService.
                pDelegator:IsInfrastructureServiceStarted = false.
            end.        
       end finally.
    end method.
    
    method public void ThrowDatasetError( phDataset as handle): 
        define variable oJson as JsonObject no-undo.
        define variable oErrorProcessor as JsonDatasetErrorProcessor no-undo.
        define variable oError as Error no-undo.
        define variable iRelation as integer no-undo.
        
        oJson = new JsonObject().
        do iRelation = 1 to phDataset:num-relations:
            // ensure keys are in json so they can be added to error 
            // as of current the dataset is not returne for errors 
            // we may return datasets for 409 in futrure , but the assumption is that it causes no isses since it will always be correct 
            // TODO  use parent relation  
            phDataset:get-relation (iRelation):foreign-key-hidden = false.
        end.       
        phDataset:write-json ("JsonObject",oJson,no,?,no,yes, yes).
        oErrorProcessor = new JsonDatasetErrorProcessor().
            
        if DatasetUtil:HasBeforeState(phDataset,row-deleted) then 
            oError = oErrorProcessor:CreateDeleteError (ojson, phdataset).
        else 
            oError = oErrorProcessor:CreateError (ojson, phdataset).
        
        if valid-object(oError) then
        do:
            undo, throw oError .
        end.
        else do:
            // this should no longer happen since JsonDatasetErrorProcessor now should manage all cases that ended up here before  
            oError = new ApplicationError("Update of data failed, but no error information was returned to the client. Please Contact IT.").
            undo, throw oError. 
        end.     
        
    end method.    
    
         
end class.