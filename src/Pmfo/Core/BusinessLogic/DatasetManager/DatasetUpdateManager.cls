 
 /*------------------------------------------------------------------------
    File        : DatasetManager
    Purpose     : Manage Dataset Update request to ensure BeforeUpdateData is 
                  called before the update and ReceiveData is called after the 
                  update in other Business Entities when applicable    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 30 20:45:59 EDT 2019
    Notes       :  
----------------------------------------------------------------------*/
block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.DatasetManager.AbstractDatasetManager from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetUpdateManager from propath.
using Pmfo.Core.BusinessLogic.EntityRequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IRequest from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataEventArgs from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.UpdateDataEventArgs from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.ValidationError from propath.
using Pmfo.Core.Manager.ServiceManager from propath.
using Pmfo.Core.Common.IWarningMessageTableHolder from propath.
using Pmfo.Core.Common.IWarningMessages from propath.

class Pmfo.Core.BusinessLogic.DatasetManager.DatasetUpdateManager inherits AbstractDatasetManager implements IDatasetUpdateManager: 
    
    constructor public DatasetUpdateManager ():
        super ().
    end constructor.
    
    method protected override logical DoSubscribe(phBuffer as handle, pRequest as IRequest).
        return true.  // return true - receive may create data for client after an update also - old logic: phBuffer:table-handle:has-records. 
    end method.
     
    method protected override logical ProcessTopTable(phBuffer as handle, pRequest as IRequest, pBusinessEntity as IBusinessEntity):
        if type-of(pRequest,IUpdateDataRequest) then
            return ProcessTable(phBuffer,new UpdateDataEventArgs(pBusinessEntity,cast(pRequest,IUpdateDataRequest),phBuffer),pBusinessEntity).
        else 
            undo, throw new IllegalArgumentError(subst("Request of type &1",pRequest:GetClass():TypeName)).         
    end method.
    
    // returns true if the table has a receive data hook so that we can subscribe when done if necessary
    method protected logical ProcessTable(phBuffer as handle,poUpdateArgs as IUpdateDataEventArgs, pBusinessEntity as IBusinessEntity):
        define variable oEntity           as IBusinessEntity no-undo.
        define variable lChildSubscribe   as logical no-undo.
        define variable lHasClientUpdate  as logical no-undo.
        define variable lSubscribeReceive as logical no-undo.
        define variable lTop              as logical no-undo.
        if valid-object(pBusinessEntity) and phBuffer:table-handle:serialize-name = pBusinessEntity:Resource then
        do:
            oEntity = pBusinessEntity.
           // CreateEntity(phBuffer, pBusinessEntity). 
            lTop = true.
        end.    
        else do: // returns ? if the entity does not need before update processing/validation
            // Parent BEs may have called GetBusinessEntityForDataset for a child from ValidateTable  
            // so if it is already there return it    
            oEntity = GetStartedBusinessEntity(phbuffer).
            if not valid-object(oEntity) then
            do:
                oEntity = ServiceManager:CreateBusinessEntityForRequest(phBuffer:table-handle:serialize-name, 
                                                                       if poUpdateArgs:Request:ActionName > "" then EntityRequestTypeEnum:Action else EntityRequestTypeEnum:Update,
                                                                       output lHasClientUpdate,
                                                                       output lSubscribeReceive).
                if valid-object(oEntity) then
                do:
                    
                    // keep track for receive hook subsctibeic
                    // - note that we need these also if the object itself does not SubscribeReceive
                    // since the event is publisfed through the dataset tree.
                    // Note that GetBusinessEntity will create ttEntity if it was not created here  
                    // it calls CreateBusinessEntity and may get return that does not subscribe
                    // it also creates ttEntoty if no BE exists for buffer   
                    CreateEntity(phBuffer, oEntity).  
                end.
            end.
            else do:
                lHasClientUpdate = ServiceManager:BusinessEntityHasRequestType(
                     phBuffer:table-handle:serialize-name, 
                     if poUpdateArgs:Request:ActionName > "" 
                     then EntityRequestTypeEnum:Action 
                     else EntityRequestTypeEnum:Update
                 ).
            end.   
        end.
        if valid-object(oEntity) and (lTop or lHasClientUpdate) then
        do:
               
            if lTop then //   
                poUpdateArgs:PublicName = phBuffer:serialize-name.
                
            oEntity:BeforeUpdateData(poUpdateArgs, phBuffer).
            // checkif errors have been added (by calls to BufferError) 
            // the validation happens in btable by-reference where the dataset is not present
            if phBuffer:error then 
            do:
                phbuffer:dataset:error = true. 
                undo, throw new ValidationError().
            end.    
        end.        
        lChildSubscribe = ProcessChildren(oEntity,phBuffer,poUpdateArgs).
        return lChildSubscribe or lSubscribeReceive. 
    end method.
    
    method protected logical ProcessChildren(pParent as IBusinessEntity, phBuffer as handle,poUpdateArgs as IUpdateDataEventArgs):
        define variable hRelation as handle no-undo.
        define variable i as integer no-undo.
        define variable lSubscribed as logical no-undo.
         
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phbuffer:get-child-relation(i).
            if hRelation:recursive  = false then
            do:
                poUpdateArgs:Join       = hRelation:relation-fields.
                poUpdateArgs:Parent     = pParent.
                poUpdateArgs:ParentName = phBuffer:table-handle:serialize-name.
                poUpdateArgs:PublicName = hRelation:child-buffer:serialize-name.
                if ProcessTable(hRelation:child-buffer,poUpdateArgs,?) then
                    lSubscribed = true.
            end.    
        end.  
        return lsubscribed.   
    end method.
    
    
       
end class.