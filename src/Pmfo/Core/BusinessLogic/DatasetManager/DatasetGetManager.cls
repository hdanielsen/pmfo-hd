 
 /*------------------------------------------------------------------------
    File        : DatasetGetManager
    Purpose     : Manage Dataset Get request to ensure ReceiveData is 
                  called in other Business Entities when applicable    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 30 20:45:59 EDT 2019
    Notes       :  
----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.AbstractDatasetManager from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetGetManager from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IClientJoinEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IRequest from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Manager.ServiceManager from propath.

class Pmfo.Core.BusinessLogic.DatasetManager.DatasetGetManager inherits AbstractDatasetManager implements IDatasetGetManager: 
    
    define public property ClientInnerJoinTables as character              no-undo get. protected set.
    define public property ClientSort            as IQuerySortEntry extent no-undo get. protected set.
    
    constructor public DatasetGetManager ():
        super ( ).
    end constructor.
    
    method protected override logical ProcessTopTable(phBuffer as handle, pRequest as IRequest,pBusinessEntity as IBusinessEntity):
        if type-of(pRequest,IGetDataRequest) then
            return ProcessTable(phBuffer,cast(pRequest,IGetDataRequest),false,if pBusinessEntity:Resource = phBuffer:table-handle:serialize-name then pBusinessEntity else ?).
        else 
            undo, throw new IllegalArgumentError(subst("Request of type &1",pRequest:GetClass():TypeName)).  
    end method.    
    
    // don't subscribe if not requested (the buffer may be in the dataset) 
    method protected override logical DoSubscribe(phBuffer as handle, pRequest as IRequest).
        return valid-object(cast(pRequest,IGetDataRequest):GetTableRequest(phBuffer:table-handle)). 
    end method.
    
    method protected logical ProcessTable(phBuffer as handle,pGetDataRequest as IGetDataRequest,pClientOnly as logical,pBusinessEntity as IBusinessEntity ):
        define variable lClientOnly        as logical no-undo.
        define variable lSubscribe         as logical no-undo.
        define variable lChildSubscribe    as logical no-undo.
        define variable oEntity            as IBusinessEntity no-undo.
        define variable lServiceClientOnly as logical no-undo.
        define variable lServiceExists     as logical no-undo.
        define variable lServiceSubscribe  as logical no-undo.
        define variable lPrepareGet        as logical no-undo.
        define variable oTableRequest       as IGetDataTableRequest no-undo.
        define variable lHasClientJoin      as logical no-undo.
        define variable oClientSort         as IQuerySortEntry extent no-undo.
        
        oTableRequest = cast(pGetDataRequest:GetTableRequest(phbuffer:table-handle:name),IGetDataTableRequest).
        if valid-object(oTableRequest) then
        do:
            if valid-object(pBusinessEntity)  then
            do: 
                lClientOnly = pBusinessEntity:ClientOnly.
                oEntity = pBusinessEntity.
                //  call the prepare - use of reflection to see if it is overrideen is likely nore expensive
                lPrepareGet = true.   
            end.
            else do:
                // check table serialize-name. Buffer serialize-name may be defined to support a request for an alias (i.e Relation:AliasName)
                oEntity = ServiceManager:CreateBusinessEntityForGet(phbuffer:table-handle:serialize-name, output lPrepareGet, output lServiceSubscribe, output lServiceExists, output lServiceClientOnly ).
                if lServiceExists then
                do:
                    if lServiceClientOnly then 
                    do:
                        lClientOnly = true.
                    end.
                    else if pClientOnly then
                    do:
                        // allow ClientOnly this to be set from preapreGetRequest
                        if oTableRequest:ClientOnly = false then
                            undo, throw new IllegalStateError(subst("Cannot request '&1' on server when parent is client only",phbuffer:serialize-name)).   
                    end. 
                    lSubscribe = lServiceSubscribe or lClientOnly.
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
                 else if pClientOnly then 
                 do: 
                     lClientOnly = true. 
                 end.     
            end.
            if lClientOnly then 
                oTableRequest:ClientOnly = true.
            
             // Both IClientEntity and IClientFilterEntity inherits this and are also always started on the client for receive (EntityDS )
            if type-of(pBusinessEntity,IClientJoinEntity) then 
            do:
                // this will remove predicates and sort that need to be done on the client 
                cast(pBusinessEntity,IClientJoinEntity):PrepareInnerJoin(pGetDataRequest, phBuffer, output lHasClientJoin, output oClientSort).
                if lHasClientJoin = true then 
                do:
                    ClientInnerJoinTables = ClientInnerJoinTables 
                                          + (if ClientInnerJoinTables = "" then "" else ",")
                                          + phBuffer:table-handle:name.
                     
                end.                
                if extent(oClientSort) > 0 then 
                    ClientSort = oClientSort.     
                
            end. 
                
            PrepareTableRequest(oEntity,phBuffer,pGetDataRequest ).
            
            lChildSubscribe = ProcessChildren(phBuffer,
                                              pGetDataRequest, 
                                              oTableRequest:ClientOnly // pass prop since we may not set lClientonly if already set  
                                              ).
            return lChildSubscribe or lSubscribe. 
        end.
        else 
            return false.        
    end method.     
    
    //  separated out for subclass override
    method protected void PrepareTableRequest(pBusinessEntity as IBusinessEntity,phBuffer as handle,pGetDataRequest as IGetDataRequest ):
        define variable oTableRequest       as IGetDataTableRequest no-undo.
        if valid-object(pBusinessEntity)  then
        do:
            oTableRequest = cast(pGetDataRequest:GetTableRequest(phbuffer:table-handle:name),IGetDataTableRequest).
         
            pBusinessEntity:PrepareGetTableRequest(oTableRequest, phBuffer).
        end.
         
    end method.    
    
    method protected logical ProcessChildren(phBuffer as handle,pGetDataRequest as IGetDataRequest, pClientOnly as logical ):
        define variable i           as integer no-undo.
        define variable hRelation   as handle  no-undo.
        define variable lSubscribe as logical no-undo.
        do i = 1 to phbuffer:num-child-relations:
            hRelation = phBuffer:get-child-relation (i).
            if hRelation:reposition = false and hRelation:recursive = false then 
            do: 
                if ProcessTable(hRelation:child-buffer,pGetDataRequest,pClientOnly,?) then
                    lSubscribe = true.
            end.    
        end.
        return lSubscribe. 
    end method.     

end class.