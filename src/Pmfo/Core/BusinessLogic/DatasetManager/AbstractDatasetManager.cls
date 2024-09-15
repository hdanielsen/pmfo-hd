 
 /*------------------------------------------------------------------------
    File        : DatasetManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 30 20:45:59 EDT 2019
    Notes       :  
----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IRequest from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetManager from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.Common.IWarningMessages from propath.
using Pmfo.Core.Common.IWarningMessageTableHolder from propath.
 
class Pmfo.Core.BusinessLogic.DatasetManager.AbstractDatasetManager abstract implements IDatasetManager  : 
    
    define temp-table ttEntity no-undo
        field bufferHandle as handle
        field Entity as Progress.Lang.Object
        index hdl as primary unique bufferHandle.
    
    define protected property ServiceManager as IServiceManager no-undo 
         get(): 
              return cast(Application:ServiceManager,IServiceManager).
         end.     
         set.
    define variable TopEntity as character no-undo. // foir toString
    define protected variable ToStringInfo as character no-undo.
     
    constructor public AbstractDatasetManager ( ):
        super ( ).
        define variable i as integer no-undo init 5.
        
        toStringInfo = "Stack:".
        do while program-name(i) <> ?:
            ToStringInfo = StringUtil:AppendEntry(ToStringInfo,entry(1,program-name(i),"")," ").
            i = i + 1.
            if i > 7 then 
                leave.
                 
        end.    
    end constructor.
    
    // the abstract methods have different behavior for get and update
    method protected abstract logical ProcessTopTable(phBuffer as handle,  pRequest as IRequest,pBusinessEntity as IBusinessEntity).
    method protected abstract logical DoSubscribe(phBuffer as handle, pRequest as IRequest).
      
    method public void ProcessDataset(pBusinessEntity as IBusinessEntity, phDataset as handle, pRequest as IRequest):
        define variable iTopTables  as integer no-undo.
        define variable hBuffer     as handle  no-undo.
        define variable lMultiTable as logical no-undo.
        define variable i           as integer no-undo.
        define variable cFunction   as character no-undo.
        define variable cAction     as character no-undo.
        
        ToStringInfo = subst("&1 &2","Request: ",pRequest:ToString()).
        
        if type-of(pRequest,IGetDataRequest) then 
        do:
            if pRequest:MainRequestedBuffer > "" then 
            do: 
                ToStringInfo = subst("Read &1",pRequest:ToString()).
                cfunction = cast(cast(pRequest,IGetDataRequest):GetTableRequest( pRequest:MainRequestedBuffer),IGetDataTableRequest):NamedQuery:name no-error.
                if cFunction > "" then 
                    ToStringInfo = subst("&1 Function: &2",ToStringInfo,cFunction).
            end.
        end.
        else if type-of(pRequest,IUpdateDataRequest) then
        do: 
            ToStringInfo = subst("Update &1",pRequest:ToString()).
            cAction = cast(pRequest,IUpdateDataRequest):ActionName.
            if cAction > "" then 
                ToStringInfo = subst("&1 Action: &2",ToStringInfo,cFunction).
                        
        end.
        if valid-object(pRequest:RequestType) then 
            lMultiTable = pRequest:RequestType = RequestTypeEnum:MultiTable.
         
        Assert:NotNull(phDataset,"handle").
        Assert:IsTrue(phDataset:type = 'dataset',subst("Handle type is '&1'",phDataset:type)).
        
        TopEntity = pBusinessEntity:Resource. 
        
        if lMultiTable then // process all top tables except repos
            iTopTables = DatasetUtil:GetNumTopBuffersExcludeRepos(phDataset).
        else // do not process top tables that are deactivated relations
            iTopTables = DatasetUtil:GetNumTopBuffers(phDataset).
         
        do i = 1 to iTopTables:
            if lMultiTable then // get from all top tables except repos
                hBuffer = DatasetUtil:GetTopBufferExcludeRepos(phDataset,i).
            else // do not process top tables that are deactivated relations
                hBuffer = DatasetUtil:GetTopBuffer(phDataset,i).
                
            // if any of the entities needs subscription then aubscribe from top 
            if ProcessTopTable(hBuffer, pRequest,pBusinessEntity) then
            do:
                if iTopTables = 1 and hBuffer:table-handle:serialize-name = pBusinessentity:Resource then
                    SubscribeReceiveToChildren(pBusinessentity,pRequest, hBuffer).
                else    
                    SubscribeReceiveTable(pBusinessEntity,pRequest,hbuffer).
            end. 
        end.     
        
        catch e as Progress.Lang.Error :
            ToStringInfo = Subst("&1 Error: &2",ToStringInfo,e:GetMessage(1)).
            for each ttEntity:
                if valid-object(ttEntity.entity) then
                   cast(ttEntity.entity,IBusinessEntity):UnsubscribeDataReceived().
            end.    
            undo, throw e.
        end catch.
    end method.     
    
    method public IBusinessEntity GetStartedBusinessEntity(phbuffer as handle).
        
        find ttEntity where ttEntity.bufferHandle = phbuffer no-error.
         
        if avail ttEntity then 
            return cast(ttEntity.entity,IBusinessEntity).
        
        return ?. 
    end method. 
      
    method public IBusinessEntity GetBusinessEntity(phbuffer as handle):
        define variable oEntity   as IBusinessEntity no-undo.
        find ttEntity where ttEntity.bufferHandle = phbuffer no-error.
       
        if avail ttEntity then
            return cast(ttEntity.entity,IBusinessEntity).
              
        // NO-ERROR for not found - allow buffers with no entity  
        // uae table-handle serialize-name - the buffer can be different (for example Relatiom aliasname) 
        do on error undo, throw:
            oEntity = ServiceManager:CreateBusinessEntity(phbuffer:table-handle:serialize-name).
            catch e as NotFoundError :
            end catch.
        end.
        return CreateEntity(phBuffer, oEntity).
    end method.    
    
    method protected IBusinessEntity CreateEntity(phBuffer as handle, oEntity   as IBusinessEntity) :
        create ttEntity.
        assign 
            ttEntity.bufferHandle = phbuffer
            ttEntity.entity = oEntity.     
        return oEntity.
    end method. 
    
    method protected void SubscribeReceiveTable(pParent as IBusinessEntity, pRequest as IRequest, phBuffer as handle ):
        define variable oEntity   as IBusinessEntity no-undo.
        define variable cQualname as character no-undo.
               
        if DoSubscribe(phBuffer, pRequest) then
        do:
            oEntity = GetBusinessEntity(phBuffer).
            if valid-object(oEntity) then
            do:
                // the datasdet may have a buffer with differtent serialize-name than the table and resource
                // on order to have many of instances fo the same resource in a dataset
                // (define tt like schema woth same serialize-name for data access request and define buffer with public name)
                // so pass serialize-name so the BE can identitfy the buffer in the dataset when receiving the event
                cQualname = BufferUtil:GetQualifiedBufferName(phBuffer).
                oEntity:SubscribeDataReceived(pParent,cQualname).
                SubscribeReceiveToChildren(oEntity,pRequest,phBuffer).
            end.
        end.
    end.    
    
    method protected void SubscribeReceiveToChildren(pEntity as IBusinessEntity, pRequest as IRequest, phBuffer as handle ):
        define variable i         as integer no-undo.
        define variable hRelation as handle no-undo.
         
        do i = 1 to phbuffer:num-child-relations:
            hRelation = phBuffer:get-child-relation (i).
            if not hRelation:reposition then
            do: 
                SubscribeReceiveTable(pEntity,pRequest,hRelation:child-buffer).
            end.    
        end.
    end.    
    
    method protected logical CallGetDataHookInChildren(phBuffer as handle ):
        define variable i         as integer no-undo.
        define variable hRelation as handle  no-undo.
        do i = 1 to phbuffer:num-child-relations:
            hRelation = phBuffer:get-child-relation (i).
            if not hRelation:reposition then 
            do: 
                if CallGetDataHook(hRelation:child-buffer) then
                    return true.
            end.    
        end.
    end.    
    
    method protected logical CallGetDataHook(phBuffer as handle ):
         define variable lServiceExists          as logical no-undo.
         define variable lServiceSubscribe       as logical no-undo.
         
     //   find ttEntity where ttEntity.Resource = phBuffer:serialize-name no-error.
        if lServiceExists and lServiceSubscribe then
            return true.
        else
            return CallGetDataHookInChildren(phBuffer).    
    end method. 
    
    method public void GetChildWarnings (output table-handle phWarningTable).
        define variable oWarningMessages as IWarningMessages no-undo.
        for each ttEntity:
            if valid-object(ttEntity.entity) then
            do:
                oWarningMessages = cast(ttEntity.entity,IBusinessEntity):WarningMessages.
                if valid-object(oWarningMessages) and oWarningMessages:HasData then 
                    cast(oWarningMessages,IWarningMessageTableHolder):GetWarningTableData(output table-handle phWarningTable append).
              end.    
        end.
        finally:
            if valid-handle(phWarningTable) then
                delete object phWarningTable.
        end.
    end method.
     
    method public override  character ToString():
        define variable cString as character no-undo.
        
        if ToStringInfo > "" then 
            return ToStringInfo.

        cString = TopEntity . 
        for each ttentity: 
            if valid-object(ttEntity.Entity) then 
            do: 
                cString = StringUtil:AppendEntry(cString,cast(ttEntity.Entity,IEntity):Resource).
            end.  
        end.     
        if cString > "" then
            return cString.
        else 
            return "[empty]".  
    end method.
    
   
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    destructor public AbstractDatasetManager ( ):
        for each ttEntity:
            delete ttEntity.  //  do not delete the objects (they need to?) stay alive until gb 
        end.     
    end destructor.
      
end class.