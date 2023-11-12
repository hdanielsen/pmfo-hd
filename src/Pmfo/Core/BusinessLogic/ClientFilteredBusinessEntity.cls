/*------------------------------------------------------------------------------
 File        : Pmfo.Core.BusinessLogic.ClientFilteredBusinessEntity
 Purpose     : 
 Syntax      : 
 Description :  
 Author(s)   :  
 Created     : 02/23/2021  
 Notes       :   
------------------------------------------------------------------------------*/

block-level on error undo, throw.

using Ccs.BusinessLogic.INamedQuery from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Pmfo.Core.BusinessLogic.BusinessEntity from propath.
using Pmfo.Core.BusinessLogic.ClientFilterDataSource from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IClientFilteredEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryRemovePredicateTableQualifier from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.

class Pmfo.Core.BusinessLogic.ClientFilteredBusinessEntity abstract inherits BusinessEntity implements IClientFilteredEntity:
    
    define protected property ClientQueryEntries  as IQueryEntry extent no-undo get. set.
    define protected property ClientSort          as IQuerySortEntry extent no-undo get. set.
    define protected property ClientQueryContext  as Ccs.BusinessLogic.IQueryEntry  no-undo get. set.
    define protected property ClientNumRecords    as int64  no-undo get. set.
    define protected property ClientCount         as logical  no-undo get. set.
    define protected property ClientSkip          as integer  no-undo get. set.
    define protected property ClientRelation      as handle   no-undo get. set.
    define protected property NoParentJoin           as logical no-undo get. set. 
    define protected property InnerJoinQueryEntries  as IQueryEntry extent no-undo get. set.
   
    // must be set from subclass constructor if needed 
    define protected property NamedQueryOnly      as logical  no-undo get. set.
    
    define protected property ServerNumResults    as int64 init ?  no-undo get. set.
    
    constructor ClientFilteredBusinessEntity(phDataset as handle):
        super(phDataset).
    end constructor.
    
    constructor ClientFilteredBusinessEntity(phDatasets as handle extent):
        super(phDatasets).
    end constructor.
    
    method public    abstract int64 ReceiveSourceRecords(input table-handle phTable).
    method protected abstract character extent GetClientFields().
    
    // allow subclasses to override 
    method protected logical KeepServerFilter():
        return false.
    end.
    
    // allow subclasses to return true of the client filter fields also should be
    // filtered on client for a join 
    // This is really the most effcient, but may cause isses when mapping parent join ClientDataManager
    // this cannot be default (the dataset link will ensure that the extra rerieved records are not shown)
    // TODO: fix this to work per default. 
    //  NOte there is a NoParentJoin option that also is used to avoid parent join errors, but in that 
    //       case the assumtion is probably that the server has some jond of filtering  
         
    method protected logical UseFilterParentJoinOnClient():
        return false.
    end.
    
    // allow subclasses to override 
    method protected logical CheckAndPrepareNamedQuery(pNAmedQuery as INamedQuery):
        return true.
    end.
    
    // allow override to only do  filewr and sort onclient if the client field is in the filter
    // It is assumed that the sort must be done on client in that case and it is removed from 
    // the server request if also in filter 
    method protected logical ResolveSortOnServerIfCLientFieldNotInFiter():
        return false.
    end.     
    
    method public override void PrepareGetTableRequest(pRequest as IGetDataTableRequest, phBuffer as handle):
         
        define variable lHasClientSort     as logical no-undo.
        define variable lClientCountOnly   as logical no-undo.
        define variable lSkipThis          as logical no-undo.
        define variable lContinue          as logical no-undo.
        define variable lSortOnCLient      as logical no-undo.     
        assign
            extent(ClientQueryEntries) = ?
            extent(ClientSort) = ?
            ClientNumRecords = ?
            ClientCount = false
            ClientSkip  = 0
            ClientQueryContext = ?
            ClientRelation = ?.
       
        if valid-object(pRequest:NamedQuery) then
            lContinue = CheckAndPrepareNamedQuery(pRequest:NamedQuery).
        else if NamedQueryOnly = false then
            lContinue = true.
    
        if lContinue then
        do:      
             
            ClientQueryEntries = cast(pRequest:GetQueryEntries(GetClientFields()),IQueryEntry).
            lHasClientSort = pRequest:HasSort(GetClientFields()).
            
            // see comments for UseFilterParentJoinOnClient 
            if UseFilterParentJoinOnClient() and HasClientParentJoin(phBuffer, GetClientFields()) then
            do: 
                ClientRelation = phBuffer:parent-relation.
             //   ClientRelation:active = false.
              // too late to turn this off here since the receive events are subscribed from parent  
            end.
            
            if extent(ClientQueryEntries) > 0 then
            do: 
                lSortOnClient = lHasClientSort. 
                if KeepServerFilter() = false then
                do:
                    pRequest:RemoveQueryEntries(ClientQueryEntries).
                    lClientCountOnly = true.
                end.
                
            end. 
            else 
                lSortOnClient = lHasClientSort and ResolveSortOnServerIfClientFieldNotInFiter() = false.
                                
            if lSortOnClient then
            do:
                ClientSort = pRequest:QueryDefinition:QuerySort.
                pRequest:RemoveQuerySort().
            end.
            
            // if client query or filter keep track and remove paging options 
            if lSortOnClient or extent(ClientQueryEntries) > 0 or valid-handle(ClientRelation) then
            do:
                if pRequest:NumRecords > 0 then
                do:
                    ClientNumRecords = pRequest:NumRecords.
                    cast(pRequest,GetDataTableRequest):NumRecords = 0.
                    lClientCountOnly = true.
                end.
                if pRequest:Skip > 0 then
                do:
                    ClientSkip = pRequest:Skip.
                    cast(pRequest,GetDataTableRequest):Skip = 0.
                    lClientCountOnly = true.
                 end.
                if valid-object(pRequest:PagingContextEntry) then
                do:
                    ClientQueryContext = pRequest:PagingContextEntry.
                    pRequest:PagingContextEntry = ?.
                    lClientCountOnly = true.
                end.
                // keep server request"count unless removal of other request options
                // above make the server count useless
                // A sub class that overrides KeepServerFilter to return true
                // can then utilize this for the count to return from the ReceiveSourceRecords
                // The receivebuffer will set ServerNumResults if count is kept 
                if pRequest:Count = true then
                do:
                    ClientCount = true. 
                    if lClientCountOnly  then
                        pRequest:Count = false.
                    
                end.
            end.
        end. // continue
      
    end method.     
    
    method protected logical HasClientParentJoin(phBuffer  as handle, pcClientFields as character extent). 
        define variable  cFields as character no-undo.
        define variable hField  as handle no-undo. 
        define variable iJoinField as integer no-undo.
        if valid-handle(phBuffer:parent-relation) then 
        do:
            cfields = BufferUtil:GetChildJoinFields(phBuffer:parent-relation).
            do iJoinField = 1 to num-entries(cfields):
                hField = phBuffer:buffer-field(entry(iJoinField,cFields)) no-error.
                if not valid-handle(hField) then 
                    undo, throw new IllegalArgumentError(Subst("Field &1 is not found in buffer &2",entry(iJoinField,cFields,phBuffer:name))).
                if Array:Find(entry(iJoinField,cFields),pcClientFields) > 0 then 
                    return true.
            end.
        end.
        return false.
    end method.
        
   // IClientJoinEntity
    method public void PrepareInnerJoin(pRequest as IGetDataRequest, phBuffer as handle, output plHasClientPredicate as logical, output poClientSort as IQuerySortEntry extent):
        define variable oParentTableRequest as IGetDataTableRequest no-undo.
        define variable cQualClientFields   as character extent no-undo.
        define variable cClientFields       as character extent no-undo.
        define variable oQueryRemovequal    as QueryRemovePredicateTableQualifier no-undo. 
        
        define variable i as integer no-undo.
        
        cClientFields = GetClientFields().
        extent(cQualClientFields) = extent(cClientfields).
        
        do i = 1 to extent(cQualClientFields):
            cQualClientFields[i] = subst("&1.&2",phBuffer:table-handle:name,cClientFields[i]).
        end.  
        
        // paging is denied on server for inner join  
        oParentTableRequest = cast(pRequest:TableRequests[1],IGetDataTableRequest).
        InnerJoinQueryEntries = cast(oParentTableRequest:GetQueryEntries(cQualClientFields),IQueryEntry).
        
        if extent(InnerJoinQueryEntries) > 0 then 
        do:
            oParentTableRequest:RemoveQueryEntries(InnerJoinQueryEntries).
            oQueryRemovequal= new QueryRemovePredicateTableQualifier(phBuffer:table-handle:name).
            // TODO add traverse of entries extent 
            oQueryRemovequal:Traverse(cast(new QueryGroup(InnerJoinQueryEntries),Ccs.BusinessLogic.IQueryGroup)).
            plHasClientPredicate = true. 
        end. 
        if oParentTableRequest:HasSort(cQualClientFields) then
            poClientSort = oParentTableRequest:RemoveQuerySort().
          
    end method. 
     
    method protected override void ReceiveBuffer( pParent as IBusinessEntity, pRequest as IGetDataTableRequest,pResponse as IGetDataTableResponse, phBuffer as handle).
        define variable hTable         as handle no-undo.
        define variable lHaveTest      as logical no-undo.
        define variable cTestKey       as character no-undo.
        define variable lOptionRequest as logical no-undo.
        define variable oService       as ClientFilterDataSource no-undo.
       
        hTable = phBuffer:table-handle.
        
        // if count was done on server set ServerNumResults 
        // so subclass can/may utilize this and add or substract depending on 
        // what is done for the count to return from the ReceiveSourceRecords
        if pRequest:Count then
            ServerNumResults = pResponse:NumResults.   
               
        if extent(ClientQueryEntries) <> ? 
        or extent(InnerJoinQueryEntries) > 0 
        or extent(ClientSort) <> ?  
        or valid-handle(ClientRelation) then
        do:

             if valid-handle(ClientRelation) then 
                 ClientRelation:active = true.
             oService = new ClientFilterDataSource(this-object). // prevents garbage cxollection - delete in finally
             if NoParentJoin then 
                oService:NoParentJoin = true.
             if extent(ClientQueryEntries) <> ? then
                 pRequest:AddQueryEntries(ClientQueryEntries).
             if extent(InnerJoinQueryEntries) > 0 then
                 pRequest:AddQueryEntries(InnerJoinQueryEntries).
                  
             if extent(ClientSort) <> ? then
                 pRequest:SetQuerySort(ClientSort).
             if ClientNumRecords > 0 then
                 cast(pRequest,GetDataTableRequest):NumRecords = ClientNumRecords.
             if ClientSkip > 0 then
                 cast(pRequest,GetDataTableRequest):Skip = ClientSkip.
             if valid-object(ClientQueryContext) then
                 pRequest:PagingContextEntry = ClientQueryContext.
             if ClientCount = true then
             do:
                 pRequest:Count = true.
             end. 
             // if count on server track the numresults  
            
             oService:ReceiveBuffer(pRequest, pResponse, phBuffer).   
        end.
        else do: 
            ReceiveSourceRecords(input table-handle hTable by-reference).
        end.    
        extent(InnerJoinQueryEntries) = ?. 
        
        finally:
            // the service leeps track of this-object for callbacks
            // this prevents GCC  so we must delete it when done 
            if valid-object(oService) then 
                delete object oService.
        end.
    end method.

end class.
