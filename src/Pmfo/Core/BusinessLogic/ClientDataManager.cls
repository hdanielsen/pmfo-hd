 
 /*------------------------------------------------------------------------
    File        : ClientDataSource
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Aug 13 20:28:03 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.GetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IClientEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableResponse from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryBuilder from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Core.BusinessLogic.ClientDataManager abstract: 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
   
    define abstract protected property ClientName as character no-undo get. 
    
    define protected property SeqHandle    as handle no-undo get. set.
    define protected property SeqCount     as int64  no-undo get. set.
    define public property NoParentJoin    as logical no-undo get. set.
    
    constructor public ClientDataManager ():
        super ().
    end constructor.
    
    // subclasses must implements the call to the client (BE) to the create/retrieve/manage the data  
    method protected abstract void CallBack(input pRequest as IGetDataTableRequest, input pResponse as IGetDataTableResponse,input phBuffer as handle). 
    
    method protected abstract handle CreateSourceTable(phBuffer as handle).
    
    method public void AfterRow(input dataset-handle phDataset):
        SeqCount = SeqCount + 1.              
/*        if valid-handle(SeqHandle) then       */
/*            SeqHandle:buffer-value = SeqCount.*/
    end.     
    
    method protected logical UseClientQuery(pRequest as IGetDataTableRequest,input phBuffer as handle):
        
        // NOTE: Request:Count is NOT included in the check - all client methods that reads/creates/managed data returns a count 
        return valid-object(pRequest:QueryDefinition) or valid-handle(phBuffer:parent-relation) or pRequest:IsPaging .
    end method.    
    
    method protected rowid GetSourceRowid(phSourceBuffer as handle, pQueryContext as Ccs.BusinessLogic.IQueryEntry) :
        define variable cQuery        as character no-undo.
        cQuery = pQueryContext:ToString().
        
        phSourceBuffer:find-unique("where " + cQuery).
        if phSourceBuffer:available then
           return phSourcebuffer:rowid. 
        else if phSourceBuffer:ambiguous then
            undo, throw new ApplicationError(subst("Paging Context for Record &1 &2 is ambiguous",phSourceBuffer:name,cQuery)).        
        else 
            undo, throw new ApplicationError(subst("Paging Context for Record &1 &2 did not find any record",phSourceBuffer:name,cQuery)).
        
    end method.    
    
    method protected character GetNextRowKey(phSourceBuffer as handle, pRowid as rowid):
        phSourcebuffer:find-by-rowid (pRowid).
        return ServiceInfo:GetUrlQueryRowKey(phSourcebuffer).        
    end method.    
    
    // apply filter to the CreateSourceRecords data if applicable  
    method protected void ProcessRecords(input pRequest as IGetDataTableRequest, input pResponse as IGetDataTableResponse, input phBuffer as handle).
        define variable hSourceTable   as handle no-undo.
        define variable hSourcebuffer  as handle no-undo.
        define variable hQuery         as handle no-undo.
        define variable hOutputTable   as handle no-undo.
        define variable iCount         as int64 no-undo. 
        define variable hSeq           as handle no-undo.
        define variable cQuery         as character no-undo.     
        define variable cClientQuery   as character no-undo.
        define variable hDataSource    as handle no-undo.
        define variable cQueryModifier as character no-undo.  
        define variable iSource        as integer no-undo.
        define variable iChild         as integer no-undo.
     
        // if there is a query then create a temp-table like the buffer to fill from  
        if UseClientQuery(pRequest, phBuffer) then 
        do on error undo, throw:  
            hOutputTable = phBuffer:table-handle.
            
            hSourceTable = CreateSourceTable(phBuffer).
            hSourceBuffer = hSourceTable:default-buffer-handle.  
            
            CallBack(pRequest, pResponse, hSourceBuffer).
            
            create query hQuery.
             
            if valid-object(pRequest:QueryDefinition) then
                cClientQuery = pRequest:queryDefinition:toString().
            
            if pRequest:Count or pRequest:skip > 0 then
                cQueryModifier = "preselect".
            else
                cQueryModifier = "for".
            
            create data-source hDataSource.        
            if NoPArentJoin = false and valid-handle(phBuffer:parent-relation) then 
            do:
                hQuery:add-buffer(phBuffer:parent-relation:parent-buffer).
                if cClientQuery begins "where " then
                    substring(cClientQuery,1,5) = "and". 
                cQuery = right-trim(subst("&1 each &2, &3 &4",
                                          cQueryModifier,
                                          phBuffer:parent-relation:parent-buffer:name,
                                          // remove FOR 
                                          substr(phBuffer:parent-relation:query:prepare-string,5),
                                          cClientQuery
                                    )
                          ).
                          
                iSource = 2.
 
            end.
            else do:
                cQuery = right-trim(subst("&1 each &2 &3",cQueryModifier,phBuffer:name,cClientQuery)).
                iSource = 1.
            end.
           
            hQuery:add-buffer (hSourceBuffer).
            hQuery:query-prepare(cQuery).
            hDataSource:query = hQuery.
            phBuffer:attach-data-source(hDataSource).
            phBuffer:fill-mode = "merge".
            // we are filling one table, but progress will try to fill children apparently and throw error since it is not attached
            // set them to no-fill  - children will be filled later here when
            // the child BEs receives data   
            do iChild = 1 to phBuffer:num-child-relations:
                phBuffer:get-child-relation (iChild):child-buffer:fill-mode = "no-fill". 
            end.    
             
            if pRequest:NumRecords > 0 then
                phBuffer:batch-size = pRequest:NumRecords.
            
            if pRequest:Skip > 0 then
            do:
                if pRequest:PagingContext > "" then
                    undo, throw new BadRequestError("Skip and Paging are conflicting request properties").
                hDataSource:restart-row = pRequest:Skip + 1.
            end.    
            else if valid-object(pRequest:PagingContextEntry) then
                hDataSource:restart-rowid(iSource) = GetSourceRowid(hSourceBuffer, pRequest:PagingContextEntry).
            
            // id sequence add or count call back to increment and assign the value
            // the query is closed after fill so num-results is not available
            SeqHandle = phBuffer:buffer-field("zz_seq") no-error.
            
            if valid-handle(SeqHandle) or pRequest:Count then
            do:
                SeqCount = 0.
                phBuffer:set-callback("After-Row-fill","AfterRow").
            end.
            
            phbuffer:fill().   
            
            // reset fill-mode to default   
            do iChild = 1 to phBuffer:num-child-relations:
                phBuffer:get-child-relation (iChild):child-buffer:fill-mode = "merge". // default
            end.            

            if pRequest:Count then
                cast(pResponse,GetDataTableResponse):NumResults = SeqCount.
           
            if hDataSource:next-rowid(iSource) <> ? then
            do:
                cast(pResponse,GetDataTableResponse):NextPagingContext = GetNextRowKey(hSourcebuffer, hDataSource:next-rowid(iSource)).
            end.
            catch e as Progress.Lang.Error :
                if e:GetMessageNum(1) = 7331 and valid-object(pRequest:PagingContextEntry) then
                    undo, throw new ApplicationError(subst("Cannot position to next &1 batch where &2 using current query",BufferUtil:GetRowName(phBuffer,true),pRequest:PagingContextEntry:ToString())) . 
                else 
                    undo, throw e. 
            end catch.
            finally:
                phbuffer:detach-data-source ().
                if valid-object(hDataSource) then
                    delete object hDataSource.
                if valid-object(hSourceTable) then
                    delete object hSourceTable. 
                if valid-object(hQuery) then
                    delete object hQuery. 
            end.       
        end.    
        else do:
            CallBack(pRequest, pResponse, phBuffer).
        end.    
       
    end method. 
       
        
end class.