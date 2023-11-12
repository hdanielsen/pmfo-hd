 
 /*------------------------------------------------------------------------
    File        : FetchDelegate
    Purpose     : All requests from BE to DataAccess is delegated to this 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 25 16:33:27 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchDelegate from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchDelegator from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataResponse from propath.
using Pmfo.Core.BusinessLogic.GetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.BusinessLogic.ReceiveDataEventArgs from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.StringUtil from propath.
 
class Pmfo.Core.BusinessLogic.Delegate.FetchDelegate implements IFetchDelegate: 
    constructor public FetchDelegate ( ):
        super ().
    end constructor.
    
    method public IGetDataResponse FetchData( pDelegator as IFetchDelegator, pDataAccess as IDataAccess, poRequest as IGetDataRequest,input-output dataset-handle phdataset). 
        define variable oGetResponse     as Pmfo.Core.BusinessLogic.IGetDataResponse no-undo.
        define variable i as integer no-undo.
        define variable lClientOnly as logical init ? no-undo.
        define variable oIter as IIterator  no-undo.
        define variable cServerJoins as character no-undo.
        define variable iJoins as integer no-undo.
        define variable hChildbuffer as handle no-undo.      
        define variable oTableRequest as Ccs.BusinessLogic.IGetDataTableRequest no-undo.  
        
        
        // The delegator is not necessarily the top request if parents are in the the url so we cannot blindly trust 
        // delegator:clientonly. 
        // -  functions will be called in the BE that wons the function and not in the parent(s) referenced in the URL 
        // -  As of current regular read is managed by the first resouece in the url but we may possibly want to change that 
        //    in the future  
        // If the delegator is a container with no corresponding table then we use the delegates clientonly if it is true
        // -  This is typically used for central BEs that are not database oriented, like session and defined as ClientOnly
        //    since the data is loaded differetly   
        //    There may also be containers that has more then one table at the top level in which case clientonly is false.
        //    In that case we do not use th4e false value to decife, but let the tablerequest:ClientOnly (set from BE:ClientOnly)
        //    decide in the check below             
        if pdelegator:ClientOnly then
        do:
            if pdelegator:resource = phdataset:serialize-name then 
            do:
                // if there is no table with the same public name as the dataset then this is a true container 
                // and should decide clientonly  
                if not valid-handle(DatasetUtil:GetBufferByTableSerializeName(phDataset, pdelegator:resource)) then
                    lClientOnly = true.
            end.
        end.     
             
        // Unless ClientOnly was set to true above, check if any top table has clientonly false in whicc case  we need to 
        // call the dataaccess.  
        if lClientOnly <> true then
        tablerequestloop:     
        do i = 1 to phdataset:num-top-buffers:
            oTableRequest = poRequest:GetTableRequest(phdataset:get-top-buffer(i):table-handle:name).
            if valid-object(oTableRequest) then
            do: 
                if type-of(oTableRequest,IGetDataTableRequest) then 
                do:
                    if cast(oTableRequest,IGetDataTableRequest):ClientOnly = false then
                    do: 
                        lClientOnly = false.
                        leave tablerequestloop.
                    end.
                    // else only set to true if not already to false by another (may not happen since we leave as soon as it is encoutered,
                    // but... 
                    else if lClientOnly = ? then 
                    do:
                       lClientOnly = true.
                    end.
                end.
                else do: // in theory: a class that does not have the property cannot client only  
                    lClientOnly = false.
                    leave tablerequestloop.
                end.
            end. 
            // else should we throw error here if there ar eno table requests ?? 
            // the data access will thow an error if no datasources were started
        end.
        
        if lClientOnly = true then 
            oGetResponse = CreateDataResponse(poRequest). 
        else do: // TODO:? optimize for cases where the dataset alreaydy has data??
              // this is in effect an append on server - deep copy both ways  
             // The passing ensures that the server gets the definitions which is not possible to detect 
             // on server since we support lots of variations for one class
             // But it may possibly be nore efficient to manage this differenr when the dataset has data
             // and somehow avoid passing existing data  
             // (but this is complicated - the first FetchData API tried to pass defs separately, but  
             // was discarded since it did not resolve how to return it in a spearate output parameter)
              
            oGetResponse = pDataAccess:FetchData(poRequest, input-output dataset-handle phdataset).
             
            if poRequest:MainRequestedBuffer > "" then 
            do:
               // If there are server joins disable the relations to avoid the empty json arrya in the repsonse  
               // This is not critical and not tested for all cases so use NO-ERROR for all statements (for now) 
               // TODO - consider extending the DataSetRouter to mark the relation added to the ClientReeust with some propery instead  
               // and let the JsonGetResponseProcsssor deal with this (DataSetRouter:GetFullExpand manages the query-only refs )       
                cast(poRequest:GetTableRequest(phDataset:get-buffer-handle(poRequest:MainRequestedBuffer)),IGetDataTableRequest):GetInnerJoins(output cServerJoins) no-error.
                if cServerJoins > "" then 
                do:
                    do iJoins = 1 to num-entries(cServerJoins):
                        hChildbuffer = DatasetUtil:GetBufferByTableName(phDataset,entry(iJoins,cServerJoins)) no-error.
                        if valid-handle(hChildBuffer) 
                        and hChildBuffer:table-handle:has-records = false 
                        and valid-handle(hChildBuffer:parent-relation)
                        then
                            hChildBuffer:parent-relation:active = false no-error.
                    end.
                end. 
            end. 
        end.
        if valid-object(oGetResponse) then
        do:
            oGetResponse:SetRequestProperties(poRequest).
            pDelegator:ReceiveData(new ReceiveDataEventArgs(poRequest,oGetResponse), phDataset).     
        end.
        if cast(poRequest,GetDataRequest):HasWarnings then 
        do: 
            oIter = cast(poRequest,GetDataRequest):Warnings:Iterator(). 
            do while oIter:HasNext():
                pDelegator:AddWarning(oIter:Next():ToString()). 
            end.
            // if this is a fetch for function we need to check for warnings
            //  again im case warings are added after the fetch so get rid of the ones we have transferred 
            cast(poRequest,GetDataRequest):Warnings:Clear().
            cast(poRequest,GetDataRequest):Warnings = ?.
        end.    
        
        return oGetResponse.   
    
    end method. 
    
    method protected IGetDataTableRequest GetTableRequest(input pDelegator as IFetchDelegator,input poRequest as IGetDataRequest, phDataset as handle, output phBuffer as handle):
        phBuffer  = DatasetUtil:GetBufferBySerializeName(phDataset, pDelegator:Resource).
        
        if not valid-handle(phBuffer)  then
            undo, throw new IllegalStateError(subst("The dataset does not have a buffer for Resource '&1'",pDelegator:Resource)).
        
        return cast(poRequest:GetTableRequest(phbuffer:table-handle:name),IGetDataTableRequest).
    end method.
    
    method protected void AssertAndAddValuesToRequest(pDelegator as IFetchDelegator, poRequest as IGetDataRequest, phDataset as handle, pcKey as character, piValues as integer extent):
        define variable oTableRequest as IGetDataTableRequest no-undo. 
        define variable hBuffer as handle no-undo.
        oTableRequest = GetTableRequest(pDelegator, poRequest, phDataset, output hBuffer).
        AssertKey(hBuffer,pcKey).
        AssertKeyType(hBuffer,pcKey,"integer").
        AssertAndAddValuesToRequest(oTableRequest, hbuffer, pcKey, pivalues).
    end method.
    
    method protected void AssertAndAddKeysToRequest(pDelegator as IFetchDelegator, poRequest as IGetDataRequest, phDataset as handle, pcKey as character, pcValues as character extent):
        define variable oTableRequest as IGetDataTableRequest no-undo. 
        define variable hBuffer as handle no-undo.
        oTableRequest = GetTableRequest(pDelegator, poRequest, phDataset, output hBuffer).
        AssertKey(hBuffer,pcKey).
        AssertKeyType(hBuffer,pcKey,"character").
        AssertAndAddValuesToRequest(oTableRequest, hbuffer, pcKey, pcvalues).
    end method.
    
    method protected character GetDelegatorRowName(pDelegator as IFetchDelegator, phDataset as handle):
        define variable hBuffer as handle no-undo. 
        hBuffer  = DatasetUtil:GetBufferBySerializeName(phDataset, pDelegator:Resource).
        return BufferUtil:GetRowName(hBuffer, true). 
    end method.    
    
    method protected character GetAndAssertKey(pDelegator as IFetchDelegator, phDataset as handle):
        define variable cKey as character no-undo.
        cKey = pDelegator:GetKeys(). 
        if num-entries(cKey) > 1 then 
            undo, throw new UnsupportedOperationError(subst("Fetch for key &1 with multiple fields - &2",GetDelegatorRowName(pDelegator,phDataset),StringUtil:GetMessageFromEntries(cKey," and ", true))).
        
        return cKey.
        
    end method.
     
    method protected void AssertKey(phbuffer as handle, pcKey as character):
        if num-entries(pcKey) > 1 then 
            undo, throw new UnsupportedOperationError(subst("Fetch for key &1 with multiple fields - &2",BufferUtil:GetRowName(phBuffer, true),StringUtil:GetMessageFromEntries(pcKey," and ", true))).
    end method.    
    
    method protected void AssertKeyType(phbuffer as handle, pcKey as character, pcDataType as character):
        if phBuffer:buffer-field(pcKey):data-type <> pcDataType then 
            undo, throw new IllegalArgumentError(subst("The &1 key is data type '&2' and not an '&3'",pcKey,phBuffer:buffer-field(pcKey):data-type,lc(pcDatatype))). 
    end method.    
    
    method protected void AssertAndAddValuesToRequest(poTableRequest as IGetDataTableRequest, phbuffer as handle, pcKey as character, pivalues as integer extent):
        if poTableRequest:FindUnique = true then 
            undo, throw new IllegalArgumentError("Request is a unique find").
        poTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,piValues)).
    end method.
    
    method protected void AssertAndAddValuesToRequest(poTableRequest as IGetDataTableRequest, phbuffer as handle, pcKey as character, pcvalues as character extent):
        if poTableRequest:FindUnique = true then 
            undo, throw new IllegalArgumentError("Request is a unique find").
        poTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,pcValues)).
    end method.
    
    method protected void AssertAndAddValuesToRequest(poTableRequest as IGetDataTableRequest, phbuffer as handle, pcKey as character, pdvalues as decimal extent):
        if poTableRequest:FindUnique = true then 
            undo, throw new IllegalArgumentError("Request is a unique find").
        poTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,pdValues)).
    end method.
    
    method protected void AssertAndAddValueToRequest(poTableRequest as IGetDataTableRequest, phbuffer as handle, pcKey as character, pivalue as integer):
        if poTableRequest:FindUnique = false then 
            undo, throw new IllegalArgumentError("Request is a not a  unique find").
        poTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,piValue)).
    end method.
    
    method protected void AssertAndAddValuesToRequest(poTableRequest as IGetDataTableRequest, phbuffer as handle, pcKey as character, pcvalue as character):
        if poTableRequest:FindUnique = false then 
            undo, throw new IllegalArgumentError("Request is a not a unique find").
        poTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,pcValue)).
    end method.
    
    method protected void AssertAndAddValuesToRequest(poTableRequest as IGetDataTableRequest, phbuffer as handle, pcKey as character, pdvalue as decimal):
        if poTableRequest:FindUnique = false then 
            undo, throw new IllegalArgumentError("Request is a not a unique find").
        poTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,pdValue)).
    end method.
    
    method  protected void AssertAndAddKeysToRequest(oTableRequest as IGetDataTableRequest, phBuffer as handle, pcKey as character, pdValues as decimal):
        if num-entries(pcKey) > 1 then 
            undo, throw new IllegalArgumentError(subst("The Key for &1 has multiple fields - &2",BufferUtil:GetRowName(phBuffer, true),StringUtil:GetMessageFromEntries(pcKey," and ", true))).
            
        if phBuffer:buffer-field(pcKey):data-type <> "decimal" then 
            undo, throw new IllegalArgumentError(subst("The &1 key is data type '&2' and not a decimal",pcKey,phBuffer:buffer-field(pcKey):data-type)).
       
        oTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,pdValues)).
    end.    
    
    method public IGetDataResponse CreateDataResponse(poRequest as Ccs.BusinessLogic.IGetDataRequest):
        define variable iTable as integer no-undo.
        define variable oTableResponses as IGetDataTableResponse extent no-undo.
        
        extent(oTableResponses) = extent(poRequest:TableRequests).
        do iTable = 1 to extent(poRequest:TableRequests):
            oTableResponses[iTable] = new GetDataTableResponse(poRequest:TableRequests[iTable]:TableName).
        end.    
        return new GetDataResponse(oTableResponses).
    end method.       
    
end class.