 
 /*------------------------------------------------------------------------
    File        : FetchDelegate
    Purpose     : Fetch data for function
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 25 16:33:27 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchDelegate from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchDelegator from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchFunctionDelegate from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.StringUtil from propath.
 
class Pmfo.Core.BusinessLogic.Delegate.FetchFunctionDelegate implements IFetchFunctionDelegate:
    
    define protected property FetchDelegate as IFetchDelegate no-undo get. private set.
     
    constructor public FetchFunctionDelegate (pFetchDelegate as IFetchDelegate ):
        super ().
        FetchDelegate = pFetchDelegate.
    end constructor.
  
    /***** Fetch for unique ***/  
    // use for Bound function that already has find request - remove namedquery and issue request to Data Access
    method public IGetDataResponse FetchForUniqueFunction( pDelegator as IFetchDelegator, pDataAccess as IDataAccess,input poRequest as IGetDataRequest, input-output dataset-handle phDataset ):
        PrepareAndAssertUniqueTableRequest(pDelegator, poRequest, phDataset).
        return FetchData(pDelegator, pDataAccess, poRequest, input-output dataset-handle phDataset by-reference). 
    end method.    
    
    /***** BEGIN: Fetch for primary key array for different data types 
           use for function with array on primary key - remove namedquery and convert to inlist request and issue request to Data Access
    ***/  
     
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, piIds as integer extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ): 
        return FetchForArrayFunction(pDelegator, pDataAccess,pDelegator:GetKeys(), piIds, poRequest, input-output dataset-handle phDataset by-reference).
    end method.    
    
    // use for function with invid array on primary key - remove namedquery and convert to inlist request and issue request to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, piInt64Ids as int64 extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ): 
        return FetchForArrayFunction(pDelegator, pDataAccess,pDelegator:GetKeys(), piInt64Ids, poRequest, input-output dataset-handle phDataset by-reference).
    end method.    
   
    // use for function with invid array on primary key - remove namedquery and convert to inlist request and issue rdequest to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pdIds as decimal extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ): 
        return FetchForArrayFunction(pDelegator, pDataAccess,pDelegator:GetKeys(), pdIds, poRequest, input-output dataset-handle phDataset by-reference).
    end method.    
    
    // use for function with invid array on primary key - remove namedquery and convert to inlist request and issue request to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcIds as character extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ): 
        return FetchForArrayFunction(pDelegator, pDataAccess,pDelegator:GetKeys(), pcIds, poRequest, input-output dataset-handle phDataset by-reference).
    end method.    
    
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, ptIds as date extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ): 
        return FetchForArrayFunction(pDelegator, pDataAccess,pDelegator:GetKeys(), ptIds, poRequest, input-output dataset-handle phDataset by-reference).
    end method.    
    
    /***** END: Fetch for primary key array for different data types ***/  
    
    /***** BEGIN: Fetch for named  key array for different data types ***/  
    
    
     // use for function with invid array - remove namedquery and convert to inlist request and issue request to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, piIds as integer extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ):
        AssertAndAddValuesToRequest(pDelegator, poRequest, phDataset, pcKey, piIds). 
        return  FetchData(pDelegator, pDataAccess, poRequest, input-output dataset-handle phDataset by-reference). 
    end method. 
    
     // use for function with invid array - remove namedquery and convert to inlist request and issue request to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, piInt64Ids as int64 extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ):
        AssertAndAddValuesToRequest(pDelegator, poRequest, phDataset, pcKey, piInt64Ids). 
        return  FetchData(pDelegator, pDataAccess, poRequest, input-output dataset-handle phDataset by-reference). 
    end method. 
    
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, pdIds as decimal extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ):
        AssertAndAddValuesToRequest(pDelegator, poRequest, phDataset, pcKey, pdIds). 
        return  FetchData(pDelegator, pDataAccess, poRequest, input-output dataset-handle phDataset by-reference). 
    end method. 
    
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, pcIds as character extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ):
        AssertAndAddValuesToRequest(pDelegator, poRequest, phDataset, pcKey, pcIds). 
        return  FetchData(pDelegator, pDataAccess, poRequest, input-output dataset-handle phDataset by-reference). 
    end method. 
    
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, ptIds as date extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ):
        AssertAndAddValuesToRequest(pDelegator, poRequest, phDataset, pcKey, ptIds). 
        return  FetchData(pDelegator, pDataAccess, poRequest, input-output dataset-handle phDataset by-reference). 
    end method.
    
   
    /***** END: Fetch for named  key array for different data types ***/  
         
    
    /**** BEGIN: AssertAndAddValues for different data types ****/  
    method protected void AssertAndAddValuesToRequest(pDelegator as IFetchDelegator, poRequest as IGetDataRequest, phDataset as handle, pcKey as character, piValues as integer extent):
        define variable oTableRequest as IGetDataTableRequest no-undo. 
        oTableRequest = PrepareAndAssertArrayRequest(pDelegator,poRequest,phDataset,pcKey,"integer").
        oTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,piValues)).
    end method.
    
    method protected void AssertAndAddValuesToRequest(pDelegator as IFetchDelegator, poRequest as IGetDataRequest, phDataset as handle, pcKey as character, pInt64Values as int64 extent):
        define variable oTableRequest as IGetDataTableRequest no-undo. 
        oTableRequest = PrepareAndAssertArrayRequest(pDelegator,poRequest,phDataset,pcKey,"int64").
        oTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,pInt64Values)).
    end method.
   
    method protected void AssertAndAddValuesToRequest(pDelegator as IFetchDelegator, poRequest as IGetDataRequest, phDataset as handle, pcKey as character, pdValues as decimal extent):
        define variable oTableRequest as IGetDataTableRequest no-undo. 
        oTableRequest = PrepareAndAssertArrayRequest(pDelegator,poRequest,phDataset,pcKey,"decimal").
        oTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,pdValues)).
    end method.
    
    method protected void AssertAndAddValuesToRequest(pDelegator as IFetchDelegator, poRequest as IGetDataRequest, phDataset as handle, pcKey as character, pcValues as character extent):
        define variable oTableRequest as IGetDataTableRequest no-undo. 
        oTableRequest = PrepareAndAssertArrayRequest(pDelegator,poRequest,phDataset,pcKey,"character").
        oTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,pcValues)).
    end method.
    
    method protected void AssertAndAddValuesToRequest(pDelegator as IFetchDelegator, poRequest as IGetDataRequest, phDataset as handle, pcKey as character, ptValues as date extent):
        define variable oTableRequest as IGetDataTableRequest no-undo. 
        oTableRequest = PrepareAndAssertArrayRequest(pDelegator,poRequest,phDataset,pcKey,"date").
        oTableRequest:AddQueryEntry(new QueryPredicate(pcKey, QueryOperatorEnum:InList,ptValues)).
    end method.

    /**** END: AssertAndAddValues for different data types ****/  
    
    method public IGetDataResponse FetchData( pDelegator as IFetchDelegator, pDataAccess as IDataAccess, poRequest as IGetDataRequest,input-output dataset-handle phdataset).
        return FetchDelegate:FetchData(pDelegator, pDataAccess, poRequest, input-output dataset-handle phDataset by-reference). 
    end method. 
     
    
    /***** Get the table request ******/
    method protected IGetDataTableRequest GetTableRequest(input pDelegator as IFetchDelegator,input poRequest as IGetDataRequest, phDataset as handle):
        define variable hBuffer as handle no-undo.
        return GetTableRequest(pDelegator, poRequest, phDataset, output hBuffer). 
    end method.
   
    method protected IGetDataTableRequest GetTableRequest(input pDelegator as IFetchDelegator,input poRequest as IGetDataRequest, phDataset as handle, output phBuffer as handle):
        phBuffer  = DatasetUtil:GetBufferBySerializeName(phDataset, pDelegator:Resource).
        if not valid-handle(phBuffer)  then
            undo, throw new IllegalStateError(subst("The dataset does not have a buffer for Resource '&1'",pDelegator:Resource)).
        return cast(poRequest:GetTableRequest(phbuffer:table-handle:name),IGetDataTableRequest).
    end method. 
    
      /** final prepare for unique  **/ 
    method protected void PrepareAndAssertUniqueTableRequest(pDelegator as IFetchDelegator, poRequest as IGetDataRequest, phDataset as handle):
        define variable oTableRequest as IGetDataTableRequest no-undo.
        oTableRequest = GetTableRequest(pDelegator, poRequest, phDataset).
        if oTableRequest:FindUnique = false then 
            undo, throw new IllegalArgumentError("Request is not a unique find").
        oTableRequest:NamedQuery = ?.
    end method.    
    
    
    /** final prepare for array  **/ 
    method protected IGetDataTableRequest PrepareandAssertArrayRequest(pDelegator as IFetchDelegator, poRequest as IGetDataRequest, phDataset as handle, pcKey as character, pcDatatype as character):
        define variable hBuffer as handle no-undo.
        define variable oTableRequest as IGetDataTableRequest no-undo.
        oTableRequest = GetTableRequest(pDelegator, poRequest, phDataset, output hBuffer).
        
        if oTableRequest:FindUnique = true then 
            undo, throw new IllegalArgumentError("Request is a unique find").
        
        AssertKey(hBuffer,pcKey).
        AssertKeyType(hBuffer,pcKey,pcDatatype).
        oTableRequest:NamedQuery = ?.
        return oTableRequest.    
    end method.
    
    /***  ***/ 
    
    method protected character GetDelegatorRowName(pDelegator as IFetchDelegator, phDataset as handle):
        define variable hBuffer as handle no-undo. 
        hBuffer  = DatasetUtil:GetBufferBySerializeName(phDataset, pDelegator:Resource).
        return BufferUtil:GetRowName(hBuffer, true). 
    end method.    
   
    
    method protected void AssertKey(phbuffer as handle, pcKey as character):
        if num-entries(pcKey) > 1 then 
            undo, throw new UnsupportedOperationError(subst("Fetch for &1 key with multiple fields - &2",BufferUtil:GetRowName(phBuffer, true),StringUtil:GetMessageFromEntries(pcKey," and ", true))).
    end method.    
    
    method protected void AssertKeyType(phbuffer as handle, pcKey as character, pcDataType as character):
        if phBuffer:buffer-field(pcKey):data-type <> pcDataType then 
            undo, throw new IllegalArgumentError(subst("The &1 key is data type '&2' and not an '&3'",pcKey,phBuffer:buffer-field(pcKey):data-type,lc(pcDatatype))). 
    end method.    
    
    
end class.