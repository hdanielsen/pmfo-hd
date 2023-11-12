 
 /*------------------------------------------------------------------------
    File        : IFetchFunctionDelegate
    Purpose     : Fetch data for unique function or key array function 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Apr 28 2022
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.Delegate.IFetchDelegator from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
 
interface Pmfo.Core.BusinessLogic.Delegate.IFetchFunctionDelegate:
    
    /***** Fetch for unique ***/  
    // use for Bound function that already has find request - remove namedquery and issue request to Data Access
    method public IGetDataResponse FetchForUniqueFunction( pDelegator as IFetchDelegator, pDataAccess as IDataAccess,input poRequest as IGetDataRequest, input-output dataset-handle phDataset ).
    
    /***** BEGIN: Fetch for primary key array for different data types 
           use for function with array on primary key - remove namedquery and convert to inlist request and issue request to Data Access    ***/  
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, piIds as integer extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ). 
    
    // use for function with invid array on primary key - remove namedquery and convert to inlist request and issue request to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, piInt64Ids as int64 extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ). 
    
    // use for function with invid array on primary key - remove namedquery and convert to inlist request and issue rdequest to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pdIds as decimal extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ). 
    
    // use for function with invid array on primary key - remove namedquery and convert to inlist request and issue request to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcIds as character extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ). 
    
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, ptIds as date extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ). 
    
    /***** END: Fetch for primary key array for different data types ***/  
    
    /***** BEGIN: Fetch for named  key array for different data types ***/  
     // use for function with invid array - remove namedquery and convert to inlist request and issue request to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, piIds as integer extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ).
    
     // use for function with invid array - remove namedquery and convert to inlist request and issue request to Data Access
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, piInt64Ids as int64 extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ).
    
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, pdIds as decimal extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ).
    
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, pcIds as character extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ).
    
    method public IGetDataResponse FetchForArrayFunction(pDelegator as IFetchDelegator, pDataAccess as IDataAccess, pcKey as character, ptIds as date extent, input poRequest as IGetDataRequest, input-output dataset-handle phDataset ).
    /***** EWD: Fetch for named  key array for different data types ***/  
    
    // base fetch - wrapper to IFetchDelegate 
    method public IGetDataResponse FetchData( pDelegator as IFetchDelegator, pDataAccess as IDataAccess, poRequest as IGetDataRequest,input-output dataset-handle phdataset).
    
end interface.