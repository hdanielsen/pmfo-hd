
/*------------------------------------------------------------------------
    File        : IFetchDelegate
    Purpose     : Fetch Data  for IFetchDelegate - currently implemented by Pmfo BusinessEntity
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 25 15:46:21 EDT 2022
    Notes       : Run as singleton - ServiceManager
                 - info about delegator is provided in parameter for IFetchDelegate    
                 The main goal is to allow pmfo and application level delegates for
                 functions                    
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.BusinessLogic.Delegate.IFetchDelegator from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
 
interface Pmfo.Core.BusinessLogic.Delegate.IFetchDelegate: 
   method public IGetDataResponse FetchData( pDelegator as IFetchDelegator, pDataAccess as IDataAccess, poRequest as IGetDataRequest,input-output dataset-handle phdataset).
   method public IGetDataResponse CreateDataResponse(poRequest as Ccs.BusinessLogic.IGetDataRequest).
        
end interface.