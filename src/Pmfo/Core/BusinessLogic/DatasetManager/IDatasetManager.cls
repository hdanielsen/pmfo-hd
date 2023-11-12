
/*------------------------------------------------------------------------
    File        : IDatasetManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jan 10 09:58:07 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IRequest from propath.

interface Pmfo.Core.BusinessLogic.DatasetManager.IDatasetManager:  
    method public IBusinessEntity GetBusinessEntity(phbuffer as handle).
    method public IBusinessEntity GetStartedBusinessEntity(phbuffer as handle).
    method public void ProcessDataset(pBusionessentoty as IBusinessEntity, phDataset as handle, pRequest as IRequest).
end interface.