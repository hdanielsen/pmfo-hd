
/*------------------------------------------------------------------------
    File        : ISubmitDelegate
    Purpose     : Submit data for a BE
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 03 06:26:58 EST 2024
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.Object from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.Delegate.ISubmitDelegator from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetUpdateManager from propath.
 
interface Pmfo.Core.BusinessLogic.Delegate.ISubmitDelegate:  
    method public Object SubmitData(pDelegator as ISubmitDelegator, pManager as IDatasetUpdateManager, pDataAccess as IDataAccess, input poUpdateDataRequest as IUpdateDataRequest, input-output dataset-handle phDataset).
    method public void ThrowDatasetError( phDataset as handle).
end interface.