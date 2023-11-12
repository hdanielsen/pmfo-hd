
/*------------------------------------------------------------------------
    File        : ITransactionManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Aug 04 16:24:38 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.DataLayer.Update.IDatasetUpdateManager from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.

interface Pmfo.Core.DataLayer.Update.ITransactionManager inherits IDatasetUpdateManager:  
    define public event ProcessInfrastructureData signature void (input phInfrastructureDataset as handle, phApplicationDataset as handle). 
    
    method public void ProcessDataset(phDataset as handle).
    method public void ProcessDataset(phDataset as handle, phInfrastructureDataset as handle).
    
end interface.