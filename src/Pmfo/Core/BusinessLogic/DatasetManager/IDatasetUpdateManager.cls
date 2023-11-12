
/*------------------------------------------------------------------------
    File        : IDatasetUpdateManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jan 10 10:17 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetManager from propath.

interface Pmfo.Core.BusinessLogic.DatasetManager.IDatasetUpdateManager inherits IDatasetManager:  
    method public void GetChildWarnings (output table-handle phWarningTable).
end interface.