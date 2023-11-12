
/*------------------------------------------------------------------------
    File        : IDataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 11 00:12:39 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.IGetDataRequest from propath.
 
using Progress.Lang.Object from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.DataLayer.IDataAccessResource from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.

interface Pmfo.Core.DataLayer.IDataAccess inherits IDataAccessResource:  
    method public IGetDataResponse FetchData ( input poRequest as IGetDataRequest, input-output dataset-handle phDataset ).
    method public Object SubmitData( input poRequest as IUpdateDataRequest, input-output dataset-handle phDataset, input dataset-handle phAlertDataset, output table-handle phWarning ).
    // TODO Do we need this? 
    //  method public Object SubmitData( input poRequest as Ccs.BusinessLogic.IUpdateDataRequest, input-output dataset-handle phDataset, output table-handle phWarning ).
    
end interface.