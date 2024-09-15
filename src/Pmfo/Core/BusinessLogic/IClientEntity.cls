
/*------------------------------------------------------------------------
    File        : IClientEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Aug 13 20:28:33 EDT 2020
    Notes       : callback for ClientDataSource
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.BusinessLogic.IClientJoinEntity from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.

interface Pmfo.Core.BusinessLogic.IClientEntity inherits IClientJoinEntity:  
    method public int64 CreateSourceRecords(output table-handle phTable).
      
    // hook from BusinessEntity:submitData called when ClientOnly 
    // Default to throw error message - UpdateData is typically not called  
    method public Object SubmitClientData(poUpdateDataRequest as IUpdateDataRequest, input-output dataset-handle phDataset, input dataset-handle phInfrastructureDataset, output table-handle phWarning).
    
end interface.