
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

interface Pmfo.Core.BusinessLogic.IClientEntity inherits IClientJoinEntity:  
    method public int64 CreateSourceRecords(output table-handle phTable).
end interface.