
/*------------------------------------------------------------------------
    File        : IClientFilteredEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Aug 13 20:28:33 EDT 2020
    Notes       : Callback for ClientFilterDataSource 
  ----------------------------------------------------------------------*/
 
 
using Pmfo.Core.BusinessLogic.IClientJoinEntity from propath.

interface Pmfo.Core.BusinessLogic.IClientFilteredEntity inherits IClientJoinEntity:  
    method public int64 ReceiveSourceRecords(input table-handle phTable).
   
end interface.