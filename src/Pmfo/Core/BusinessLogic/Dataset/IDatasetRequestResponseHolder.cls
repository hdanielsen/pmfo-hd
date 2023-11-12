
/*------------------------------------------------------------------------
    File        : IDatasetHolder
    Purpose     : Allow different relations and different tables, but same data for same tables for client and server  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 14 15:25:20 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.Dataset.IDatasetHolder from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.

interface Pmfo.Core.BusinessLogic.Dataset.IDatasetRequestResponseHolder inherits IDatasetHolder:  
     define public property ResponseDataset   as handle no-undo get.
end interface.