
/*------------------------------------------------------------------------
    File        : IJsonWriter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Mar 12 03:58:01 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/
 
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IJsonWriter from propath.

interface Pmfo.Core.BusinessLogic.IJsonRecursiveWriter inherits IJsonWriter:  
    method public JsonObject WriteDataset(phDataset as handle, pGetTableRequest as IGetDataTableRequest, piLevels as integer).
end interface.