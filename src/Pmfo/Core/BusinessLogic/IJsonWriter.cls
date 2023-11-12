
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

interface Pmfo.Core.BusinessLogic.IJsonWriter:  
    define public property DynamicExtentFields as character no-undo get. set.
    method public JsonObject WriteDataset(phDataset as handle).
end interface.