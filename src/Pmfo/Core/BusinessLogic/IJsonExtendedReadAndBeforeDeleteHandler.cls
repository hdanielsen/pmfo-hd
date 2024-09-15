
/*------------------------------------------------------------------------
    File        : IJsonExtendedReadAndBeforeDeleteHandler
    Purpose     : Extends IJsonExtendedReadHandler to allow control of the before image deletes   
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 
    Notes       : The before image deletes are all loded by the JsonReader 
                  before the actual rows are processed 
                 - TODO: This may be simplified to    
----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.IJsonExtendedReadHandler from propath.
using Pmfo.Util.JsonReader from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

interface Pmfo.Core.BusinessLogic.IJsonExtendedReadAndBeforeDeleteHandler inherits IJsonExtendedReadHandler:  
    
    // return true to signal that the JsonReader should skip laoding the before image for this buffer  
    // typically CheckOtskipRow would handle the rows for thos table  
    method public logical LoadBeforeImageDeletesFromJson(pBeforeImageJson as JsonObject,phbuffer as handle, phParentFields as handle extent, pcChildFields as character extent,  pReader as JsonReader ). 
    
  
end interface.