
/*------------------------------------------------------------------------
    File        : IJsonExtendedReadHandler
    Purpose     : Extends JsonReadHandler to allow control of rows   
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 
    Notes       : support skipping a row (condtional create ) 
                  It is likely still best to use the intherited 
                  readField and handlefiednotinjosn for field management/control  
----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.IJsonReadHandler from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Util.JsonReader from propath.

interface Pmfo.Core.BusinessLogic.IJsonExtendedReadHandler inherits IJsonReadHandler:  
    
    // called with buffer released and not avail 
    // return true to signal that the JsonReader should skip this row 
    // the buffer is released before this call and created right after so it should be safe to navigate it 
    method public logical CheckOrSkipRow(pJson as JsonObject, phBuffer as handle, pReader as JsonReader).
    
end interface.