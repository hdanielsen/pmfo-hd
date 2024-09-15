
/*------------------------------------------------------------------------
    File        : IGetDataMessage   
    Purpose     : 
    Syntax      : 
    Description : Support exposing server side overrides in a GetData Request in business logic 
    Author(s)   : hdaniels
    Created     : Wed Jan 31 12:00:54 EDT 2019
    Notes       :  
  ----------------------------------------------------------------------*/

using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.Common.IKey from propath.

interface Pmfo.Core.Common.IGetDataMessage:  
    define public property Id              as integer     no-undo get.
    define public property Entity          as character   no-undo get.  
    define public property Message         as character   no-undo get.
    define public property Key             as IKey        no-undo get.
    define public property Keys            as IKey extent no-undo get.
    method public JsonObject ToJson().
    
end interface.