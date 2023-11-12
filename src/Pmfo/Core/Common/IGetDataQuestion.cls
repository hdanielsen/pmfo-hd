
/*------------------------------------------------------------------------
    File        : IGetDataQuestion:  
    Purpose     : 
    Syntax      : 
    Description : Support server side questions for a GetData Request in business logic or data access
    Author(s)   : hdaniels
    Created     : Mon Aug 19 12:00:54 EDT 2019
    Notes       : Use for Assert type of Get request to allow questions to be asked before the actual update
                  The client must collect the Ids of all and submit as comma separated list of confirmations in  url query on submit   
  ----------------------------------------------------------------------*/

using Progress.Json.ObjectModel.JsonObject from propath.

interface Pmfo.Core.Common.IGetDataQuestion:  
    define public property Key     as character no-undo get.  
    define public property Entity  as character no-undo get.  
    define public property Message as character no-undo get.
    
    method public JsonObject ToJson().
    
end interface.