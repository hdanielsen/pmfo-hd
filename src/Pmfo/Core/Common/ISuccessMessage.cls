
/*------------------------------------------------------------------------
    File        : ISuccessMessage
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 13 15:38:19 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.Common.IKey from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

interface Pmfo.Core.Common.ISuccessMessage: 
    define property Entity     as character no-undo get.
    define property Key        as IKey no-undo get. 
    define property UrlId      as character no-undo get. 
    
    define property Message as character no-undo get. set. 
    
    method JsonObject ToJson():
        

end interface.