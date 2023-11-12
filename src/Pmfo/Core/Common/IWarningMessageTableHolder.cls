
/*------------------------------------------------------------------------
    File        : ISuccessMessage
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 13 15:38:19 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

 
 
using Progress.Json.ObjectModel.JsonArray from propath.
using Pmfo.Core.Common.IWarning from propath.

interface Pmfo.Core.Common.IWarningMessageTableHolder:
    define public property TableHandle as handle no-undo get. set. 
    method public void GetWarningTableData(output table-handle phTable) .         
end interface.