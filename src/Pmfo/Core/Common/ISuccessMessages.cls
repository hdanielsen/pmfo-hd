
/*------------------------------------------------------------------------
    File        : ISuccessMessage
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 13 15:38:19 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.Common.ISuccessMessage from propath.
using Progress.Json.ObjectModel.JsonArray from propath.

interface Pmfo.Core.Common.ISuccessMessages: 
    method public logical Add(posucces as ISuccessMessage):
    method public JsonArray ToJsonArray() :         
end interface.