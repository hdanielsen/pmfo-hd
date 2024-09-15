
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
using Pmfo.Core.Common.IWarningMessages from propath.

interface Pmfo.Core.Common.IWarningMessages: 
    define public property HasData as logical  no-undo get. 
    method public logical Add(pcEntity as character, poWarning as IWarning).
    method public logical Add(pcEntity as character, pcMessage as character).
    method public logical Add(pcEntity as character, pcMessage as character, pcFieldName as character).
    method public logical AddAll (pcResource as character,poWarnings as IWarningMessages).
    method public logical Clear ().
    
    method public IWarning extent GetWarnings().
    method public JsonArray ToJsonArray() :         
end interface.