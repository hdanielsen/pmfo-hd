
/*------------------------------------------------------------------------
    File        : IGetDataMessages
    Purpose     : Track messages for request as property on Request/Response
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jan 31 2024
    Notes       :   
  ----------------------------------------------------------------------*/
 
using Progress.Json.ObjectModel.JsonArray from propath.
 
using Pmfo.Core.Common.IGetDataMessage from propath.
using Pmfo.Core.Common.IGetDataMessages from propath.

interface Pmfo.Core.Common.IGetDataMessages: 
    define public property Size as integer no-undo get. 
     
    method public logical Add(poMessage as IGetDataMessage).
    method public logical AddAll (poMessages as IGetDataMessages).
    method public IGetDataMessage extent ToArray() .
    method public JsonArray ToJsonArray() :
    method public void Clear().         
end interface.