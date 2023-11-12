
/*------------------------------------------------------------------------
    File        : IGetDataQuestions
    Purpose     : Track questions for GetData request as proparty on IGetdataRequest
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon july 3  2023
    Notes       : Transferred to GetDataResponse at end of GET request
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.Common.IGetDataQuestion from propath.
using Pmfo.Core.Common.IGetDataQuestions from propath.
using Progress.Json.ObjectModel.JsonArray from propath.

interface Pmfo.Core.Common.IGetDataQuestions: 
    define public property Size as integer no-undo get. 
     
    method public logical Add(poQuestion as IGetDataQuestion).
    method public logical AddAll (poQuestions as IGetDataQuestions).
    method public IGetDataQuestion extent ToArray() .
    method public JsonArray ToJsonArray() :
    method public void Clear().         
end interface.