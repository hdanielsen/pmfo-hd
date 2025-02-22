
/*------------------------------------------------------------------------
    File        : IMemoryLogger
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Nov 05 07:21:18 EST 2024
    Notes       : 
  ----------------------------------------------------------------------*/

using OpenEdge.Web.IWebRequest from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

interface Pmfo.Util.IMemoryLogger:  
    method public void Log(poWebRequest as IWebRequest).
end interface.