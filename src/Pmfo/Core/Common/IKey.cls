
/*------------------------------------------------------------------------
    File        : IKey
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 13 15:21:58 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.Common.IFieldValue from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

interface Pmfo.Core.Common.IKey:  
    define public property FieldValues as IFieldValue no-undo extent get.
    method public character ToString(pcOption as character).     
    method public JsonObject ToJson().
         
end interface.