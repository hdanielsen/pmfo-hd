
/*------------------------------------------------------------------------
    File        : INamedArrays
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Sep 30 08:52:14 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using Pmfo.Core.Common.NamedArrayNameEnum from propath.

interface Pmfo.Core.Common.INamedArrayHolder:
    
    define public property NameQualifier as NamedArrayNameEnum no-undo get.
      
    method public logical AddValues(pcName as char, pcValues as character extent). 
    method public logical AddValues(pcName as char, pcValues as integer extent). 
    method public logical AddValues(pcName as char, pcValues as int64 extent). 
    method public logical AddValues(pcName as char, pcValues as decimal extent). 
    method public logical AddValues(pcName as char, pcValues as logical extent). 
    method public logical AddValues(pcName as char, pcValues as date extent). 
    method public logical AddValues(pcName as char, pcValues as datetime extent). 
    method public logical AddValues(pcName as char, pcValues as datetime-tz extent). 
    method public logical AddValues(pcName as char, pholder as IPrimitiveArrayHolder). 
    method public character   extent GetCharacterValues(pcName as char). 
    method public integer     extent GetIntegerValues(pcName as char). 
    method public int64       extent GetInt64Values(pcName as char). 
    method public decimal     extent GetDecimalValues(pcName as char). 
    method public logical     extent GetLogicalValues(pcName as char). 
    method public date        extent GetDateValues(pcName as char). 
    method public datetime    extent GetDatetimeValues(pcName as char). 
    method public datetime-tz extent GetDatetimetzValues(pcName as char). 
    method public IPrimitiveArrayHolder GetHolder(pcName as char).    
    method public character extent GetNames().
    method public character GetBufferQualifier(phBuffer as handle).
    method public character extent GetNamesForBuffer(phBuffer as handle).  
    method public IPrimitiveArrayHolder GetHolderForField(hField as handle).  
end interface.