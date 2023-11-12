
/*------------------------------------------------------------------------
    File        : IParameter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu May 30 08:40:05 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.DataTypeEnum from propath.

interface Pmfo.Core.Common.IParameter:  
    define public property Name         as character no-undo get.  
    define public property DataType     as DataTypeEnum no-undo get.  
    define public property IsArray      as logical no-undo get.  
    define public property IsPrimitive  as logical no-undo get.  
    
end interface.