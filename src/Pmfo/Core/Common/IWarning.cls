
/*------------------------------------------------------------------------
    File        : IWarning
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Aug 10 11:23:36 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Core.Common.IWarning:  
    define public property Message   as character get. 
    define public property FieldName as character get. 
end interface.