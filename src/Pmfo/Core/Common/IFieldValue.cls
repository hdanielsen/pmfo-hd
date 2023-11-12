
/*------------------------------------------------------------------------
    File        : IFieldValue
    Purpose     : 
    Syntax      : 
    Description : Field value as object
    Author(s)   : hdaniels
    Created     : Fri May 13 12:31:35 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Ccs.Common.Support.IPrimitiveHolder from propath.

interface Pmfo.Core.Common.IFieldValue:  
    define public property Name  as character no-undo get.
    define public property Value as IPrimitiveHolder no-undo get.
end interface.