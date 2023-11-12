
/*------------------------------------------------------------------------
    File        : ISignature
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Oct 15 14:01:59 EDT 2019
    Notes       : Used for overloads of IMethod
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.Common.IParameter from propath.

interface Pmfo.Core.Common.ISignature:  
    define public property Parameters  as IParameter extent no-undo get.
    method public integer GetParameterPosition(pcname as character).       
end interface.