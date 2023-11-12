
/*------------------------------------------------------------------------
    File        : IJsonReadHandlerProvider
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Sep 13 12:03:07 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.BusinessLogic.IJsonReadHandler from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.

interface Pmfo.Core.BusinessLogic.IJsonReadHandlerProvider:  
    method IJsonReadHandler GetJsonReadHandler(pcMethod as character, poAction as IAction).
end interface.