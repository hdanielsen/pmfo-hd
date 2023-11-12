
/*------------------------------------------------------------------------
    File        : IRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Sep 21 00:55:43 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.

interface Pmfo.Core.BusinessLogic.IRequest:  
    define public property RequestType         as RequestTypeEnum no-undo get. set. 
    define public property MainRequestedBuffer as character no-undo get. set.
    method public Progress.Reflect.Method GetClientMethod(pObject as Object, phDataset as handle,output pClientParameterList as ParameterList):
    
end interface.