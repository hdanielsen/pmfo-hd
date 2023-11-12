 
 /*------------------------------------------------------------------------
    File        : OperationHandlerService
    Purpose     : Common OperationHandler methods
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Oct 12 18:33:06 EDT 2019
    Notes       : Split out of OperationHandler to allow override of common
                  beavior at application level for all OperationHandler 
                  subclasses
                - Basically decomposition to achieve multiple inheritance
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Net.HTTP.HttpHeader from propath.

block-level on error undo, throw.

class Pmfo.Core.Service.OperationHandlerService:
    
    constructor public OperationHandlerService (  ):
        super ().
    end constructor.
     
    method public HttpHeader extent GetOptionsHeaders  (pEntity as Object):
        define variable oHeaders as HttpHeader extent no-undo.
        return oHeaders.
    end method.
    
end class.