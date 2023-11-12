 
 /*------------------------------------------------------------------------
    File        : ClientServiceUpdateRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 31 16:33:22 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.Service.ClientRequest from propath.
using Pmfo.Core.Service.IClientServiceUpdateRequest from propath.

class Pmfo.Core.Service.ClientServiceUpdateRequest inherits ClientRequest implements IClientServiceUpdateRequest: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    constructor public ClientServiceUpdateRequest (pMethod as char, pServiceURI as character, pURIQuery as IStringStringMap  ):
        super (pMethod, pServiceURI,pURIQuery).
    end constructor.
    
end class.