 
 /*------------------------------------------------------------------------
    File        : ReceiveTableEventArgs
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Jul 27 11:33:25 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.IGetDataRequest   from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse  from propath.
using Pmfo.Core.BusinessLogic.IReceiveDataEventArgs from propath.

class Pmfo.Core.BusinessLogic.ReceiveDataEventArgs implements IReceiveDataEventArgs: 
 
    define public property Request as IGetDataRequest no-undo 
    get.
    protected set. 

    define public property Response as IGetDataResponse no-undo  
    get.
    protected set. 
    
    define public property Failed       as logical          no-undo get. set.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    constructor public ReceiveDataEventArgs ( pRequest as IGetDataRequest, pResponse as IGetDataResponse, plError as logical):
        this-object(pRequest,pResponse).
        Failed = plError.
    end constructor.
        
    constructor public ReceiveDataEventArgs ( pRequest as IGetDataRequest, pResponse as IGetDataResponse):
        super ().
        Request      = pRequest.
        Response     = pResponse.
    end constructor.

end class.