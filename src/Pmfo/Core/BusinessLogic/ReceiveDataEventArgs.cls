 
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
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.

class Pmfo.Core.BusinessLogic.ReceiveDataEventArgs implements IReceiveDataEventArgs: 
 
    define public property Request as IGetDataRequest no-undo 
    get.
    protected set. 

    define public property Response as IGetDataResponse no-undo  
    get.
    protected set. 
    
    define public property UpdateRequest as IUpdateDataRequest no-undo 
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
    
    // used for refresh for update - specifcally added for ClientBusnessEntity that rarely is included in an update, 
    // but there may be a problem since the data may have been created already 
    constructor public ReceiveDataEventArgs ( pRequest as IGetDataRequest, pResponse as IGetDataResponse, pUpdateRequest as IUpdateDataRequest, plError as logical):
        this-object(pRequest,pResponse,plError).
        UpdateRequest = pUpdateRequest.
    end constructor.
    


end class.