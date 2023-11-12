 
 /*------------------------------------------------------------------------
    File        : ClientUpdateRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 31 16:33:22 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Service.ClientRequest from propath.
using Pmfo.Core.Service.IClientUpdateRequest from propath.
using Pmfo.Util.Array from propath.

class Pmfo.Core.Service.ClientUpdateRequest inherits ClientRequest implements IClientUpdateRequest: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    define public property ConfirmationsProp  as character init "confirmations"   no-undo get.  
    
    define public property Confirmations      as character     no-undo 
        get():
            return string(UriQuery:Get(ConfirmationsProp)).
            catch e as Progress.Lang.Error :
                undo, throw new BadRequestError(subst("&1=&2",ConfirmationsProp,string(UriQuery:Get(ConfirmationsProp))),e).    
            end catch.
        end. 
    
    define public property KeysOnly           as logical    no-undo get. protected set.
       
    constructor public ClientUpdateRequest (pMethod as char, pServiceURI as character, pURIQuery as IStringStringMap , plKeysonly as logical ):
        super (pMethod, pServiceURI,pURIQuery).
        KeysOnly = plKeysonly.
    end constructor.
    
    method protected override character GetUriQueryErrorText(pRequestType as RequestTypeEnum):
        return Method.  
    end method. 
    
    method override protected character extent GetValidProperties( input pRequestType as Pmfo.Core.BusinessLogic.RequestTypeEnum ):
        return Array:ToArray(ConfirmationsProp).
    end method.
    
   
end class.