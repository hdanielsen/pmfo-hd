 
 /*------------------------------------------------------------------------
    File        : Error.ForbiddenRequestError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sep 17 2019
    Notes       : We enforce error numbers for 403 so that the client can 
                  detect expired password 
                  1 - is this class without message
                  2 - for subclass PasswordExpiredError 
                  3 - for subclass DisabledUserAccountError
                  4 - crud or action not allowed 
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.Error.ApplicationError from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.ForbiddenRequestError inherits ApplicationError: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    constructor protected ForbiddenRequestError (pcMsg as character, piNum as integer ):
        super (pcMsg,piNum).
    end constructor.
         
    constructor public ForbiddenRequestError (  ):
        super ("Forbidden request",1).
    end constructor.
    
    constructor public ForbiddenRequestError (pcMessage as char ):
        super ("Forbidden request: " + pcMessage ,4).
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:Forbidden.
    end method.    
    
    
end class.