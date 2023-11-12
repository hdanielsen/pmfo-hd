 
 /*------------------------------------------------------------------------
    File        : BadRequestError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Apr 04 10:30:12 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using Pmfo.Core.Error.NotFoundError from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.BadRequestError inherits ApplicationError: 
    
    // wrap not found    
    constructor public BadRequestError ( innErr as NotFoundError ):
        super (innErr:GetMessage(1),innErr).        
    end constructor.
    
    constructor public BadRequestError ( input pmsg as character ):
        super ("Bad Request: " + pmsg).        
    end constructor.
    
    constructor public BadRequestError ( input pmsg as character, piNum as integer ):
        super ("Bad Request: " + pmsg, piNum).        
    end constructor.
    
    constructor public BadRequestError ( input pmsg as character, input innErr as Progress.Lang.Error ):
        super ("Bad Request: " + pmsg, innErr).
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:BadRequest.
    end method.    

end class.