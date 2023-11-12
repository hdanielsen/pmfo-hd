 
 /*------------------------------------------------------------------------
    File        : Error.UnauthorizedError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 11 13:11:13 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.UnauthorizedError inherits ApplicationError:
    
    constructor protected UnauthorizedError ( input pmsg as character ):
        super (pMsg).  
    end constructor.
    
    // only use if error handler somehow can block cause in production
    constructor protected UnauthorizedError ( input pmsg as character, pe as Error ):
        super (pMsg, pe).  
    end constructor.
    
    // only use if error handler somehow can block cause in production
    constructor protected UnauthorizedError ( pe as Error ):
        super ("Unauthorized request", pe).  
    end constructor.
    
    constructor public UnauthorizedError (  ):
        super ("Unauthorized request").  
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:Unauthorized.
    end method.    
    
end class.