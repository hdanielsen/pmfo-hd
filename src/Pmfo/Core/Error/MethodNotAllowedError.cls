 
 /*------------------------------------------------------------------------
    File        : Error.MethodNotAllowedError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 1 11:20 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Net.HTTP.MethodEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.MethodNotAllowedError inherits ApplicationError: 
    constructor public MethodNotAllowedError ( input pMethod as character ):
        super (input pMethod + " method is not allowed").  
    end constructor.
    
    constructor public MethodNotAllowedError ( input pMethod as character, pcResource as character ):
        super (input pMethod + " method is not allowed for " + pcResource).  
    end constructor.
    
    constructor public MethodNotAllowedError ( input pMethod as MethodEnum ):
        this-object (input pMethod:ToString()).  
    end constructor.
    
    constructor public MethodNotAllowedError ( input pMethod as MethodEnum, pcResource as character):
        this-object (input pMethod:ToString(),pcResource).
    end constructor.
    
    constructor public MethodNotAllowedError ( input pMethod as character, pcResource as character, pcChild as character  ):
        super (input subst("&1 method is not allowed for &2(...)/&3",pMethod,pcResource,pcChild)).  
    end constructor.

    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:MethodNotAllowed.
    end method.    
    
end class.