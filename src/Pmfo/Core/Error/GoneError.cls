 
 /*------------------------------------------------------------------------
    File        : Error.GoneError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 10/19/2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Error.StackError from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.GoneError inherits StackError: 
    constructor public GoneError ( input pmsg as character ):
        super (input pmsg + " is gone").
    end constructor.
    
    constructor public GoneError ( input pmsg as character, pcExplain as character  ):
        super (input pmsg + subst(" is gone. &1",pcExplain)).
    end constructor.
    
    constructor public GoneError (  ):
        this-object(GetMethodName()).
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:Gone.
    end method.    
end class.