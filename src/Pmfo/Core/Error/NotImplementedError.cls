 
 /*------------------------------------------------------------------------
    File        : Error.NotImplementedError
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
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Error.StackError from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.NotImplementedError inherits StackError: 
    constructor public NotImplementedError ( input pmsg as character ):
        super (input pmsg + " is not implemented").
    end constructor.
    
    constructor public NotImplementedError (  ):
        this-object(GetMethodName()).
    end constructor.
    
    method protected override character GetMethodName():
        return GetMethodName(4).    
    end method.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:NotImplemented.
    end method.    
end class.