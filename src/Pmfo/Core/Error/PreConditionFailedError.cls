 
 /*------------------------------------------------------------------------
    File        : Error.PreConditionFailedError
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

class Pmfo.Core.Error.PreConditionFailedError inherits StackError: 
    constructor public PreConditionFailedError ( input pmsg as character ):
        super (input pmsg).
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:PreconditionFailed.
    end method.    
end class.