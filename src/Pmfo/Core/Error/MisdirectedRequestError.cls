 
 /*------------------------------------------------------------------------
    File        : Error.MisdirectedRequestError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : February 29 2020
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Error.StackError from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.MisdirectedRequestError inherits StackError: 
    constructor public MisdirectedRequestError ( input pmsg as character ):
        super (input pmsg).
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:MisdirectedRequest.
    end method.    
end class.