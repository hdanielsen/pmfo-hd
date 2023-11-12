 
 /*------------------------------------------------------------------------
    File        : UnprocessableEntityError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Jun 14 21:11:54 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.UnprocessableEntityError inherits ApplicationError:         
    constructor public UnprocessableEntityError ( input pmsg as character ):
        super (input "Unprocessable Entity: " + pmsg).
    end constructor.

    constructor public UnprocessableEntityError ( input pmsg as character, input innErr as Progress.Lang.Error ):
        super (input  "Unprocessable Entity: " + pmsg, input innErr).
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:UnprocessableEntity.
    end method.   
     
end class.