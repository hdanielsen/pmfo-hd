using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Net.HTTP.MethodEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.ControllerMethodNotImplemented inherits ApplicationError: 
    constructor public ControllerMethodNotImplemented ( controllerName as character, methodName as character ):
        super (input substitute("Method &1 on controller &2 not implemented", methodName, controllerName)).  
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:NotImplemented.
    end method.    
    
end class.