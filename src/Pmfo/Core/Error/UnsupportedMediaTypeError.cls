 
 /*------------------------------------------------------------------------
    File        : UnsupportedMediaTypeError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Sep 24 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.UnsupportedMediaTypeError inherits ApplicationError: 

    constructor public UnsupportedMediaTypeError ( input pmsg as character ):
        super ("Unsupported Media Type: " + pmsg).        
    end constructor.
    
    constructor public UnsupportedMediaTypeError ( ):
        super ("Unsupported Media Type").        
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:UnsupportedMediaType.
    end method.    

end class.