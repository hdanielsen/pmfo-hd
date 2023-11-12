 
 /*------------------------------------------------------------------------
    File        : RefreshError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Aug 08 09:24:35 EDT 2019
    Notes       : Dedicated error for errors occurring after a successful transaction
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.RefreshError inherits ApplicationError: 

    constructor public RefreshError ( pError as Error):
        super (input "Data was successfully saved, but the saved data was not returned due to error during refresh.", pError).
    end constructor.
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:Accepted.
    end method. 
 
end class.