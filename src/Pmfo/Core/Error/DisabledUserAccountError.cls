 
 /*------------------------------------------------------------------------
    File        : DisabledUserAccountError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Sep 24 08:11:42 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ForbiddenRequestError.

block-level on error undo, throw.

class Pmfo.Core.Error.DisabledUserAccountError inherits ForbiddenRequestError: 
   
    constructor public DisabledUserAccountError ():
        super (input "User Account is Disabled",3).
    end constructor.

end class.