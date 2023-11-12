 
 /*------------------------------------------------------------------------
    File        : PasswordExpiredError
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

class Pmfo.Core.Error.PasswordExpiredError inherits ForbiddenRequestError: 
   
    constructor public PasswordExpiredError ():
        super (input "Password is expired",2).
    end constructor.

end class.