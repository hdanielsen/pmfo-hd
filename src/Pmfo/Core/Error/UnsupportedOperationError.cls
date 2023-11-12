 
 /*------------------------------------------------------------------------
    File        : Error.UnsupportedOperationError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 11 13:11:13 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.

block-level on error undo, throw.

class Pmfo.Core.Error.UnsupportedOperationError inherits ApplicationError: 
    constructor public UnsupportedOperationError ( input pmsg as character ):
        super (input "Unsupported operation: " + pmsg).
    end constructor.
end class.