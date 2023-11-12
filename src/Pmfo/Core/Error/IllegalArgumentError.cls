 
 /*------------------------------------------------------------------------
    File        : Error.IllegalArgumentError
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

class Pmfo.Core.Error.IllegalArgumentError inherits ApplicationError: 
    constructor public IllegalArgumentError ( input pmsg as character ):
        super (input "Illegal argument: " + pmsg).
    end constructor.
    constructor public IllegalArgumentError ( input pmsg as character, pe as Error ):
        super (input "Illegal argument: " + pmsg,pe).
    end constructor.
end class.