 
 /*------------------------------------------------------------------------
    File        : IllegalStateError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Apr 19 8:24 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.

block-level on error undo, throw.

class Pmfo.Core.Error.IllegalStateError inherits ApplicationError: 
    constructor public IllegalStateError ( input pmsg as character ):
        super (input "Illegal state: " + pmsg).
    end constructor.
    constructor public IllegalStateError ( input pmsg as character, pe as Error ):
        super (input "Illegal state: " + pmsg,pe).
    end constructor.
end class.