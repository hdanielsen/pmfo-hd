 
 /*------------------------------------------------------------------------
    File        : Error.IllegalOperationError
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

class Pmfo.Core.Error.IllegalOperationError inherits ApplicationError: 
    constructor public IllegalOperationError ( input pmsg as character ):
        super (input "Illegal operation: " + pmsg).
    end constructor.
    
    constructor public IllegalOperationError ( input pmsg as character, pe As Error ):
        super (input "Illegal operation: " + pmsg,pe).
    end constructor.
    
end class.