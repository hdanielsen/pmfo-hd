 
 /*------------------------------------------------------------------------
    File        : Error.OutOfBoundsError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     :  
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.

block-level on error undo, throw.

class Pmfo.Core.Error.OutOfBoundsError inherits ApplicationError: 
    constructor public OutOfBoundsError ( input piValue as integer ):
        super (input subst("Out of bounds: &1",piValue)).
    end constructor.
    
    constructor public OutOfBoundsError ( input piValue as integer, pe as Error ):
        super (input subst("Out of bounds: &1",piValue),pe).
    end constructor.
    
    constructor public OutOfBoundsError ( input pcLabel as character, piValue as integer):
        super (input subst("Out of bounds: &1 &2",pcLabel, piValue)).
    end constructor.
end class.