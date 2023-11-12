 
 /*------------------------------------------------------------------------
    File        : ValidationError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jun 16 23:19:39 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.

block-level on error undo, throw.

class Pmfo.Core.Error.ValidationError inherits ApplicationError: 
    constructor public ValidationError ():
        super ("Validation failed").
    end constructor.     
         
    constructor public ValidationError (pMsg as char ):
        super (pmsg).
    end constructor.
    
    constructor public ValidationError (pMsg as char, pe as Error ):
        super (pmsg,pe).
    end constructor. 
     
end class.