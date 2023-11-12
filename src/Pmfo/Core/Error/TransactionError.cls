 
 /*------------------------------------------------------------------------
    File        : FieldTransactionError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 10 13:36:47 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.

block-level on error undo, throw.

class Pmfo.Core.Error.TransactionError inherits ApplicationError: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    constructor public TransactionError ():
        super ("Transaction failed").
    end constructor.     
         
    constructor public TransactionError (pMsg as char ):
        super (pmsg).
    end constructor.
    
    constructor public TransactionError (pMsg as char, pe as Error ):
        super (pmsg,pe).
    end constructor. 

end class.