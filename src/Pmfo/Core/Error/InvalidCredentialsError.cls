 
 /*------------------------------------------------------------------------
    File        : Error.InvalidCredentialsError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Sep 01 09:14:41 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.UnauthorizedError.

block-level on error undo, throw.

class Pmfo.Core.Error.InvalidCredentialsError inherits UnauthorizedError: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable cMessage as character no-undo init "Access is denied due to invalid credentials".
    constructor public InvalidCredentialsError (pcMsg as character  ):
        super (subst("&1. &2.",cMessage,right-trim(pcMsg,"."))).
    end constructor.
         
    constructor public InvalidCredentialsError (  ):
        super (cMessage).
    end constructor.

end class.