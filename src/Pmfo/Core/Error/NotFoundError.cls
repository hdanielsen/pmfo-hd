 
 /*------------------------------------------------------------------------
    File        : Error.NotFoundError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 11 13:11:13 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.NotFoundError inherits ApplicationError:
    define private variable cTxtTemplate as character init "&1 was not found &2" no-undo. 
    constructor public NotFoundError ( input pmsg as character ):        
        super (input GetMessage(pMsg)).  
    end constructor.
    
    constructor public NotFoundError ( input pmsg as character, pe as Error ):
        super (input GetMessage(pMsg),pe).          
    end constructor.
    
    constructor public NotFoundError ( input pmsg as character, pcContext as character ):
        super (GetMessage(pMsg,"in " + pcContext)).  
    end constructor.
    
    constructor public NotFoundError ( input pmsg as character, pcContext as character, pe as error ):
        super (GetMessage(pMsg,"in " + pcContext),pe).  
    end constructor.
    
    method protected character GetMessage(pcMessage as character):
        return right-trim(subst(cTxtTemplate,pcMessage)).
    end.   
    
    method protected character GetMessage(pcMessage as character,pcContext as character):
        return subst(cTxtTemplate,pcMessage,pcContext).
    end. 
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:NotFound.
    end method.    
    
    // override to remove the was not found - i.e use GetStrippedMessage as input to another  new NotFoundError. 
    method public override character GetStrippedMessage():
        define variable cMessage as character no-undo.
        cMessage = GetMessage(1).
        cMessage = replace(cMessage," was not found","").
        return cMessage. 
    end method.
    
end class.