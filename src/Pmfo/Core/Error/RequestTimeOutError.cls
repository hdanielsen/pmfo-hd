/*------------------------------------------------------------------------
    File        : RequestTimeOutError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jul 17 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Stop from propath.

class Pmfo.Core.Error.RequestTimeOutError inherits ApplicationError: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    // Stop has Callstack , but inherits from Object and not Error
    define public property InnerStop as Stop no-undo get. protected set.
    
    define protected property MessageTemplate as character no-undo init "The &1 has timed out&2" get.
      
    constructor public RequestTimeOutError (pRequest as character):
        super (GetMessage(pRequest)).
    end constructor.
  
    constructor public RequestTimeOutError (pRequest as character, piTime as int):
        super (GetMessage(pRequest,piTime)).
    end constructor.
    
    constructor public RequestTimeOutError (pRequest as character, pInnerStop as Stop):
        super (GetMessage(pRequest,pInnerStop)).
        InnerStop = pInnerStop.
    end constructor.  
    
    method override protected StatusCodeEnum GetStatusCodeEnum(  ):
        return StatusCodeEnum:RequestTimeOut.
    end method. 
    
    // separated out to allow subclasses to override the request type/name of the message 
    method protected character GetRequestName(pRequest as char):
        return subst('&1 request',pRequest). 
    end method.
    
    // called from constructor to be passed to super - subclasses may override 
    method protected character GetMessage(pRequest as char):
        return GetMessage(pRequest,0). 
    end method.
    
    // called from constructor to be passed to super - subclasses may override 
    method protected character GetMessage(pRequest as char,pInnerStop as Stop):
        return GetMessage(pRequest,0).
    end method.
    
    // called from constructor to be passed to super - subclasses may override 
    method protected character GetMessage(pRequest as char, piTime as integer):
        define variable cMsgTime as character no-undo.
        if piTime > 0 then 
           cMsgTime = subst(". Maximum time allowed is &1 seconds.",piTime).
        return subst(MessageTemplate,GetRequestName(pRequest),cMsgTime). 
    end method.
end class.