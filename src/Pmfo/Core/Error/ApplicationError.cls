/*------------------------------------------------------------------------
    File        : ApplicationError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 2019
    Notes       : Extends AppError with InnerError and hides the problematic 
                  AppError constructor for ReturnValue 
  ----------------------------------------------------------------------*/
/* error in error  */
block-level on error undo, throw.

using Progress.Lang.AppError from propath.
using Progress.Lang.Error from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using OpenEdge.Core.StringConstant from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using Pmfo.Core.Error.IHttpErrorStatus from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Core.Error.ApplicationError inherits AppError implements IHttpErrorStatus: 
    define protected property MessageNodeName as char init "errorMsg" no-undo get.
    define protected property MessageNumNodeName as char init "errorNum" no-undo get.
    define public property InnerError as Error no-undo 
        get.
        protected set.
    
    define public property StatusCode as StatusCodeEnum no-undo 
        get():
            if not valid-object(StatusCode)then
                StatusCode = GetStatusCodeEnum().
            return StatusCode.    
        end. 
        private set.
         
    constructor public ApplicationError (pmsg as char):   
        super (pmsg,?). // use the error constructor
    end constructor.     
    
    constructor public ApplicationError (pmsg as char, piNum as integer):   
        super (pmsg,pinum).  
    end constructor.     
    
    constructor public ApplicationError (pmsg as char, innErr as Error):   
        this-object(pmsg).
        InnerError = innErr.
    end constructor.     
    
    constructor public ApplicationError (pmsg as char, piNum as integer, innErr as Error):   
        this-object(pmsg,piNum).
        InnerError = innErr.
    end constructor.     
    
    // allow override of Statuscode property - called once only 
    method protected StatusCodeEnum GetStatusCodeEnum():
        return StatusCodeEnum:InternalServerError.
    end method.
              
    // returns a string of all messages including returnvalue 
    method public override character ToString():
        define variable i        as integer no-undo.
        define variable cMessage as character no-undo.
        cMessage = ReturnValue.
        do i = 1 to NumMessages:
           cMessage =  cMessage 
                    + (if cMessage > "" then StringConstant:LF else "")
                    + GetMessage(i).  
        end.    
        return cMessage.
    end.    
    
    // used to get the message without the leading text added by many classes
    // use in cases where you ant to wrap themessage and also show the reason
    method public character GetStrippedMessage():
        define variable iColon as integer no-undo.
        define variable cMessage as character no-undo.
        cMessage = GetMessage(1).
        iColon = index(cMessage,":").
        if iColon > 0 then
            return trim(substr(cMessage,iColon + 1)).
        return cMessage. 
    end method.
    
    method public JsonObject GetError (pindex as integer):
        define variable oError as JsonObject no-undo.  
        assign oError = new JsonObject().  
        oError:Add(MessageNodeName, GetMessage(pindex)).
        oError:Add(MessageNumNodeName, GetMessageNum(pindex)).
        return oError.
    end method.
/*                                                                        */
/*    // return a string of all messages in an error including returnvalue*/
/*    method public static character GetMessage(pe as Error):             */
/*        define variable i        as integer no-undo.                    */
/*        define variable cMessage as character no-undo.                  */
/*        if type-of(pe,ApplicationError) then                            */
/*            return pe:ToString().                                       */
/*                                                                        */
/*        if type-of(pe,AppError) then                                    */
/*              cMessage = cast(pe,Apperror):ReturnValue.                 */
/*        do i = 1 to pe:NumMessages:                                     */
/*           cMessage =  cMessage                                         */
/*                    + (if cMessage > "" then StringConstant:LF else "") */
/*                    + pe:GetMessage(i).                                 */
/*        end.                                                            */
/*        return cMessage.                                                */
/*    end.                                                                */
/*                                                                        */
end class.
 
