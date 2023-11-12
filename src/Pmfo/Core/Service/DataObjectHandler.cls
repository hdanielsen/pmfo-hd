 
 /*------------------------------------------------------------------------
    File        : DataObjectHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Mar 11 17:17:12 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Assert from propath.
using OpenEdge.Logging.Logger from propath.
using OpenEdge.Net.HTTP.IHttpResponse from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Web.DataObject.DataObjectHandler from propath.
using OpenEdge.Web.DataObject.DataObjectService from propath.
using OpenEdge.Web.DataObject.HandlerErrorEventArgs from propath.
using OpenEdge.Web.DataObject.MappedOperation from propath.
using OpenEdge.Web.IWebRequest from propath.
using OpenEdge.Web.SendExceptionError from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

// NOTE does not worj with using

class Pmfo.Core.Service.DataObjectHandler inherits DataObjectHandler: 
    
    define protected property PingEntity  as character no-undo init "ping" get. 
    
      /* Default constructor*/
    constructor public DataObjectHandler():
        super().
    end constructor.
    
    // This calls performOperation, so we would need a full override, but it is not needed ao juast rhrow not implemented
    method protected override integer ListServices(input poOperation as MappedOperation,
                                          input poRequest  as IWebRequest,
                                          input poMessageBody as Progress.Lang.Object,
                                          input poResponse as IHttpResponse):
      undo, throw new SendExceptionError(StatusCodeEnum:NotImplemented,"ListServices is not implemented").
    end method.
    
    // override to skip logging of errors
    method protected override integer HandleException(
                                             input poError    as Progress.Lang.Error,
                                             input poRequest  as IWebRequest,
                                             input poResponse as IHttpResponse,
                                             input poBusinessEntity as Progress.Lang.Object,
                                             input pService as DataObjectService,
                                             input poOperation as MappedOperation):
        define variable oArgs as HandlerErrorEventArgs no-undo.
        
        Assert:NotNull(poRequest,  'Web request').
        Assert:NotNull(poError, 'Error').
        
        /** we do not want thisd
        Logger:Error(substitute('Exception raised for &1 &2; Entity=&3; Operation=&4',
                              poRequest:Method,
                              poRequest:URI:ToString(),
                              (if valid-object(poBusinessEntity) then poBusinessEntity:ToString() else '':u),
                              (if valid-object(poOperation) then poOperation:ToString() else '':u)),
                      poError).
        **/
        // We'll need a response
        if not valid-object(poResponse) then
            assign poResponse = CreateDefaultResponse().
        
        assign oArgs                = new HandlerErrorEventArgs(poRequest, poResponse, poError)
               // don't pass the BusinessEntity in the constructor, since that validates for nulls  
               oArgs:BusinessEntity = poBusinessEntity
               .
        oArgs:SetService(pService).
        oArgs:SetOperation(poOperation).
        
        OnOperationError(oArgs).
        
        /* a non-zero value means that we will use the static pages */
        if oArgs:ReturnStatusCode eq 0 then
        do:
            /* is there an envelope? */
            if     valid-object(poOperation) 
               and poOperation:Options:HasErrorEnvelope
            then
                AddResponseEnvelope(poResponse, poOperation, poOperation:Options:ErrorEnvelope).
            
            WriteResponse(oArgs:Response).
        end.    
        
        return oArgs:ReturnStatusCode.
        catch oError as Progress.Lang.Error:
            /* Something went wrong while processing errors */
            Logger:Error('Error handling exceptions: using default status code 500/Internal Server Error', oError).
            
            return integer(StatusCodeEnum:InternalServerError).
        end catch.
    end method.
    
    method protected override void OnOperationError(input poEventArgs as HandlerErrorEventArgs):
        define variable cConfirmations as character no-undo.
        // @TODO aubscribe on the event from somewhere ?    
        if type-of(poEventArgs:Response:Entity,JsonObject) then
        do:
             cConfirmations = poEventArgs:Request:URI:GetQueryMap():Get("confirmations").
             if cConfirmations > "" then
                 cast(poEventArgs:Response:Entity,JsonObject):Add("@Confirmations",cConfirmations).
             cast(poEventArgs:Response:Entity,JsonObject):Add("@Context",poEventArgs:Request:URI:ToString()).     
        end.
        super:OnOperationError(poEventArgs).
    end method.
                                                    
    destructor DataObjectHandler():
        Destroy().
    end destructor.
    
end class.