 
 /*------------------------------------------------------------------------
    File        : OperationHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Oct 12 18:24:19 EDT 2019
    Notes       : Does NOT inherit NET lib OperationHandler     
----------------------------------------------------------------------*/
 

block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.DateTimeAddIntervalEnum from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.Logging.ILogWriter from propath.
using OpenEdge.Logging.ISupportLogging from propath.
using OpenEdge.Net.HTTP.HttpHeader from propath.
using OpenEdge.Net.HTTP.HttpHeaderBuilder from propath.
using OpenEdge.Net.HTTP.IHttpResponse from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Web.DataObject.DataObjectService from propath.
using OpenEdge.Web.DataObject.IOperationHandler from propath.
using OpenEdge.Web.DataObject.MappedOperation from propath.
using OpenEdge.Web.IWebRequest from propath.
using OpenEdge.Web.SendExceptionError from propath.
using OpenEdge.Web.WebResponseWriter from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.Catalog.ICatalog from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.Error.IHttpErrorStatus from propath.
using Pmfo.Core.Error.MethodNotAllowedError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.OperationHandlerService from propath.
using Pmfo.Util.JsonUtil from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.

class Pmfo.Core.Service.OperationHandler abstract implements IOperationHandler,ISupportLogging: 
 
    define public property Logger as ILogWriter no-undo get. set.        
    define public property ResponseNodeName as character no-undo init "response" get.
    define public property WarningEntityName      as character init "@warnings" no-undo get. 
    define public property MessageEntityName      as character init "@supervisorOverrides" no-undo get. 
    define public property QuestionEntityName     as character init "@questions" no-undo get. 
      
    define protected property OperationHandlerService as OperationHandlerService no-undo 
       get().
            if not valid-object(OperationHandlerService) then
                OperationHandlerService = cast(Application:ServiceManager:GetService(get-class(OperationHandlerService)),OperationHandlerService).
            return OperationHandlerservice.     
       end.
       set.
    
    constructor public OperationHandler (  ):
        super ().
    end constructor.
    
    // final  - override in OperationHandlerService
    method protected final HttpHeader extent GetOptionsHeaders  (pEntity as Object):
        define variable oHeaders as HttpHeader extent no-undo.
        return OperationHandlerService:GetOptionsHeaders(pEntity).
    end method.
    
    method protected integer ReturnStatusNoBody(poResponse as IHttpResponse,pStatusCodeEnum as StatusCodeEnum):
        define variable oStreamWriter as  WebResponseWriter no-undo.
        poResponse:StatusCode = integer(pStatusCodeEnum).
        assign oStreamWriter = new WebResponseWriter(poResponse).
        oStreamWriter:Open().
        return 0.
        finally:
            oStreamWriter:Close().
        end.
    end method.
    
    // final - override OperationHandlerService:GetOptionsHeaders
    method protected final integer WriteOptionsHeader(pEntity as Object, poResponse as IHttpResponse, pStatusCodeEnum as StatusCodeEnum):
        define variable oHeaders as HttpHeader extent no-undo.
        // poResponse:SetHeader(HttpHeaderBuilder:Build('Allow':u):Value("HEAD, OPTIONS, GET"):Header).
        oHeaders = GetOptionsHeaders(pEntity).
        if extent(oHeaders) > 0 then 
            //  oHeaders[2] = HttpHeaderBuilder:Build('Access-Control-Allow-Headers':u):Value("X-PSI-Authorization"):Header.
            poResponse:SetHeaders(oHeaders).
        return ReturnStatusNoBody(poResponse,pStatusCodeEnum).
    end method.  
    
    
    method protected JsonObject GetResponseWrapper(pjson as jsonObject, pcName as character):
        define variable oJsonResponse as JsonObject no-undo.
        define variable cNames        as character extent no-undo.
        define variable i             as integer no-undo.
        oJsonResponse = new jsonObject().
        // if the first buffer is the dataset name then replace it woth response
        cnames = pJson:GetNames().
        if pJson:Has(pcName)  then 
        do:
            if pJson:GetType(pcName) = JsonDataType:ARRAY then
                oJsonResponse:Add(ResponseNodeName,pJson:GetJsonArray(pcName)).
            else
                oJsonResponse:Add(ResponseNodeName,pJson:GetJsonObject(pcName)).
            
            JsonUtil:CopyNodes(pJson,oJsonResponse,subst("&1@",pcName),subst("&1@",responseNodeName),pcname).
               
        end.
        // else wrap it in response
        else do:
            oJsonResponse:Add(ResponseNodeName,pjson).  
        end.    
        return oJsonResponse.
    end method.
    
    method protected abstract character MethodsAllowedList(pObject as Object).
    method protected abstract integer WriteAllowedHeader(pObject as Object, poResponse as IHttpResponse, pStatusCodeEnum as StatusCodeEnum). 
    
    method public abstract JsonObject ExecutePostOrPutorDelete( 
                                       pcMethod                     as character,
                                       pMessageBody                 as Object,
                                       pcBaseAppTransportServiceURI as character,
                                       pcServiceURI                 as character,
                                       pURIQuery                    as IStringStringMap,
                                       pEntity                      as IEntity).
   
   method public abstract JsonObject ExecuteGetOrHead( 
                                       pcMethod as character,
                                       pcBaseAppTransportServiceURI as character,
                                       pcServiceURI as character,
                                       pURIQuery as IStringStringMap,
                                       pEntity as IEntity).                                    
    
    // allow subclass overide  
    method protected void SetRequest(poRequest as iWebRequest):
    end.
                       
    // final - implement ExecuteGetOrHead ExecutePostOrPutorDelete
    // if anything else need to be overridden split it into an overidable method                                       
    method public final integer Execute( input poRequest as IWebRequest, 
                                   input poMessageBody as Object, 
                                   input poResponse as IHttpResponse, 
                                   input pService as DataObjectService, 
                                   input poOperation as MappedOperation, 
                                   input poEntity as Object):
                                
   //     define variable oBusinessEntity       as IBusinessEntity       no-undo.
        define variable jsonResponse          as JsonObject            no-undo.   
        define variable cFullRootPath         as character             no-undo.
        define variable oHeader               as HttpHeader            no-undo.
        define variable lSetEntity            as logical               no-undo.
        define variable cNames                as character extent      no-undo.
        define variable iComNum               as integer               no-undo.
        // allow subclasses to override
        SetRequest(poRequest).
        
        if valid-object(poEntity) then do: 
            cFullRootPath = subst("&1&2&3/&4",poRequest:URI:BaseURI,poRequest:WebAppPath,poRequest:TransportPath,pService:name). 
            if type-of(poEntity,IEntity) or type-of(poEntity,ICatalog) then 
            do:
                case poRequest:Method:
                    when "OPTIONS" then
                    do on error undo, throw:
                        return WriteOptionsHeader(poEntity,poResponse,StatusCodeEnum:NoContent).
                    end.   
                    when "GET" or when "HEAD"  then do on error undo, throw:
                        jsonResponse  = ExecuteGetOrHead(poRequest:Method,
                                                         cFullRootPath,
                                                         poOperation:ServiceURI,
                                                         poRequest:URI:GetQueryMap(),
                                                         cast(poEntity,IEntity)).
                        if poRequest:Method = "HEAD" then                                                                                                                                            
                        do:                                                                                                                                                                          
                            return WriteAllowedHeader(cast(poEntity,IEntity),poResponse,StatusCodeEnum:NoContent).                                                                                          
                        end.
                    end.
                    when "POST" or when "PUT" or when "DELETE" then
                    do:
                              
                        jsonResponse  = ExecutePostOrPutorDelete(poRequest:Method,
                                                                 poMessageBody,
                                                                 cFullRootPath,
                                                                 poOperation:ServiceURI,
                                                                 poRequest:URI:GetQueryMap(),
                                                                 cast(poEntity,IEntity)).
                              
                    end.
                    otherwise 
                        undo, throw new MethodNotAllowedError(poRequest:Method).                            
                end case.
                
            end.
            else 
                 undo, throw new NotFoundError(subst("Resource '&1' for URI '&2'",poEntity:GetCLass():TypeName,poOperation:ServiceURI)).
            
                    
            // TODO - the textplain management is a mess... 
            // The entire logic should be elsewhere
            // CommNum should be retrieved from IClientRequest using ComNumProp for name   
            if valid-object(jsonResponse) then
            do:
                if poRequest:HasHeader("Accept") then
                do:
                    oHeader = poRequest:GetHeader("Accept").
                    
                    if oHeader:Value = "text/plain" then
                    do: 
                        poResponse:ContentType = "text/plain".
                        cnames = jsonResponse:GetNames().
                        iComNum = integer(poRequest:URI:GetQueryMap():Get("ComNum")) no-error. 
                        /** uncomment this if PLC improves error handling
                                   if error-status:get-number(1) = 76 then 
                                       undo, throw new IllegalOperationError("URL Query Option ComNum must be an integer"). 
                                   **/
                        poResponse:Entity = GetTextPlainResponse(jsonResponse,iComNum,'&').
                        lSetEntity = true.
                    end.
                end. 
                if not lSetEntity then
                do: 
                    jsonResponse:Add("@Context",poRequest:URI:ToString()).     
                    poResponse:Entity = jsonResponse.
                end.
            end.    
            else // No Content
            do:
                // TODO - the textplain management is a mess... 
                // maybe clientRequesats should be creeared here and passed into the Execute* (lots of unittest changes!)
                // or maybe they should rerurn object so that textppaln can be managed slighlty more controlled
                //    
                // The entire logic should be elsewhere
                // CommNum should be retrieved from IClientRequest using ComNumProp for name   
                if cast(Application:ServiceManager,IServiceManager):ServerType = ServerTypeEnum:TextPlain then
                do:
                    if poRequest:HasHeader("Accept") then
                    do:
                        oHeader = poRequest:GetHeader("Accept").
    
                        if oHeader:Value = "text/plain" then
                        do: 
                           iComNum = integer(poRequest:URI:GetQueryMap():Get("ComNum")) no-error. 
                           /** uncomment this if PLC improves error handling
                           if error-status:get-number(1) = 76 then 
                               undo, throw new IllegalOperationError("URL Query Option ComNum must be an integer"). 
                           **/
                           poResponse:ContentType = "text/plain". 
                          //  poResponse:Entity = JsonUtil:GetTextPlain(jsonResponse:GetJsonObject(cnames[1]), "&")
                           poResponse:Entity = GetTextPlainResponse(subst("HBeat=&1",string(interval(now, datetime(today), DateTimeAddIntervalEnum:Seconds:ToString()),'99999')), iComNum, "&").
                           return 0.
                        end.     
                    end.   
                end.      
                return ReturnStatusNoBody(poResponse,StatusCodeEnum:NoContent).
            end.
    
            return 0. 
        end. 
        else // missing entity is handled before we get here  - but just in case throw something 
            undo, throw new NotFoundError(subst("No entity found for service URI &1",poOperation:ServiceURI)).
        
        catch e as Progress.Lang.Error:
            HandleExecuteError(e, poEntity, poResponse).  //make sure to handle the error before attempting to log                 
        end catch.
     
    end method. /* Execute */                                        
    
    method protected String GetTextPlainResponse(pJson as JsonObject, piComNum as integer, pcDLm as character):
        define variable cNames as character extent no-undo.
        cnames = pJson:GetNames().
        return GetTextPlainResponse(JsonUtil:GetTextPlain(pJson:GetJsonObject(cnames[1]), pcDlm), piComNum, pcDLm).
    end.    
    
    method protected String GetTextPlainResponse(pcText as character, piComNum as integer, pcDLm as character):
        define variable i      as integer no-undo.
        define variable cEntry as character no-undo.
        define variable iEntries as integer no-undo.
        if piComNum > 0 then 
        do: 
            if index(subst("&1&2",pcDlm,pcText),subst("&1&2=",pcDlm,"ComNum")) = 0 then        
                pcText = subst("&1=&2&3&4","ComNum",piComnum,pcDLm,pcText).  
            else do:
                iEntries = num-entries(pcText,pcDlm).
                do i = 1 to iEntries:
                    cEntry = entry(i,pcText,pcDlm).
                    if cEntry begins "ComNum=" then 
                    do:
                        entry(i,pcText,pcDlm) = subst("&1=&2","ComNum",piComnum).
                        leave.
                    end.        
                end. 
            end.
        end. 
        return new String(pcText).   
    end.    
    
    method protected void HandleExecuteError(pError as Error,poEntity as Object,poResponse as IHttpResponse):
       define variable oErrorStatus as StatusCodeEnum no-undo.  
        
       /* The DataObjectHandler will return error with stack and inner error stack  if the type is SendExceptionError */  
                
/*         not tested - closes the stream and probably loses the body - maybe ok? for this?? */
/*            if type-of(e,MethodNotAllowedError) then                               */
/*            do:                                                                    */
/*              if valid-object(oEntity) then                                          */
/*                     WriteAllowedHeader(oEntity,poResponse,StatusCodeEnum:MethodNotAllowed).*/
/*            end.*/
        if type-of(pError,IHttpErrorStatus) then 
        do: 
            if cast(pError,IHttpErrorStatus):StatusCode = StatusCodeEnum:MethodNotAllowed then
                poResponse:SetHeader(HttpHeaderBuilder:Build('Allow':u):Value(MethodsAllowedList(poEntity)):Header).
            
            oErrorStatus = cast(pError,IHttpErrorStatus):StatusCode.
        end.
        else   
            oErrorstatus = StatusCodeEnum:InternalServerError.
        
        undo, throw new SendExceptionError(oErrorstatus,pError).            
    end.     
end class.