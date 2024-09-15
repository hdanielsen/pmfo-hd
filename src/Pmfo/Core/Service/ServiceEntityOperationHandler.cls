 
 /*------------------------------------------------------------------------
    File        : CrudOperationHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Mar 21 09:10:09 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Memptr from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.Logging.ISupportLogging from propath.
using OpenEdge.Net.HTTP.HttpHeader from propath.
using OpenEdge.Net.HTTP.HttpHeaderBuilder from propath.
using OpenEdge.Net.HTTP.IHttpResponse from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Web.DataObject.DataObjectService from propath.
using OpenEdge.Web.DataObject.IOperationHandler from propath.
using OpenEdge.Web.DataObject.MappedOperation from propath.
using OpenEdge.Web.IWebRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.BusinessLogic.Parser.JsonParameterParser from propath.
using Pmfo.Core.BusinessLogic.Parser.MethodParser from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.MethodNotAllowedError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.UnprocessableEntityError from propath.
using Pmfo.Core.Error.UnsupportedMediaTypeError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.ClientServiceGetRequest from propath.
using Pmfo.Core.Service.ClientServiceUpdateRequest from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Core.Service.IClientServiceGetRequest from propath.
using Pmfo.Core.Service.IClientServiceUpdateRequest from propath.
using Pmfo.Core.Service.OperationHandler from propath.
using Pmfo.Util.JsonUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.AppError from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
using Progress.Reflect.Property from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
 

class Pmfo.Core.Service.ServiceEntityOperationHandler inherits OperationHandler implements IOperationHandler,ISupportLogging: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    constructor public ServiceEntityOperationHandler (  ):
        super ().        
    end constructor.
    
    /* public for unit test - not in interface
        handle  GET or HEAD request 
       The head request returns after url and query has been processed and validated                               
    */                                          
    method public override JsonObject ExecuteGetOrHead
                                          (pcMethod as character,
                                           pcBaseAppTransportServiceURI as character,
                                           pcServiceURI as character,
                                           pURIQuery as IStringStringMap,
                                           pEntity as IEntity):
                                               
        define variable oServiceEntity  as IServiceEntity        no-undo.
        if type-of(pEntity,IServiceEntity) then 
             oServiceEntity = cast(pEntity,IServiceEntity).
        else     
            undo, throw new IllegalArgumentError(subst("Entity of Type &1",pEntity:GetClass():TypeName)).
        
        return ExecuteService(pcMethod, 
                              ?,
                              pcBaseAppTransportServiceURI,
                              pcServiceURI, 
                              pURIQuery ,
                              oServiceEntity).                                     
    
    end method.  
    
     // public for unit test - not in interface
    method public override JsonObject ExecutePostOrPutOrDelete
                                  (pcMethod as character,
                                   pMessageBody as Object,
                                   pcBaseAppTransportServiceURI as character,
                                   pcServiceURI as character,
                                   pURIQuery as IStringStringMap,
                                   pEntity as IEntity):
        
        define variable oServiceEntity  as IServiceEntity        no-undo.
        if type-of(pEntity,IServiceEntity) then 
             oServiceEntity = cast(pEntity,IServiceEntity).
        else     
            undo, throw new IllegalArgumentError(subst("Entity of Type &1",pEntity:GetClass():TypeName)).
                                       
        return ExecuteService(pcMethod, 
                              pMessageBody,
                              pcBaseAppTransportServiceURI,
                              pcServiceURI, 
                              pURIQuery ,
                              oServiceEntity).
    
    end method.                                             
    
    // public for unit testing not part of interface
    method public JsonObject ExecuteService
                                  (pcMethod as character,
                                   pMessageBody as Object,
                                   pcBaseAppTransportServiceURI as character,
                                   pcServiceURI as character,
                                   pURIQuery as IStringStringMap,
                                   pServiceEntity as IServiceEntity):
                                       
        define variable oJson       as JsonObject no-undo.
        define variable oJsonOutput as JsonObject no-undo.
        define variable oEntityJson as JsonObject no-undo.
        
        define variable oJsonRequest as JsonObject no-undo.
        
        define variable oProperties as Property no-undo extent. 
        define variable oClientServiceRequest as IClientRequest no-undo. 
        define variable i as integer no-undo.
        define variable lFound as logical no-undo.
        define variable oPredicate as IQueryPredicate no-undo.
        define variable cNames as character extent no-undo.
        define variable oMethodParser as MethodParser no-undo.
        define variable oParameterList as ParameterList no-undo.
        define variable cReturn as character no-undo. 
        define variable oError  as AppError  no-undo.
        define variable iRequestedProperty as integer no-undo.
        define variable oJsonParameterParser as JsonParameterParser no-undo.
        
        if pcMethod = "GET" or pcMethod = "HEAD" then
            oClientServiceRequest  = CreateClientServiceGetRequest(pcMethod, pcServiceURI,pURIQuery).
        else 
            oClientServiceRequest  = CreateClientServiceUpdateRequest(pServiceEntity,pcMethod, pcServiceURI,pURIQuery,pMessageBody).
           
        if extent(oClientServiceRequest:Collections) > 2 then 
        do: 
            undo, throw new NotFoundError(pcServiceURI).
        end.
         
        oClientServiceRequest:AssertUriQuery(RequestTypeEnum:SingleRow).
        
        if pcMethod = "HEAD" then
            return ?.
        
        case pcMethod:
            when "DELETE" then do:
                if not pServiceEntity:CanDelete then
                    undo, throw new MethodNotAllowedError(pcMethod,pServiceEntity:Resource).
            end.
            
            when "PUT" then do:
                if not pServiceEntity:CanUpdate then
                    undo, throw new MethodNotAllowedError(pcMethod,pServiceEntity:Resource).
            end.
            when "POST" then do:
                if not pServiceEntity:CanCreate then
                    undo, throw new MethodNotAllowedError(pcMethod,pServiceEntity:Resource).
            end.
            when "GET" then do:
                if not pServiceEntity:CanRead then
                    undo, throw new MethodNotAllowedError(pcMethod,pServiceEntity:Resource).
            end.
            otherwise 
                undo, throw new IllegalArgumentError(subst("Method '&1' for service &2",pcMethod,pServiceEntity:Resource)).
        end case.
             
        // hook for subclasses
        ApplyServiceRequestProperties(oClientServiceRequest,pServiceEntity).
         // we are allowing parameters on POST as long as there is no body (if you need both use BusinessEntity to define the tt for the body)
        if pcMethod <> "GET" 
        and oClientServiceRequest:KeyWheres[extent(oClientServiceRequest:KeyWheres)] > "" 
        and pServiceEntity:AllowAction 
        and IsBodyEmpty(pMessageBody) = false then
            undo, throw new UnsupportedOperationError(subst("Passing both action parameters and body to &1 service with '&2'",pServiceEntity:Resource,pcMethod)).
        
        if extent(oClientServiceRequest:Collections) = 1 then
        do: 
            if pcMethod <> "GET" 
            and extent(pServiceEntity:Parameters) <> ? 
            and oClientServiceRequest:KeyWheres[1] = "" then
            do:
                if not valid-object(pMessageBody) then
                    undo, throw new UnprocessableEntityError(subst("The request for '&1' must have a json body",pServiceEntity:Resource)).
                if type-of(pMessageBody,JsonArray) then
                    undo, throw new UnprocessableEntityError(subst("The body for '&1' must be a json object not a json array",pServiceEntity:Resource)).
                
                if type-of(pMessageBody,JsonObject) then
                do: 
                    if not valid-object(pServiceEntity:Method) then 
                        undo, throw new IllegalOperationError(subst("'&1' of &2 service with no method",pcMethod,pServiceEntity:Resource)).
            
                    oJsonRequest = cast(pMessageBody,JsonObject).
                     
                    if not oJsonRequest:Has(pServiceEntity:Resource) then 
                        undo, throw new UnprocessableEntityError(subst("The entity body must have a top node named '&1'",pServiceEntity:Resource)).
                    if oJsonRequest:GetType(pServiceEntity:Resource) <> JsonDataType:Object then 
                        undo, throw new UnprocessableEntityError(subst("The '&1' node is not an object",pServiceEntity:Resource)).
                    
                    oJsonRequest = oJsonRequest:GetJsonObject(pServiceEntity:Resource).
                    oJsonParameterParser = new JsonParameterParser(pServiceEntity:Method,pServiceEntity:Parameters,pServiceEntity:Resource).
                    
                    oParameterList = oJsonParameterParser:GetParameterList(oJsonRequest).
                    pServiceEntity:Method:Invoke(pServiceEntity,oParameterList).
                    
                end.
                else do:
                    
                    if IsBodyEmpty(pMessageBody) then
                    do:
                        undo, throw new UnsupportedOperationError(subst("&1 &2 with no body&3",
                                                                        pcMethod,
                                                                        pServiceEntity:Resource,
                                                                        if pServiceEntity:AllowAction then " or action parameters" else "")).
                    end.
                    undo, throw new UnsupportedMediaTypeError("Expected media type is application/json").
                end.  
            end.
            else 
            do: 
                oMethodParser = new MethodParser().
                oJson = oMethodParser:Invoke(pServiceEntity, oClientServiceRequest, ResponseNodeName).
            end.
        end.
        else do: 
            // check if this is a property request 
            if pcMethod = "GET" and oClientServiceRequest:KeyWheres[2] = "" then
            do:
                oProperties = pServiceEntity:Properties.
                if extent(oProperties) > 0 then
                do i = 1 to extent(oProperties): 
                    if oProperties[i]:Name = oClientServiceRequest:Collections[2] then
                    do:
                        iRequestedProperty = i.
                        leave.
                    end.
                end.
            end.
            if iRequestedProperty = 0 then
            do: 
                oMethodParser = new MethodParser().
                oJson = oMethodParser:Invoke(pServiceEntity, oClientServiceRequest, ResponseNodeName).
            end.         
        end.        
         
        // we may already have oJson response if method and 2 collections
        if not valid-object(oJson) then
        do: 
            oJson = new JsonObject().
            oEntityJson = new JsonObject().
            oProperties = pServiceEntity:Properties.
            
            if extent(oProperties) > 0 then
            do i = if iRequestedProperty > 0 then iRequestedProperty else 1
                   to
                   if iRequestedProperty > 0 then iRequestedProperty else extent(oProperties): 
                
                if oProperties[i]:Extent = 0 then 
                    AddPropertyValueToJson(pServiceEntity, oProperties[i]:Name, oProperties[i]:DataType, oEntityJson).
                else
                    AddPropertyExtentValueToJson(pServiceEntity, oProperties[i]:Name, oProperties[i]:DataType, oEntityJson).
            end. 
            else do:
                //if extent(oClientServiceRequest:Collections) > 1 then
                // if something was executed in an object with no properties return ? which means 204 no content 
                return ?. 
                             
            end.             
            oJson:Add(ResponseNodeName,oEntityJson).
        end. // not valid ojson
        
        if valid-object(oJson) then
            AddServiceEntityProperties(oJson, pServiceEntity, oClientServiceRequest ). 
        
        if valid-object(pServiceEntity:WarningMessages) and pServiceEntity:WarningMessages:HasData then
        do:
            oJson:Add(WarningEntityName,pServiceEntity:WarningMessages:ToJsonArray()).
        end.  
        
        return oJson.        
        
        catch e as Progress.Lang.Error :
            // this is not likely to happen after the introduction of MethodParser 
            if e:GetMessageNum(1) = 15313 
            and (valid-object(pServiceEntity:Method) or extent(oClientServiceRequest:Collections) > 1) then
            do:
                // incorrect url is 404, but add the readon as second message. 
                oError = new NotFoundError(pcServiceURI,e).
                oError:AddMessage(subst("One or more parameters is missing for &1 &2",
                                        if pcMethod = "GET" then "Function" else "Action",
                                        oClientServiceRequest:Collections[extent(oClientServiceRequest:Collections)]
                                  ),
                                  ?).
                                        
                undo, throw oError.                        
            end.                                                 
            undo, throw e.    
        end catch. 
    end method.
    
    method protected void AddPropertyValueToJson(pServiceEntity as IServiceEntity,
                                                 pName as character,
                                                 pDataType as Progress.Reflect.DataType,
                                                 pJson as JsonObject):
        define variable cVal as character no-undo.
        define variable iVal as integer no-undo.
        define variable i64val as int64 no-undo.
        define variable decVal as decimal no-undo.
        define variable datVal as date no-undo.
        define variable dtiVal as datetime no-undo.
        define variable dtzVal as datetime-tz no-undo.
        define variable lval as logical no-undo.
        define variable longval as longchar no-undo.    
                             
        // note passing dynamic-property directly to Json:ADD causes two calls 
        case pDataType:
            when Progress.Reflect.DataType:Character then do:
                cVal =  dynamic-property(pServiceEntity,pName).
                pJson:Add(pName,cVal).
            end.    
            when Progress.Reflect.DataType:Date then do:
                datVal =  dynamic-property(pServiceEntity,pName).
                pJson:Add(pName,datVal).
            end.    
            when Progress.Reflect.DataType:Datetime then do:
                dtiVal =  dynamic-property(pServiceEntity,pName).
                pJson:Add(pName,dtiVal).
            end.    
            when Progress.Reflect.DataType:DateTimeTZ then do:
                dtzVal =  dynamic-property(pServiceEntity,pName).
                pJson:Add(pName,dtzVal).
            end.    
            when Progress.Reflect.DataType:Decimal then do:
                decVal =  dynamic-property(pServiceEntity,pName).
                pJson:Add(pName,decVal).
            end.
            when Progress.Reflect.DataType:Integer then do:
                iVal =  dynamic-property(pServiceEntity,pName).
                pJson:Add(pName,iVal).
            end.
            when Progress.Reflect.DataType:Int64 then do:
                i64Val =  dynamic-property(pServiceEntity,pName).
                pJson:Add(pName,i64Val).
            end.    
            when Progress.Reflect.DataType:Logical then do:
                lVal =  dynamic-property(pServiceEntity,pName).
                pJson:Add(pName,lVal).
            end.    
            when Progress.Reflect.DataType:Longchar then do:
                longVal =  dynamic-property(pServiceEntity,pName).
                pJson:Add(pName,longVal).
            end.    
            otherwise 
                undo, throw new UnsupportedOperationError(subst("Get ServiceEntity property &1 datatype '&2'",pName,pDataType:toString())).
        end.                                                 
    end method.                                                  
    
    method protected void AddPropertyExtentValueToJson(pServiceEntity as IServiceEntity,
                                                 pName as character,
                                                 pDataType as Progress.Reflect.DataType,
                                                 pJson as JsonObject):
        define variable cVal    as character extent no-undo.
        define variable iVal    as integer extent no-undo.
        define variable i64val  as int64 extent no-undo.
        define variable decVal  as decimal extent no-undo.
        define variable datVal  as date extent no-undo.
        define variable dtiVal  as datetime extent no-undo.
        define variable dtzVal  as datetime-tz extent no-undo.
        define variable lval    as logical extent no-undo.
        define variable longval as longchar extent no-undo.                                             
        define variable oJsonArray as JsonArray no-undo.              
        oJsonArray = new JsonArray().     
        pJson:Add(pName,oJsonArray).         
        do on error undo, throw:
            case pDataType:
                when Progress.Reflect.DataType:Character then do:
                    cVal =  dynamic-property(pServiceEntity,pName).
                    oJsonArray:Add(cVal).
                end.    
                when Progress.Reflect.DataType:Date then do:
                    datVal =  dynamic-property(pServiceEntity,pName).
                    oJsonArray:Add(datVal).
                end.    
                when Progress.Reflect.DataType:Datetime then do:
                    dtiVal =  dynamic-property(pServiceEntity,pName).
                    oJsonArray:Add(dtiVal).
                end.    
                when Progress.Reflect.DataType:DateTimeTZ then do:
                    dtzVal =  dynamic-property(pServiceEntity,pName).
                    oJsonArray:Add(dtzVal).
                end.    
                when Progress.Reflect.DataType:Decimal then do:
                    decVal =  dynamic-property(pServiceEntity,pName).
                    oJsonArray:Add(decVal).
                end.
                when Progress.Reflect.DataType:Integer then do:
                    iVal =  dynamic-property(pServiceEntity,pName).
                    oJsonArray:Add(iVal).
                end.
                when Progress.Reflect.DataType:Int64 then do:
                    i64Val =  dynamic-property(pServiceEntity,pName).
                    oJsonArray:Add(i64Val).
                end.    
                when Progress.Reflect.DataType:Logical then do:
                    lVal =  dynamic-property(pServiceEntity,pName).
                    oJsonArray:Add(lVal).
                end.    
                when Progress.Reflect.DataType:Longchar then do:
                    longVal =  dynamic-property(pServiceEntity,pName).
                    oJsonArray:Add(longVal).
                end.    
                otherwise 
                    undo, throw new UnsupportedOperationError(subst("Get ServiceEntity datatype '&1'",pDataType:toString())).
            end case.
            catch e as Progress.Lang.Error :
                // rethrow error unless Progress returned an indetermined array 
                // (error 16081) in which case we do nothing. In this case the 
                // JsonArray already created and added to the json will be empty 
                // as expected by caller    
                if e:GetMessageNum(1) <> 16081 then
                    undo, throw e.     
            end catch.  
        end.                                               
    end method.                                                  
    
    // hook for subclass
    method protected void ApplyServiceRequestProperties  (pClientRequest as IClientRequest,pServiceEntity as IServiceEntity):
    end method.
    
    // subclass hook 
    method protected void AddServiceEntityProperties(pJson as JsonObject, pServiceEntity as IServiceEntity, pClientRequest as IClientRequest ):
    end.   
    
    method protected logical IsBodyEmpty(pMessageBody as Object):
        // empty body comes as memptr
        return valid-object(pMessageBody) = false or (type-of(pMessageBody,Memptr) and cast(pMessageBody,Memptr):Size = 0). 
    end method.      
    
     // allow subclass override
    method protected IClientServiceGetRequest CreateClientServiceGetRequest(pcMethod as character, pcServiceUri as character, pUriQuery as IStringStringMap):
        return new ClientServiceGetRequest(pcMethod, pcServiceUri,pUriQuery).
    end.
    
     // allow subclass override
    method protected IClientServiceUpdateRequest CreateClientServiceUpdateRequest(pServiceEntity as IServiceEntity, pcMethod as character, pcServiceUri as character, pUriQuery as IStringStringMap, pBody as Object):
        return new ClientServiceUpdateRequest(pcMethod, pcServiceUri,pUriQuery).
    end.
    
    method protected override character MethodsAllowedList(pObject as Object):
        if type-of(pObject,IServiceEntity) then
            return MethodsAllowedList(cast(pObject,IServiceEntity)). 
        else  
            return "OPTIONS, HEAD, GET".
    end method.
        
    method protected character MethodsAllowedList(pEntity as IServiceEntity):
        define variable cAllowed as character no-undo.
        define variable cDlm     as character no-undo.
        cAllowed = "OPTIONS".
        cDlm = ", ".
        cAllowed = cAllowed + cDlm + "HEAD".
        if not pEntity:ReadOnly then
        do:
            if pEntity:CanUpdate then
                cAllowed  = cAllowed + cDlm + "PUT".
            if pEntity:CanCreate then
                cAllowed  = cAllowed + cDlm + "POST".
            if pEntity:CanDelete then
                cAllowed  = cAllowed + cDlm + "DELETE".     
        end.   
        else // POSAT PUt and GET is mutually exclusive for service entity
            cAllowed  = cAllowed + cDlm + "GET".
            
        return cAllowed.  
    end method.
    
    method protected override integer WriteAllowedHeader(pObject as Object, poResponse as IHttpResponse, pStatusCodeEnum as StatusCodeEnum):
        define variable oHeader as HttpHeader no-undo.
        oHeader = HttpHeaderBuilder:Build('Allow':u):Value(MethodsAllowedList(pObject)):Header.
        poResponse:SetHeader(oHeader).
        return ReturnStatusNoBody(poResponse,pStatusCodeEnum).
    end method. 
    
    destructor public ServiceEntityOperationHandler ( ):
           
    end destructor.

end class.