 
 /*------------------------------------------------------------------------
    File        : CatalogOperationHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Oct 12 EDT 2019 -  separated from crudOperationHandler
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
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
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.Catalog.ICatalog from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IHttpErrorStatus from propath.
using Pmfo.Core.Error.MethodNotAllowedError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Core.Service.ClientCatalogRequest from propath.
using Pmfo.Core.Service.IClientCatalogRequest from propath.
using Pmfo.Core.Service.JsonCatalogResponseProcessor from propath.
using Pmfo.Core.Service.OperationHandler from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.JsonUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
 

class Pmfo.Core.Service.CatalogOperationHandler inherits OperationHandler implements IOperationHandler,ISupportLogging: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
         
    constructor public CatalogOperationHandler (  ):
        super ().        
    end constructor.
    
    method public override JsonObject ExecuteGetOrHead
                                          (pcMethod as character,
                                           pcBaseAppTransportServiceURI as character,
                                           pcServiceURI as character,
                                           pURIQuery as IStringStringMap,
                                           pEntity as IEntity):

         define variable oCatalog      as ICatalog   no-undo.      
          
            
         if type-of(pEntity,ICatalog) then 
             oCatalog  = cast(pEntity,ICatalog).
         else     
            undo, throw new IllegalArgumentError(subst("Entity of Type &1",pEntity:GetClass():TypeName)).
        
         return ExecuteCatalog(pcMethod, 
                               pcBaseAppTransportServiceURI,
                               pcServiceURI, 
                               pURIQuery,
                               oCatalog).
        
    end method.
    
    method public override JsonObject ExecutePostOrPutOrDelete
                                  (pcMethod as character,
                                   pMessageBody as Object,
                                   pcBaseAppTransportServiceURI as character,
                                   pcServiceURI as character,
                                   pURIQuery as IStringStringMap,
                                   pEntity as IEntity):
                                       
        undo, throw new MethodNotAllowedError(pcMethod).
                                         
    end method.     
    
    // public for unittest - not defined in interface
    method public JsonObject ExecuteCatalog
                                  (pcMethod as character,
                                   pcBaseAppTransportServiceURI as character,
                                   pcServiceURI as character,
                                   pURIQuery as IStringStringMap,
                                   pCatalog as ICatalog):
                                       
        define variable oClientCatalogRequest as IClientCatalogRequest no-undo.
        define variable jsonResponse as JsonObject no-undo.
        define variable cSelect as character no-undo.
        define variable oJsonProcessor as JsonCatalogResponseProcessor no-undo.
        
        oClientCatalogRequest   = CreateClientCatalogRequest(pcMethod, pcServiceUri, pUriQuery).
        if extent(oClientCatalogRequest:Collections) > 2 then 
        do: 
            undo, throw new NotFoundError(pcServiceURI).
        end.
        
        oClientCatalogRequest:AssertUriQuery().
         
        if oClientCatalogRequest:KeyWheres[1] > "" 
        or (extent(oClientCatalogRequest:Collections) = 2 and oClientCatalogRequest:KeyWheres[2] > "") then
            undo, throw new BadRequestError("Catalog requests do not support keys in parenthesis").
        
        oClientCatalogRequest:AssertSelect().
               
        if pcMethod = "HEAD" then
            return ?. 
            
        jsonResponse = pCatalog:GetResources(pcBaseAppTransportServiceURI,
                                             oClientCatalogRequest:Collections,
                                             ResponseNodeName).     
        
        cSelect = oClientCatalogRequest:Select.
        if cselect> "" then
        do:
            oJsonProcessor = new JsonCatalogResponseProcessor().
            ojsonProcessor:ProcessGetResponse(jsonResponse,cSelect,ResponseNodeName).
        end.                                                                        
        return jsonResponse.   
                            
    end method.
    
    // allow subclass override
    method protected IClientCatalogRequest CreateClientCatalogRequest(pcMethod as character, pcServiceUri as character, pUriQuery as IStringStringMap):
        return new ClientCatalogRequest(pcMethod, pcServiceUri,pUriQuery).
    end.  
     
    method protected override character MethodsAllowedList(pObject as Object):
        return "OPTIONS, HEAD, GET".
    end method.
    
    method protected override integer WriteAllowedHeader(pObject as Object, poResponse as IHttpResponse, pStatusCodeEnum as StatusCodeEnum):
        define variable oHeader as HttpHeader no-undo.
        oHeader = HttpHeaderBuilder:Build('Allow':u):Value("HEAD, OPTIONS, GET"):Header.
        poResponse:SetHeader(oHeader).
        return ReturnStatusNoBody(poResponse,pStatusCodeEnum).
    end method.  
     
    destructor public CatalogOperationHandler ( ):
           
    end destructor.

end class.