 
 /*------------------------------------------------------------------------
    File        : TestCustomer
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 15 11:46:50 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

 

block-level on error undo, throw.

using Ccs.Common.Application from propath.
 
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using OpenEdge.Core.StringConstant from propath.
using OpenEdge.Web.SendExceptionError from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IFindService from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.Common.DataService from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonConstruct from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.AppError from propath.
using Progress.Lang.Error from propath.
using Pmfo.Core.BusinessLogic.DatasetErrorProcessor from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Psi.Core.Manager.ISessionManager from propath.
using Pmfo.Core.Service.BusinessEntityOperationHandler from propath.
using Pmfo.Core.Service.CatalogOperationHandler from propath.
using Pmfo.Core.Service.ServiceEntityOperationHandler from propath.
using Pmfo.Core.Service.ClientGetRequest from propath.
using Pmfo.Core.BusinessLogic.RequestBuilder.QueryRequestBuilder from propath.
using OpenEdge.Core.JsonDataTypeEnum from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
 
 

class Pmfo.Tools.Test.TestBase abstract inherits DataService:
    define protected property ShowErrors as logical no-undo get. set. 
 
    define protected property ShowErrorsInWindow as logical   no-undo 
       get. 
       set(plShow as logical):
           if plShow then
               ShowErrors = true.
           ShowErrorsInWindow = plShow.     
       end.    
       
    define protected property ErrorEntity as JsonConstruct no-undo get. set. 
 
    
    define protected property FindService as IFindService no-undo 
       get():
           return cast(Ccs.Common.Application:ServiceManager:getService(get-class(IFindService)),IFindService).
       end.
       
    define protected property ServiceManager as  IServiceManager no-undo
        get():
            return cast(Ccs.Common.Application:ServiceManager,IServiceManager).
        end get.
        protected set.
    
    define protected property SessionManager as ISessionManager no-undo
        get():
            return cast(Ccs.Common.Application:SessionManager,ISessionManager).
        end get.
        protected set.
    
    define abstract property OperationHandler as BusinessEntityOperationHandler no-undo get. set.
   
    
     define property CatalogOperationHandler as CatalogOperationHandler no-undo
        get():
            if not valid-object(CatalogOperationHandler) then
                CatalogOperationHandler = new CatalogOperationHandler().
            return CatalogOperationHandler.    
        end.
        set.
    
    define property DatasetErrorHandler as DatasetErrorProcessor no-undo
        get():
            if not valid-object(DatasetErrorHandler) then
                DatasetErrorHandler = new DatasetErrorProcessor().
            return DatasetErrorHandler.    
        end.
        set.
    
    define abstract property ServiceOperationHandler as ServiceEntityOperationHandler no-undo get. set.  
    
    define protected property TempDir as character no-undo get():
            return session:temp-directory + "/temp/".
        end.
        set.
        
    method public void Setup(plant as integer):  
       // cast(Application:SessionManager,ISessionManager):Connection = "http://10.0.0.198:8080/web".
    end method.
     
    method public void Setup(  ):
        Init().
    end method.
    
    method public void Before(  ):
        Init().
    end method.
    
    method protected abstract character GetConnection().
    
    method protected void Init(  ):
        define variable cConnection as character no-undo.
          
        cconnection = GetConnection().  
        SessionManager:Connection = cConnection.
        // ContextManager keeps context  
        SessionManager:IsDebug = true.
        SessionManager:PlantId = 1.
        
    end method.
     
    method override public void Destroy(  ):
         

    end method.
    
    method public IGetDataResponse GetDataset(pBusinessEntity as IBusinessEntity,pServiceUri as character,pUriQuery as IStringStringMap,output dataset-handle phDataset):
        define variable oClientGetRequest     as IClientGetRequest     no-undo.
     
        define variable oQueryRequestBuilder  as Pmfo.Core.BusinessLogic.RequestBuilder.QueryRequestBuilder   no-undo.
        define variable oGetDataRequest       as IGetDataRequest       no-undo.
      
        assign
            oClientGetRequest    = CreateClientGetRequest("GET",pServiceURI,pURIQuery) .
            phDataset             = pBusinessEntity:GetRequestDataset(oClientGetRequest).
            oQueryRequestBuilder = CreateQueryRequestBuilder().
            oGetDataRequest      = oQueryRequestBuilder:BuildGetDataRequest(oClientGetRequest,phDataset).
        // get the data 
        return pBusinessEntity:GetData(oGetDataRequest,output dataset-handle phDataset by-reference).
    end. 
    
    // convert a response to prodataset readable
    method protected JsonObject CreateJsonRenameResponse(pJson as jsonobject, pcname as character):
        define variable oJsonArray as JsonArray no-undo.
        define variable oJson  as JsonObject no-undo.
        define variable oNewJson as JsonObject  no-undo. 
        oNewjson = new JsonObject().
        if pJson:GetType("response") = JsonDataType:Array then
        do:
            oJsonArray = pJson:GetJsonArray("response").
        end.
        else
        do: 
            oJsonArray = new Jsonarray().
            oJson = pJson:GetJsonObject("response").
            
            oJsonArray:Add(oJson).
       
        end.      
        oNewJson:Add(pcname,oJsonArray).
       
        return onewJson.
        
    end method.  
    
  
    method protected void AssertDatasetUpdate(phDataset as handle):
        
        if phDataset:error then 
        do:
            DatasetErrorHandler:AssertDataset(phdataset).
        end.    
    end method.    
    
       // override to return a requestbuilder that creates Core.BusinessLogic.GetDataRequest that has Metric property
    method protected QueryRequestBuilder CreateQueryRequestBuilder():
       return new QueryRequestBuilder().
    end method.
    
    method protected IClientGetRequest CreateClientGetRequest( input pcMethod as character, input pcServiceUri as character, input pUriQuery as IStringStringMap ):
         return new ClientGetRequest(input pcMethod, input pcServiceUri, input pUriQuery).
    end method.
    
    method public IBusinessEntity GetBE(pcservice as character  ):  
        define variable oBe as IBusinessEntity no-undo.
        define variable oInnerError as Error no-undo.
    
        oBe = cast(ServiceManager:GetService(get-class(IBusinessEntity),pcService),IBusinessEntity). 
         
        return oBe.
        
    end method.
    
    method public IServiceEntity GetServiceEntity(pcservice as character  ):  
        define variable oService as IServiceEntity no-undo.
        define variable oInnerError as Error no-undo.
    
        oService = cast(ServiceManager:GetService(get-class(IServiceEntity),pcService),IServiceEntity). 
         
        return oService.
        
    end method.
    
  
    method override public void Initialize(  ):

    end method.
     
    method public IStringStringMap  GetQuery(pcQueryNames as character extent,pcQueryValues as character extent):
        define variable qmap  as IStringStringMap no-undo.
        define variable i as integer no-undo.
        qmap = new StringStringMap().
        do i = 1 to extent(pcQueryNames):
            qmap:put(pcQueryNames[i],pcQueryValues[i]).
        end.
        return qmap.    
    end method.
    
    // can be added to catch to assist with debugging of errors
    method public void HandleError (pError as Error):
        ErrorEntity = ?.
        if ShowErrors then
        do:         
            Showerror(pError).  
        end.
       
        undo, throw pError.
    end method.
 
    
    method protected void WriteError(input poError as Progress.Lang.Error):
        define variable oResponse as JsonObject no-undo.
        define variable oError as JsonObject no-undo.
        define variable oErrorList as JsonArray no-undo.
        define variable iLoop as integer no-undo.
        define variable cRetVal as character no-undo.
        define variable iMax as integer no-undo.
        define variable oProp as Progress.Reflect.Property no-undo.
        define variable oInner as Progress.Lang.Error no-undo.
        define variable istart as integer no-undo.
        define variable oComplexError as JsonConstruct no-undo.
         
        Assert:NotNull(poError, 'Error').
        /* build the current error */
        assign oResponse  = new JsonObject()
               oErrorList = new JsonArray()
               iMax       = poError:NumMessages
               iStart = 1  
               .
        
/*        if type-of(poError, ComplexError) then                              */
/*        do:                                                                 */
/*             oComplexError = cast(poError,ComplexError):JsonMessages.       */
/*             if type-of(oComplexError,JsonObject) then                      */
/*                 oResponse:Add("dataErrors",cast(oComplexError,JsonObject)).*/
/*             else                                                           */
/*                 oResponse:Add("dataErrors",cast(oComplexError,JsonArray)). */
/*        end.                                                                */
/*        else do:                                                            */
            if type-of(poError, AppError) then
            do:
                if cast(poError, AppError):ReturnValue > "" then
                do:
                   assign oError = new JsonObject().
                   oErrorList:Add(oError).
                   oError:Add('errorMsg', cast(poError, AppError):ReturnValue).
                   oError:Add('errorNum', 0).
                   iStart = 2.
                end.
            end.
            if(type-of(poError,ApplicationError)) then
            do iLoop = iStart to iMax:
               oErrorList:Add(cast(poError,ApplicationError):GetError(iLoop)).   
            end.
            else do iLoop = iStart to iMax:
                assign oError = new JsonObject().  
                oErrorList:Add(oError).
                oError:Add('errorMsg' , poError:GetMessage(iLoop)).
                oError:Add('errorNum', poError:GetMessageNum(iLoop)).
            end.
            oResponse:Add('Errors':u, oErrorList).
/*        end.*/
        case true:
            // single error? add an simple JSON object
            when not valid-object(this-object:ErrorEntity) then
            do:
                assign this-object:ErrorEntity = oResponse.
            end.
            // if there's already an array, add to it                
            when type-of(this-object:ErrorEntity, JsonArray) then
                cast(this-object:ErrorEntity, JsonArray):Add(oResponse).
            
            // if there's a single json object, switch to an array
            when type-of(this-object:ErrorEntity, JsonObject) then
            do:
                assign oErrorList = new JsonArray().
                oErrorList:Add(cast(this-object:ErrorEntity, JsonObject)).
                oErrorList:Add(oResponse).
                
                assign this-object:ErrorEntity = oErrorList.
            end.
        end case.
        
/*        if session:debug-alert then                              */
/*            oResponse:Add('type':u, poError:GetClass():TypeName).*/
/*                                                                 */
        if poError:CallStack ne ? then
        do:
            assign oErrorList = new JsonArray()
                   iMax       = num-entries(poError:CallStack, StringConstant:LF)
                   .
            oResponse:Add('Stack':u, oErrorList).
            do iLoop = 1 to iMax:
                oErrorList:Add(entry(iLoop, poError:CallStack, StringConstant:LF)).
            end.
        end.
        
        assign oProp = poError:GetClass():GetProperty('InnerError':u).
        if valid-object(oProp) and
           oProp:DataType eq Progress.Reflect.DataType:Object and
           Progress.Lang.Class:GetClass(oProp:DataTypeName):IsA(get-class(Progress.Lang.Error)) then
        do:           
            assign oInner = oProp:Get(poError).
            if valid-object(oInner) then
                this-object:WriteError(oInner).
        end.
        
    end method.
    
    method public void  Showerror (pError as Error):
       
        define variable hWindow as handle no-undo.
        define variable oJson as JsonObject no-undo.
        define variable oJsonArray as JsonArray no-undo.
        define variable cLong as longchar no-undo.
        define variable cCaller as character no-undo.
        if ShowErrorsInWindow then
        do:
            WriteError(pError).
            run Pmfo/Tools/Gui/Wincode.w persistent set hWindow.
            if type-of(ErrorEntity,JsonObject) then
            do:
               oJson = cast(ErrorEntity,JsonObject).
                oJson:Write(input-output cLong, yes).
            end.
            else if type-of(ErrorEntity,JsonArray) then
            do:
                oJsonArray = cast(ErrorEntity,JsonArray).
                oJsonArray:Write(input-output cLong, yes).
            end.     
            
            clong = replace(cLong,"[","").
            clong = replace(cLong,"]","").
            clong = replace(cLong,"~{","").
            clong = replace(cLong,"~}" + StringConstant:LF,"").
            clong = replace(cLong,"~}","").       
            clong = replace(cLong,"~"","").
            clong = replace(cLong,",","").
            clong = replace(cLong,"~~,","").
            clong = replace(cLong,"~\\","~\").
            clong = replace(cLong,"~\/","/").
            // these may come from the error class - not from writeError
           // clong = replace(cLong,"errorMsg","Error Msg").
           // clong = replace(cLong,"errorNum","Error Num").
                                   
            cCaller = entry(1,program-name(2)," ").
            if cCaller = "handleError" then  
                cCaller = entry(1,program-name(3)," ").
            run SetContent in hWindow ("Error","Test",cCaller,cLong).
            run SetWidth in hWindow(160).
            run SetHeight in hWindow(20).
            run SetCloseOnEscape in hWindow.
            run SetCloseOnSpace in hWindow.
            run MoveToTop in hWindow.
            wait-for close of hWindow.
            return.
        end.
        else do:

            if type-of(pError,ApplicationError) then
            do:
                if valid-object(cast(pError,ApplicationError):InnerError) then
                   pError = cast(pError,ApplicationError):InnerError.
            end.
            else if type-of(pError,SendExceptionError) then
            do:
                if valid-object(cast(pError,SendExceptionError):InnerError) then
                   pError = cast(pError,SendExceptionError):InnerError.
            end.
            message "Error:" skip pError:GetMessage(1) skip
                    "InnerError:" skip perror skip
                    "CallStack" skip pError:CallStack
               view-as alert-box.

       end.
            
 
    end method.
    
   . 
    method public void WriteMemory(pcfile as character  ):  
        define variable oBe as IBusinessEntity no-undo.
        define variable ojson as JsonObject no-undo.
        define variable oResponse as JsonObject no-undo. 
        define variable oMap as StringStringMap no-undo.
        oMap = new StringStringMap().
        oBe = GetBE("memoryObjects").
        oResponse = OperationHandler:ExecuteGetOrHead("GET",  "http://localhost/minster/web/genesis", "/memoryObjects", oMap, oBe).
        oResponse:writefile(pcfile,yes).
        catch e as Progress.Lang.Error :
            handleError(e). 
        end catch.
          finally:
            delete object oBe.
        end.
    end method.
    
        
end class.