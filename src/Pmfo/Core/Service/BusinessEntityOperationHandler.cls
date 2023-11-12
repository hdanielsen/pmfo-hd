 
 /*------------------------------------------------------------------------
    File        : BusinessEntityOperationHandler
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Mar 21 09:10:09 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.IGetDataTableResponse from propath.
using Ccs.BusinessLogic.IGetTableResultCountResponse from propath.
using Ccs.Common.Application from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.Logging.ISupportLogging from propath.
using OpenEdge.Net.HTTP.HttpHeader from propath.
using OpenEdge.Net.HTTP.HttpHeaderBuilder from propath.
using OpenEdge.Net.HTTP.IHttpResponse from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Net.URI from propath.
using OpenEdge.Web.DataObject.IOperationHandler from propath.
using Pmfo.Core.BusinessLogic.BeforeImageReadEnum from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IDynamicChild from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.BusinessLogic.IJsonReadHandler from propath.
using Pmfo.Core.BusinessLogic.IJsonReadHandlerProvider from propath.
using Pmfo.Core.BusinessLogic.IJsonRecursiveWriter from propath.
using Pmfo.Core.BusinessLogic.IJsonWriter from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.JsonRecursiveWriter from propath.
using Pmfo.Core.BusinessLogic.MethodBindingEnum from propath.
using Pmfo.Core.BusinessLogic.Parser.TableFieldValueParser from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.BusinessLogic.RequestBuilder.FindParentsRequestBuilder from propath.
using Pmfo.Core.BusinessLogic.RequestBuilder.FindRequestBuilder from propath.
using Pmfo.Core.BusinessLogic.RequestBuilder.QueryRequestBuilder from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.ResponseMetaDataEnum from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.ApplyRequestProcessor from propath.
using Pmfo.Core.Common.IFunction from propath.
using Pmfo.Core.Common.IResource from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.MethodNotAllowedError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Core.Error.UnprocessableEntityError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Error.ValidationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.ClientGetRequest from propath.
using Pmfo.Core.Service.ClientUpdateRequest from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Core.Service.IClientUpdateRequest from propath.
using Pmfo.Core.Service.JsonDatasetErrorProcessor from propath.
using Pmfo.Core.Service.JsonGetResponseProcessor from propath.
using Pmfo.Core.Service.OperationHandler from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.ErrorStringUtil from propath.
using Pmfo.Util.JsonBeforeImageValidator from propath.
using Pmfo.Util.JsonReader from propath.
using Pmfo.Util.JsonUtil from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.
using Pmfo.Util.RelationUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonConstruct from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.
using Progress.Reflect.Method from propath.
using Pmfo.Util.Array from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Progress.Lang.FlagsEnum from propath.
using Progress.Lang.ParameterList from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
 
 
class Pmfo.Core.Service.BusinessEntityOperationHandler inherits OperationHandler implements IOperationHandler,ISupportLogging: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define public property NextPageLinkTemplate   as character init "&1@nextPageLink" no-undo get. 
    define public property NextBatchLinkTemplate  as character init "&1@nextBatchLink" no-undo get. 
    define public property TotalCountTemplate     as character init "&1@count" no-undo get.
    define public property SuccessEntityName      as character init "@success" no-undo get.
  
     
    define protected property ServiceManager as IServiceManager no-undo 
       get():
            return cast(Application:ServiceManager,IServiceManager).
       end.
         
    constructor public BusinessEntityOperationHandler (  ):
        super ().        
    end constructor.
  
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method protected JsonObject ExtractRequestedTable(pJsonObject as JsonObject,pcSerializeName as characte):
        define variable oJsonObject       as JsonObject no-undo.
        define variable oJsonArray        as JsonArray  no-undo.
        define variable oJsonSingleObject as JsonObject no-undo.
        
        oJsonObject = new JsonObject().
         
        oJsonArray = JsonUtil:GetArrayFromSingleParents(pJsonObject,pcSerializeName).
        
        if not valid-object(oJsonArray) then
        do:
            oJsonSingleObject = JsonUtil:GetObjectFromSingleParents(pJsonObject,pcSerializeName).
            if valid-object(oJsonSingleObject) then
            do:
                oJsonObject:Add(pcSerializeName,oJsonSingleObject). 
                return oJsonObject.
            end.
            else
                undo, throw new NotFoundError(subst("Collection '&1'",pcSerializeName)).
        end.    
         
        oJsonObject:Add(pcSerializeName,oJsonArray). 
        return oJsonObject.
        catch e1 as NotFoundError :
            undo, throw e1.    
        end catch.
        catch e2 as Progress.Lang.Error :
            undo, throw new NotFoundError(subst("Collection '&1'",pcSerializeName),e2).
        end catch. 
    end method.
    
    method protected JsonObject ExtractRequestedRow(pJsonObject as JsonObject,phBuffer as handle, plUpdate as logical, pTableRequests as IGetDataTableRequest extent, plNoError as logical):
        define variable oJsonObject    as JsonObject  no-undo.
        define variable oJsonRow       as JsonObject  no-undo.
        define variable iExtent        as integer     no-undo.
        
        phBuffer:find-unique().
        
        oJsonObject = new JsonObject().
        oJsonRow = JsonUtil:GetObjectFromSingleParents(pJsonObject,phBuffer:serialize-name).
        oJsonObject:Add(phBuffer:serialize-name,cast(oJsonRow:Clone(),JsonObject)).
        return oJsonObject.
        catch e as Progress.Lang.Error :
            if e:GetMessageNum(1) = 138 then
            do:
                if plNoError then
                do:
                    oJsonObject = new JsonObject().
                    oJsonObject:AddNull(phBuffer:serialize-name).
                    return oJsonObject.
                end.
                do iExtent = 1 to extent(pTableRequests):
                     
                    if pTableRequests[iExtent]:TableName = phBuffer:table-handle:name then
                    do:    
                        undo, throw new TableNotFoundError(phBuffer, pTableRequests[iExtent], e).
                    end.
                end.
            end.
            // don't throw 404 for update
            if plUpdate = false  then
                undo, throw new NotFoundError(subst("Requested row for &1",phBuffer:serialize-name),e).
            else // this is not an expected error after update , but may happen for example if the dataset buffer serialize-names
                 // are lost/wrong      
                undo, throw e.
        end catch. 
    end method.
        
    // subclass hook 
    method protected void AddBusinessEntityProperties(pJson as JsonObject, pBusinessEntity as IBusinessEntity, pClientRequest  as ICLientRequest ):
    end.   
    
     
    method protected RowStateEnum GetMethodRowStateEnum (pcMethod as char, plAction as logical):
        if plAction then
        do:
            // allow PUT since we mistakenly used that on somne requests
            // but it will be made into create ny JsonReader 
            if pcMethod = "POST" or pcMethod = "PUT" then
                return RowStateEnum:Action.
            undo, throw new IllegalArgumentError(subst("Method '&1' and Action true",pcMethod)).    
        end.
        case pcMethod:
            when "GET"    then return RowStateEnum:None.
            when "PUT"    then return RowStateEnum:Modified.
            when "POST"   then return RowStateEnum:Created.
            when "DELETE" then return RowStateEnum:Deleted.
            otherwise 
                undo, throw new IllegalArgumentError(subst("Method '&1'",pcMethod)).
        end.
    end method.
    
    // public for unit test - not in interface
    method public override JsonObject ExecutePostOrPutOrDelete
                                  (pcMethod as character,
                                   pMessageBody as Object,
                                   pcBaseAppTransportServiceURI as character,
                                   pcServiceURI as character,
                                   pURIQuery as IStringStringMap,
                                   pEntity as IEntity):
        
        define variable oClientUpdateRequest  as IClientUpdateRequest no-undo.
        define variable jsonRequest           as JsonObject no-undo.
        define variable jsonDatasetRequest    as JsonObject no-undo.
 
        define variable hDataset              as handle no-undo.
        define variable oUpdateDataRequest    as IUpdateDataRequest no-undo. 
        define variable oFindRequestBuilder   as FindParentsRequestBuilder no-undo.
        define variable jsonResponse          as JsonObject no-undo.
        define variable oGetDataRequest       as IGetDataRequest no-undo. 
        define variable oGetActionDataRequest as IGetDataRequest no-undo. 
        define variable hRequestedBuffer      as handle no-undo.
        define variable oErrorProcessor       as JsonDatasetErrorProcessor no-undo.
        define variable oError                as Error no-undo.
        define variable lBeforeImage          as logical no-undo.
        define variable lBeforeImageRequired  as logical no-undo.
 
        define variable oRequestType          as RequestTypeEnum no-undo.
        define variable oJsonRequest          as JsonConstruct no-undo.
        define variable oJsonParent           as JsonObject no-undo.
        define variable oJsonChildArray       as JsonArray  no-undo.
        define variable ojsonReader           as JsonReader no-undo.
       
        define variable oUpdateBe             as IBusinessEntity no-undo.  
        define variable oEmptyTableRequests   as IGetDataTableRequest extent no-undo.
      //  define variable cActionFields         as character no-undo.
        define variable oAction               as IAction  no-undo.
        define variable iField                as integer no-undo.
        define variable lNoData               as logical no-undo.
        define variable iJson                 as integer no-undo.
        define variable lActionWithoutBody    as logical no-undo.
        define variable lRowCreatedFromURL    as logical no-undo.
        define variable lUniqueAction         as logical no-undo.
        define variable oBusinessEntity       as IbusinessEntity no-undo.
        define variable oJsonBeforeImageValidator as JsonBeforeImageValidator no-undo.
        define variable iCollection           as integer no-undo.
        define variable lDeleteAndNoContent   as logical no-undo.
        define variable lActionHasParent            as logical no-undo.
        define variable oResponse              as Object  no-undo.
        define variable cBeforeImageNames      as character extent no-undo.
        define variable lUseJsonReaderOnPost   as logical no-undo.
        define variable oBeforeImageReadOption as BeforeImageReadEnum no-undo.
        define variable lReturnDataAfterDelete as logical no-undo.
        define variable lRecursive             as logical no-undo.
        define variable oJsonWriter            as IJsonWriter           no-undo.
         
        if type-of(pEntity,IBusinessEntity) then 
             oBusinessEntity = cast(pEntity,IBusinessEntity).
        else     
            undo, throw new IllegalArgumentError(subst("Entity of Type &1",pEntity:GetClass():TypeName)).
            
        if oBusinessEntity:ReadOnly then
            undo, throw new MethodNotAllowedError(pcMethod).
        
        if valid-object(pMessageBody) and type-of(pMessageBody,JsonObject) = false then
            pMessageBody = ?.
        
        case pcMethod:
            when "PUT" then do:
                if oBusinessEntity:ReadOnly then
                    undo, throw new MethodNotAllowedError(pcMethod).
            end.
            when "POST" then do:
                // test after building the requests
            end.    
            when "DELETE" then do:
                //  
                // if not oBusinessEntity:CanDelete then
                
                  //  undo, throw new MethodNotAllowedError(pcMethod).
                //    
            end.
            otherwise 
                undo, throw new IllegalArgumentError(subst("Method parameter '&1'",pcMethod)).
        end case.
                 
        assign
            oClientUpdateRequest          = CreateClientUpdateRequest(oBusinessEntity,pcMethod,pcServiceURI,pURIQuery,cast(pMessageBody,JsonObject))
            hDataset                      = oBusinessEntity:GetUpdateRequestDataset(oClientUpdateRequest,output lBeforeImageRequired, output  oUpdateDataRequest )
           .
        
        if oUpdateDataRequest:RequestType = RequestTypeEnum:MultiTable then
        do:  
            if pcMethod <> "POST" then  
                undo, throw new NotImplementedError(subst("&1 of Multi Table",caps(pcMethod))).
             
            if oUpdateDataRequest:MainRequestedBuffer > "" then 
                undo, throw new IllegalStateError(subst("Main Requested Buffer '&1' defined in update request of type &2. Please contact IT.",
                                                    oUpdateDataRequest:MainRequestedBuffer,
                                                    oUpdateDataRequest:RequestType:ToString())).
                                                    
        end.
        else do:    
            if oUpdateDataRequest:MainRequestedBuffer = "" or oUpdateDataRequest:MainRequestedBuffer = ?  then 
                undo, throw new IllegalStateError(subst("Main Requested Buffer not defined in update request of type &1",oUpdateDataRequest:RequestType:ToString())).
         
            hRequestedBuffer = hdataset:get-buffer-handle (oUpdateDataRequest:MainRequestedBuffer).
                                                                                                   
            // this is an internal error not a notfounderror
            if not valid-handle(hRequestedBuffer) then
               undo, throw new IllegalStateError(subst("Requested buffer '&1' not found in dataset",oUpdateDataRequest:MainRequestedBuffer)).   
        
        end. 
        
        // do this after we have the oUpdateDataRequest
        case pcMethod:
            when "POST" then do:
                
                if oBusinessEntity:CanCreate = false and oUpdateDataRequest:ActionName = "" then
                    undo, throw new MethodNotAllowedError(pcMethod).
            end.
            when "DELETE" then do:
                // this is only for top level 
                if extent(oClientUpdateRequest:Collections) = 1 then
                do: 
                    
                    if not oBusinessEntity:CanDelete then
                        undo, throw new MethodNotAllowedError(pcMethod).
                end.  
            end.
        end case.
        
        oClientUpdateRequest:AssertUriQuery(oUpdateDataRequest:RequestType).
         
        // probably not necessary to check both conditions 
        if extent(oClientUpdateRequest:Collections) > 1 + (if valid-object(oUpdateDataRequest:CustomRequest) and type-of(oUpdateDataRequest:CustomRequest,IAction) then 1 else 0) 
        and oBusinessEntity:resource <> hRequestedBuffer:table-handle:serialize-name then
            oUpdateBe = ServiceManager:CreateBusinessEntity(hRequestedBuffer:table-handle:serialize-name).
        else
            oUpdateBe = oBusinessEntity.
        
        if valid-object(oUpdateDataRequest:CustomRequest) then
        do:
            if type-of(oUpdateDataRequest:CustomRequest,IAction) then
            do:
                oAction = cast(oUpdateDataRequest:CustomRequest,IAction).
                // if the requested entity has a key then 
                // - fetch the record or 
                //  - or prepare a request and set lUniqueAction true to avoid   
                if oClientUpdateRequest:Keywheres[extent(oClientUpdateRequest:Keywheres) - 1] > "" then
                do:
                    if pcMethod = "POST" 
                    // we're not able to handle empty json for action that is child  - 
                    // would likely need to invoke the FindParentsRequestBuilder - see below before we find the child
                    // or will the FindRequestbuilder just add the correct stuff???  
                    and (valid-object (pMessageBody) = false or oUpdateBe = oBusinessEntity) 
                    then 
                    do: 
                        if oAction:IsCreate then 
                        do: 
                            if not valid-object(hRequestedbuffer) then 
                                undo, throw new IllegalOperationError(subst("Request for &1(&2) with no requested buffer",
                                                                            oClientUpdateRequest:Collections[extent(oClientUpdateRequest:Collections) - 1],
                                                                            oClientUpdateRequest:KeyWheres[extent(oClientUpdateRequest:Keywheres) - 1]
                                                                            
                                                                            )). 
                             // return true if created  (no messagebody) 
                            lRowCreatedFromURL = AssignKeysFromUrl(pcMethod, oClientUpdateRequest, pMessageBody, hRequestedBuffer, true). // true = action
                        end.
                        else do:
                            // throws error if the fetch cannot be reolved and valid-object(pMessageBody) = false
                            // return false if the fetch cannot be resolved and valid-object(pMessageBody) - allowing the json body to 
                            // be processed below 
                            // True means the top buffer is empty - there may be child data 
                            lActionWithoutBody = GetRowForUniqueAction(oUpdateBe, oAction, oClientUpdateRequest, hDataset, hRequestedBuffer, pMessageBody).
                                
                        end.        
                    end.
                    else do:
                        // define request and set flag to validate the uniqueness after json has been processed 
                        oGetActionDataRequest = CreateRequestForUniqueAction(oClientUpdateRequest, hDataset,extent(oClientUpdateRequest:Collections) - 1).
                        lUniqueAction = true.  
                    end.    
                end.
                else if type-of(oUpdateBE,IDynamicChild) and oUpdateBe <> oBusinessentity then 
                do:
                    GetParentRowsForDynamicChild(oBusinessEntity, oClientUpdateRequest, hDataset).
                    lActionHasParent = true. 
                end.
                /** tested for cases where parent(s) has key and child does not not  need logic to manage empty or not
                else do:
                    oGetActionDataRequest = CreateRequestForUniqueAction(oClientUpdateRequest, hDataset,extent(oClientUpdateRequest:Collections) - 1).
                    oBusinessEntity:GetData(oGetActionDataRequest, output dataset-handle hDataset by-reference).
                    
                     
                end.
                **/
                
            end. 
            else 
                undo, throw new UnsupportedOperationError(subst("Custom Request of Type &1",oUpdateDataRequest:CustomRequest:GetClass():TypeName)).
                
        end.   
        else  
        do: 
            // create, delete or modify with a key  provided for the requested entity
            if valid-handle(hRequestedBuffer)
            and oClientUpdateRequest:Keywheres[extent(oClientUpdateRequest:Keywheres)] > "" then
            do:
                // return true if created  (no messagebody) 
                lRowCreatedFromURL = AssignKeysFromUrl(pcMethod, oClientUpdateRequest, pMessageBody, hRequestedBuffer, false). // false = not action
            end.     
        end.
         
        if valid-object(pMessageBody) then 
        do:
            jsonRequest  = cast(pMessageBody,JsonObject).
            
            lBeforeImage = JsonUtil:HasBeforeImage(jsonRequest).
            
            if lBeforeImage = false  then 
            do:
                if lBeforeImageRequired then 
                    undo, throw new UnprocessableEntityError(subst("The entity body must have before image data to execute a '&1' request for '&2' &3 ",
                                                                   pcMethod,
                                                                   oBusinessEntity:Resource,
                                                                   if valid-object(oUpdateDataRequest:CustomRequest) and type-of(oUpdateDataRequest:CustomRequest,IAction) 
                                                                   then quoter(cast(oUpdateDataRequest:CustomRequest,IAction):name,"'") 
                                                                   else ""
                                                                   )).
            end. 
         
            
            // if the url has parents then find them (or throw error if missing info ) before loading the json 
            if extent(oClientUpdateRequest:Collections) > 1 + (if valid-object(oUpdateDataRequest:CustomRequest) and type-of(oUpdateDataRequest:CustomRequest,IAction) then 1 else 0) then    
            do on error undo, throw:
                // TODO - This has gotten a bit out of hand...  duplication of find for parents above aand here
                // probably also unnecessary if lActionWithoutBody 
                
                if not lActionHasParent then 
                do:
                    oFindRequestBuilder   = CreateFindParentsRequestBuilder(). 
                    //  the find request builder works with the entire URL, but will return before processing the last table entry and thus aalso ignore an action  
                    oGetDataRequest       = oFindRequestBuilder:BuildGetDataRequest(oClientUpdateRequest,hDataset).
                    
                    do iCollection =  1 to extent(oClientUpdateRequest:Collections) - (if valid-object(oClientUpdateRequest:ServiceMethod) then 2 else 1):
                       if oClientUpdateRequest:KeyWheres[iCollection] = "" then 
                       do:
                           // TODO check relation
                           if oClientUpdateRequest:Collections[iCollection + 1] <> "notepages" then
                           undo, throw new UnsupportedOperationError(subst('The URL request for entity "&1" must have a key when it is parent for entity "&2"',
                                                                           oClientUpdateRequest:Collections[iCollection],
                                                                           oClientUpdateRequest:Collections[iCollection + 1]
                                                                           )).
                       end.                                                    
                    end. 
                    // get the parent data 
                    oBusinessEntity:GetData(oGetDataRequest,output dataset-handle hDataset by-reference).
                    AssertUniqueRecords(hDataset, oGetDataRequest:TableRequests).
                end.
                
                oJsonReader = GetJsonReader(oUpdateBe,pcMethod,oAction).
            
                ojsonReader:ImportJson(hRequestedBuffer, 
                                       false,
                                       jsonRequest,
                                       GetMethodRowStateEnum(pcMethod,oUpdateDataRequest:ActionName > ""),
                                       // this is a safe option that only kicks in if fields are missing from the after image and exists in the before image 
                                       if lBeforeImage 
                                       then BeforeImageReadEnum:AssignAfterFromBeforeImage
                                       else BeforeImageReadEnum:None).
                                       
                catch eval as ValidationError :
                     oUpdateBe:ThrowDatasetError(hDataset).    
                end catch.  
                catch e1 as Progress.Lang.Error :
                    HandleLoadError(e1,pcMethod,hDataset,jsonRequest).
                end catch.
            end.
            else do:
                         
                // TODO - all these rules should be moved to UpdateDataRequest
                // TODO -  BeforeImageReadEnum:AssignAfterFromBeforeImage should be used always if there is before iamge data 
                //         This is assuimed to now - it is a safe option that only kicks in if fields are missing from the after image and exists in the before image
                //          but should do some testing             
                if pcMethod = "POST" then
                do:
                    // default to BE settings 
                    oBeforeImageReadOption = oBusinessentity:BeforeImageReadOption.
                    lUseJsonReaderOnPost   = oBusinessentity:UseJsonReaderOnPost. 
                    if oUpdateBe = oBusinessentity then 
                    do:
                        if lBeforeImage 
                        and valid-object(oUpdateDataRequest:CustomRequest) then  
                        do:
                            cBeforeImageNames = JsonRequest:GetJsonObject("prods:before"):GetNames().
                            // if we have before image data for child table always use jsonreader and AssignAfterFromBeforeImage
                            if extent(cBeforeImageNames) > 1 or Array:Find(oBusinessentity:resource, cBeforeImageNames) = 0 then
                            do:   
                                 // this is a safe option for POST that only kicks in if fields are missing from the after image and exists in the before image
                                 // this will likely be simplied to always be used if the data has before image 
                                 oBeforeImageReadOption = BeforeImageReadEnum:AssignAfterFromBeforeImage.
                                 lUseJsonReaderOnPost   = true.
                            end.   
                        end.  
                    end.
                    else do: 
                        // if wqe are using updateBe grab the POST options from the updateBE if it has use json reader or AssignAfterFromBeforeImage
                        // but don't override if the BE has these settings since we may have defined these BECAUSE the action needs it and the uodateBE
                        // may not have been set to do this (since it would not have any effect)   
                        do:
                            if oUpdateBe:UseJsonReaderOnPost then 
                                lUseJsonReaderOnPost  = true.
                            if BeforeImageReadEnum:AssignAfterFromBeforeImage:Equals(oUpdateBe:BeforeImageReadOption) then
                               oBeforeImageReadOption = oUpdateBe:BeforeImageReadOption.
                        end.
                    end.     
                end.
                else 
                    oBeforeImageReadOption = oBusinessentity:BeforeImageReadOption.
                
               
                
                // check conditions and flags that tells that we cannot use read-json and load using jsonreader  
                // MOTE: This should be able to handle every case but we currently check the conditionsa tthat requires this and
                // have an else below since read-json is faster 
                if (pcMethod = "PUT" and (lBeforeImageRequired = false or oBusinessEntity:UseJsonReaderOnPut )) 
                or (pcMethod = "DELETE" and not lBeforeImageRequired) 
                // UseJsonLoadedOnPost defaults to returns true if NextIdent or the BE implements JsonLoadHandler  
                // can also be set to true for other reasone       
                // if actionwithoiutbody is true and we  get here we have child json and need to use the logic hjere
                // to avoid creating another record (which would happen using dataset:read-json below) 
                or (pcMethod = "POST" and (lUseJsonReaderOnPost or lActionWithoutBody or oUpdateDataRequest:RequestType = RequestTypeEnum:MultiTable)) 
                then
                do on error undo, throw:
                    
                    if lBeforeImage then 
                    do:
                        if oUpdateDataRequest:RequestType = RequestTypeEnum:MultiTable then 
                            undo, throw new NotImplementedError("Update with Before Image for Multi Table request").
                        jsonDatasetRequest = JsonUtil:WrapRequestForDataset(jsonRequest, hDataset, hRequestedBuffer:name).
                        oJsonBeforeImageValidator = new JsonBeforeImageValidator(pcMethod).
                        oJsonBeforeImageValidator:Parse(jsonDatasetRequest, true, hRequestedbuffer). // tru only check top after row - TODO? be stricter after next code freeze
                    end.    
                    
                    oJsonReader = GetJsonReader(oUpdateBe,pcMethod,oAction).
                    ojsonreader:ImportJson(if oUpdateDataRequest:RequestType = RequestTypeEnum:MultiTable then hDataset else hRequestedbuffer, 
                                           lActionWithoutBody,
                                           jsonRequest,
                                           GetMethodRowStateEnum(pcMethod,oUpdateDataRequest:ActionName > ""),
                                           oBeforeImageReadOption).
                    
                    catch eval as ValidationError :
                         oUpdateBe:ThrowDatasetError(hDataset).    
                    end catch.                       
                    catch e2 as Progress.Lang.Error :
                        HandleLoadError(e2,pcMethod,hDataset,jsonRequest).
                    end catch.
                end.
                
                else do on error undo, throw: 
                    jsonDatasetRequest = JsonUtil:WrapRequestForDataset(jsonRequest, hDataset, hRequestedBuffer:name).
                    // we have validated the method that allows thos above
                    if lBeforeImage = false then 
                    do: 
                        DatasetUtil:SetTrackingChanges(hDataset,true).
                    end.
                    else do:
                        // validate before image format and props
                        // ABL is too forgiving and may criss-cross values if for example prods:id is not unique
                        // it is too tempting for UI developers to leave the prods:id value blank since it works for single record updates 
                        // REGRET (for PMFO future): should not have allowed this  
                        oJsonBeforeImageValidator = new JsonBeforeImageValidator(pcMethod).
                        oJsonBeforeImageValidator:Parse(jsonDatasetRequest).
                    end.    
                    hDataset:read-json ("JsonObject",jsonDatasetRequest,"append"). // empty, but if there is data replace is correct
                    
                    if lBeforeImage = false then 
                    do: 
                        DatasetUtil:SetTrackingChanges(hDataset,false).
                    end.
                    
                    catch eval as ValidationError :
                         oUpdateBe:ThrowDatasetError(hDataset).    
                    end catch.  
                    catch e3 as Progress.Lang.Error :
                        HandleLoadError(e3,pcMethod,hDataset,jsonDatasetRequest).
                    end catch.
                end.
            end.
            
            if lUniqueAction then 
            do on error undo, leave:
                hRequestedBuffer:find-unique (cast(oGetActionDataRequest:TableRequests[1],Pmfo.Core.BusinessLogic.IGetDataTableRequest):QueryDefinition:toString()).
                catch e as Progress.Lang.Error :
                    if e:GetMessageNum(1) = 138 then 
                        undo, throw new BadRequestError(subst("The '&1' data in the body does not match unique request '&1/(&2)'",hRequestedBuffer:serialize-name,oClientUpdateRequest:KeyWheres[extent(oClientUpdateRequest:Keywheres) - 1])).   
                    else 
                        undo, throw e.     
                end catch.
            end.
            
        end.
        // Throw error if none of the preconditions to update without body is true 
        else if lRowCreatedFromURL = false and lActionWithoutBody = false then 
            undo, throw new UnprocessableEntityError(subst("'&1' request must have a valid json object body (or a key in the URL if supported)",pcMethod)).
        
         // NOT an action with no body 
       // else @TODO need error handling or management of metric support for actions woth no body 
        // if we have a BE for the requested table different than the requested resource use that from now on 
        
        oBusinessEntity = oUpdateBe.
        oResponse = oBusinessEntity:UpdateData(input-output dataset-handle hDataset by-reference,oUpdateDataRequest).
        
        // we have a bug  - some large dataset with more than one top buffer loses the serialize-name
/*        do i = 1 to hDataset:num-buffers:                                                                                           */
/*            hDataset:get-buffer-handle(i):buffer-release().                                                                         */
/*          //  hDataset:get-buffer-handle(i):table-handle:serialize-name = hDataset:get-buffer-handle(i):table-handle:serialize-name.*/
/*            hDataset:get-buffer-handle(i):serialize-name = hDataset:get-buffer-handle(i):serialize-name.                            */
/*        end.                                                                                                                        */
        
        if valid-object(oJsonReader) and valid-object(oJsonReader:WarningMessages) then 
        do:
            // paranoid - should be creatred in updataData
            if valid-object(oBusinessEntity:WarningMessages) then 
                oBusinessEntity:WarningMessages:AddAll(oBusinessEntity:Resource,oJsonReader:WarningMessages).
            else // BE WarningMessages  is public just for this...       
                oBusinessEntity:WarningMessages = oJsonReader:WarningMessages.
        end.
        
        // anticipate set of main requested buffer for multi table reqest that may have changed the relations to one top 
        if valid-handle(hRequestedBuffer) = false and oUpdateDataRequest:MainRequestedBuffer > "" then  
             hRequestedBuffer = hdataset:get-buffer-handle (oUpdateDataRequest:MainRequestedBuffer).
        jsonResponse = new JsonObject().
        
        if pcMethod = "DELETE" then 
        do:
            // support repsone with refreshed data after delete - added for recursive cases where 
            // the delete may be posted for a record further down the tree and parents may be updated an not deleted 
            if hDataset:get-buffer-handle(1):table-handle:has-records then
            do: 
                lReturnDataAfterDelete = true.
            end.   
        end.
        
        // create response unless this is a delete or delete returns data 
        // (delete returns data  = for example delete from recursive tree may return refreshed tree)    
        if pcMethod <> "DELETE" or lReturnDataAfterDelete then
        do:
           // check if the action should return no content or sucdess instead of data  
            if valid-object(oAction) then 
            do:
                if oAction:NoContentResponse then 
                do:
                      
                    if (valid-object(oBusinessEntity:WarningMessages) and oBusinessEntity:WarningMessages:HasData) or oBusinessEntity:HasSuccessMessages then
                        lNoData = true. // no data, but add warnings below
                    else
                       return ?.  // flag to caller to return http 204 no content 
                end. 
                else if oAction:IsPrimitive then 
                do:
                    if type-of(oResponse,IPrimitiveHolder) then 
                    do:
                        AddPrimitiveValueToJson( ResponseNodeName, cast(oResponse,IPrimitiveHolder), jsonResponse).
                        lNoData = true.
                    end.
                    else 
                        undo, throw new IllegalStateError(subst("Action &1 was excuted without errors, but the response is of not the expected format or type. Please Contact IT.",oAction:name)).   
                end.
                   
                /* Not sure if this is a good idea - maybe as a query option
                  so the client can control it
                if oAction:CompareFields > "" then
                do iField = 1 to num-entries(oAction:CompareFields) 
                on error undo, throw:
                   
                   cActionFields = cActionFields 
                                 + (if iField = 1 then "" else ",")                    
                                 + hRequestedBuffer:buffer-field(entry(iField,oAction:CompareFields)):serialize-name.
                   
                   catch e as Progress.Lang.Error :
                       // don't throw bad field - the fielda are validated elsewhere 
                      if e:GetMessageNum(1) <> 7351 then
                         undo, throw e.
                   end catch.
                end. 
                */  
                    
            end.     
            if lNoData = false then
            do:            
                oJSonWriter =  oBusinessEntity:GetJsonWriter(GetMethodRowStateEnum(pcMethod,oUpdateDataRequest:ActionName > ""),oUpdateDataRequest:ActionName,oClientUpdateRequest:Collections).                                            
                if not valid-object(oJsonWriter) and oUpdateDataRequest:RequestType <> RequestTypeEnum:MultiTable then
                do: 
                    
                    lRecursive = valid-handle(DatasetUtil:GetRecursiveRelation(hRequestedBuffer)).
                    if lRecursive then 
                        oJsonWriter = new JsonRecursiveWriter().  
                end.      
 
              
                 
               // TODO - clean up  - GetEntityResponse dedicated for update 
                jsonResponse = GetEntityResponse(pcBaseAppTransportServiceURI,
                                                hDataset, 
                                                oJsonWriter,
                                                oClientUpdateRequest, 
                                                if valid-object(oGetDataRequest) then oGetDataRequest:TableRequests else oEmptyTableRequests, 
                                                hRequestedBuffer, 
                                                "", // not pn use - option to slect some fields -   cActionFields,
                                                oClientUpdateRequest:Relations,
                                                oUpdateDataRequest:RequestType,
                                                true, // no error - don't throw 404 for missing requestebuffer after an update    
                                                if Pmfo.Core.Common.ServerTypeEnum:TextPlain:Equals(this-object:ServiceManager:ServerType) then ResponseMetaDataEnum:None
                                                else ResponseMetaDataEnum:NavigationLinks,
                                                "",  // select property name not applicable for update (used for error)
                                                true, // this is an update response
                                                false
                                                ).
           
                AddBusinessEntityProperties(jsonResponse,oBusinessEntity,oClientUpdateRequest).
                jsonResponse = WrapResponse(jsonResponse, hdataset, hRequestedBuffer, oUpdateDataRequest:RequestType).
            end.
        end. // not delete
        else // set flag to return ? - will be set false if sucess or warning messages are added  
            lDeleteAndNoContent = true.
           
        /***
           WarningMessages and successMessages have some differences that currently requires different approaches
           TODO - unify and abstract this and also add info messages
        ***/   
        /*
           Warningmessages Can be also be added on server and is currently returned from DA in a temp-table
                           It is also used both on GET and here    
                           The WarningMessages is thus a temp-table wrapper created at the start of a request
                           that knows the dataset of the request
                           The dataset is used to convert internal field names to serfializenames gfetting 
                           the dataset buffer form serializ-name
        */ 
        if valid-object(oBusinessEntity:WarningMessages) and oBusinessEntity:WarningMessages:HasData then
        do:
            jsonResponse:Add(WarningEntityName,oBusinessEntity:WarningMessages:ToJsonArray()).
            lDeleteAndNoContent = false.              
        end.  
        /**
           Successmessages is pure OO ands is currently (?) only managed in the BE 
           The SuccessMessage objects have constructors that requires public field keys and field names
           so there is no need to know the dataset 
           The class is thus lazyloaded and HasSuccessMessages is set when used
           It is deleted on start of update request toi make sure no statle data is present if 
           a BE UpdateData is called twice    
        */ 
        if oBusinessEntity:HasSuccessMessages then
        do:
            JsonResponse:Add(SuccessEntityName,oBusinessEntity:SuccessMessages:ToJsonArray()).
            lDeleteAndNoContent = false.            
        end.    
        if lDeleteAndNoContent then
        do:
            return ?.  // flag to caller to return http 204 no content  
        end.      
 
        return jsonResponse.
    end method.
    
    method protected void HandleTableNotFoundForUniqueFunction(phRequestedbuffer as handle,plOneToOneParent as logical,pcKeyExpression as character):
        define variable hParent as handle no-undo.
        
        // if one to one parent then the keyexpression is for the parent
        if plOneToOneParent then 
        do:
             // if the parent is found then throw not found child of parent  
            hParent = phRequestedbuffer:parent-relation:parent-buffer.
            hParent:find-unique() no-error.
            if phRequestedbuffer:avail then
            do:
                undo, throw new NotFoundError(subst("The single &1 of the &2 where &3",
                                               BufferUtil:GetRowName(phRequestedBuffer,true),
                                               BufferUtil:GetRowName(hParent,true),
                                               cast(QueryUtil:GetKeyEntry(pcKeyExpression,hParent,true),IQueryEntry):ToString(hParent,true,true) 
                                               )
                                           ).       
                
            
            end.
            // otherwise throw error below about the parent
            else 
                phRequestedbuffer = hParent.     
        end.
        
        undo, throw new TableNotFoundError(
                         phRequestedBuffer,
                         QueryUtil:GetKeyEntry(
                             pcKeyExpression,
                             phRequestedBuffer, 
                             true // public names in expression
                         )        
                     ).
               
    end.
    
    method protected void HandleLoadError(pError as Error, pcMethod as char, phDataset as handle, pJson as JsonObject):
        if pError:GetMessageNum(1) = 132 then 
        do:
            undo, throw new BadRequestError(BufferUtil:GetAlreadyExistsMessage(phDataset, pError),pError).
        end.
        else    
            undo, throw pError.
    end method.    
    
    method protected JsonReader GetJsonReader(pBusinessEntity as IBusinessEntity,pcMethod as character, poAction as IAction):
        define variable oNextIdMap as IMap no-undo.
        define variable oJsonReadHandler as IJsonReadHandler no-undo.
        oNextIdMap = pBusinessEntity:NextIds.
          
        if type-of(pBusinessEntity,IJsonReadHandlerProvider) then
            oJsonReadHandler = cast(pBusinessEntity,IJsonReadHandlerProvider):GetJsonReadHandler(pcMethod,poAction).
        else if type-of(pBusinessEntity,IJsonReadHandler) then
            oJsonReadHandler = cast(pBusinessEntity,IJsonReadHandler).
         
        // the reader cuttently allow  null for both, but the assumption is 
        // that we may want to validate the map .. perhaps 
        if valid-object(oNextIdMap) then
            return new JsonReader(oJsonReadHandler,oNextIdMap).
        else          
            return new JsonReader(oJsonReadHandler).
      
    end.    
    
    method protected Method GetFirstMethod(pObject as Object,pcname as char):                     
        define variable oMethods as Method extent no-undo.
        define variable i as integer no-undo.
        oMethods = pObject:GetClass():GetMethods(Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance or Progress.Reflect.Flags:DeclaredOnly).
        do i = 1 to extent(oMethods): 
            if oMethods[i]:Name = pcname then 
                return oMethods[i].
        end.    
    end.
    
    method private void FlattenXRefLinks(pJson as JsonObject, pParent as character, pLinks as ICollection):
        define variable cNames as character extent no-undo.
        define variable i as integer no-undo.
        define variable j as integer no-undo.
        define variable oIterator as IIterator no-undo.
        define variable oRelDef   as IRelationDefinition no-undo.
        define variable cFlattenList as character no-undo.
        
        if valid-object(pLinks) and pLinks:size > 0 then
        do:
            oIterator = pLinks:Iterator().
           
            if pParent > "" then
            do while oIterator:HasNext().
                oRelDef = cast(oIterator:Next(),IRelationDefinition).
               
                if oReldef:ParentName = pParent 
                and type-of(oRelDef,IRelation) 
                and cast(oReldef,IRelation):Xref then
                do: 
                    cFlattenList = cFlattenList 
                                 + (if cFlattenList = "" then "" else ",")
                                 + oReldef:PublicName.   
                end. 
            end. 
           
            if cFlattenList > "" then
            do:
                cNames = pjson:GetNames().
                do i = 1 to extent(cNames):
                     if pjson:GetType(cNames[i]) = JsonDataType:Array 
                     or pjson:GetType(cNames[i]) = JsonDataType:Object then
                     do:
                         if cFlattenList > "" and lookup(cNames[i],cFlattenList) > 0 then
                         do:
                             ReplaceXrefWithEntity(pjson, cNames[i]).
                         end.
                         else if pjson:GetType(cNames[i]) = JsonDataType:Array then
                         do:
                             do j = 1 to pjson:GetJsonArray(cNames[i]):Length:
                                 
                                 if pjson:GetJsonArray(cNames[i]):GetType(j) = JsonDataType:OBJECT then 
                                     FlattenXRefLinks(pjson:GetJsonArray(cNames[i]):GetJsonObject(j),cNames[i],pLinks).
                             end.                        
                         end.
                         else  
                         do:
                             FlattenXRefLinks(pJson:GetJsonObject(cNames[i]),cNames[i],pLinks).   
                         end.
                     end. 
                end.
            end.
        end.
    end method.     
    
    // TODO can this be improved by using Set and AddAll - must ensure that Equals works in the relations   
    method private ICollection MergeLinks(pBe as IBusinessEntity, pExpandedLinks as ICollection):
        define variable oLinks      as ICollection no-undo.
        define variable oIterator   as IIterator no-undo.
        define variable cLinkNames  as character no-undo.
        define variable cParentName as character no-undo.
        define variable oRelation   as IRelation no-undo.
        define variable oRelDef     as IRelationDefinition no-undo.
        
        oLinks = new Collection(). 
        // if expandedlinks we need to keep track of the belinks that we add
        if valid-object(pExpandedLinks) and pExpandedLinks:size > 0 then
        do: 
            oIterator = pBe:Links:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                cLinkNames = cLinkNames 
                           + (if cLinkNames = "" then "" else ",")
                           + subst("&1-&2",oRelation:ParentName,oRelation:PublicName).
                oLinks:Add(oRelation).
            end. 
            cLinkNames = right-trim(cLinkNames,",").
        end.
        else 
           oLinks:addAll(pBe:Links).
        if oLinks:size > 0 then
        do:
            oIterator = pExpandedLinks:Iterator().
            do while oIterator:HasNext():
                oRelDef = cast(oIterator:Next(),IRelationDefinition).
                if lookup(subst("&1-&2",oRelDef:ParentName,oRelDef:PublicName),cLinkNames) = 0 then
                   oLinks:Add(oRelDef).
            end.    
        end.
        else 
            oLinks:addAll(pExpandedLinks).
        return oLinks.  
    end method.     
    
    method protected IBusinessEntity ResolveGetRequest(pClientGetRequest as IClientGetRequest,input pRequestedBe as IBusinessEntity, output pRequest as IGetDataRequest, output phDataset as handle).
        define variable cRedirect        as character no-undo.
        define variable cRedirects       as character extent no-undo.
        define variable i                as integer no-undo.
        define variable cFixedContainers as character no-undo.
        define variable cContainerRef    as character no-undo.
        define variable cRelativeRequest as character no-undo.
        define variable cParentContext   as character no-undo.
        define variable iResource        as integer no-undo.
        define variable oUseBe           as IBusinessEntity no-undo.
        if extent(pClientGetRequest:Collections) > 1 and pClientGetRequest:KeyWheres[1] = "" then
        do:
            if pRequestedBe:HasRedirectMap then
            do:
                cRedirect = string(pRequestedBe:RedirectMap:Get(pClientGetRequest:Collections[2])).
                if cRedirect > "" then 
                do on error undo, throw: 
                    oUseBe = this-object:ServiceManager:CreateBusinessEntity(cRedirect).
                    pClientGetRequest:Redirect(cRedirect).
                    catch e as NotFoundError :
                        undo, throw new NotFoundError(subst("Redirect '&1' for '&2/&3'",
                                                           cRedirect,
                                                           pRequestedBe:Resource,
                                                           pClientGetRequest:Collections[2])
                                                      ,e).
                    end catch.
                    
                end.
                
            end.  
            if not valid-object(oUseBe) then
            do:
                cRedirect = pRequestedBE:GetContainerRedirect(pClientGetRequest, output cParentContext).
                if cRedirect > "" then 
                do: 
                    iResource = lookup(cRedirect,pClientGetRequest:ServiceURI,"/") - 1.
                    if iResource > 0 then 
                    do on error undo, throw:
                        oUseBe = this-object:ServiceManager:CreateBusinessEntity(cRedirect).
                        if iResource = extent(pClientGetRequest:collections) then   
                            undo, throw new UnsupportedOperationError(subst("Request for '&1' without Function in URL '&2'",cRedirect,pClientGetRequest:ServiceURI)). 
                            
                        pClientGetRequest:RedirectTo(iResource). // must correct this before LoadMethod
                        if type-of(oUseBe,IDynamicChild) then 
                        do:
                            cast(oUseBe,IDynamicChild):LoadMethod(pClientGetRequest,cParentContext).
                        end.
                        catch e as NotFoundError :
                            // give some context to clarify that the name was used to try to find a resource
                            // The URI could in theory reference a function 
                            undo, throw new NotFoundError(subst("Redirect Container '&1' for URI '&2'",
                                                               cRedirect,
                                                               pClientGetRequest:ServiceURI)
                                                          ,e).
                        end catch.
                    end. 
                    else do:
                        if index(pClientGetRequest:ServiceURI,subst("/&1(",cRedirect)) > 0  then 
                            undo, throw new UnsupportedOperationError(subst("Request for '&1' with key in URL '&2'",cRedirect,pClientGetRequest:ServiceURI)). 
                        else
                            undo, throw new IllegalStateError(subst("Container redirect to '&1' has not match in URL '&2'",cRedirect,pClientGetRequest:ServiceURI)). 
                    end.   
                end. 
           end.
        end.
        if not valid-object(oUseBe) then
            oUSeBe = pRequestedBe. 
        
        phDataset = oUseBE:GetRequestDataset(pClientGetRequest).  
        pRequest  = CreateQueryRequestBuilder():BuildGetDataRequest(pClientGetRequest,phDataset).
        return oUseBe.
    end method.  
    
    
    
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
        define variable jsonResponse                   as JsonObject                no-undo.
        define variable oClientGetRequest              as IClientGetRequest         no-undo.
        define variable oGetDataTableResponse          as IGetDataTableResponse     no-undo.
        define variable hDataset                       as handle                    no-undo.
        define variable oQueryRequestBuilder           as QueryRequestBuilder       no-undo.
        define variable oGetDataResponse               as IGetDataResponse          no-undo.
        define variable oGetDataRequest                as IGetDataRequest           no-undo.
        define variable iTable                         as integer                   no-undo.
        define variable cNextId                        as character                 no-undo.
        define variable cQuery                         as character                 no-undo.
        define variable iTotalCount                    as int64                     no-undo.
        define variable cNextLinkName                  as character                 no-undo.
        define variable hRequestedBuffer               as handle                    no-undo.
        define variable oLinks                         as ICollection               no-undo.
        define variable lRecursive                     as logical                   no-undo.
        define variable oJsonWriter                    as IJsonWriter               no-undo.
        define variable oBusinessEntity                as IBusinessEntity           no-undo.
        define variable cInternalServiceURI            as character                 no-undo.
        define variable oJson                          as JsonObject                no-undo.     
        define variable cRedirect                      as character                 no-undo.
        define variable iSelect                        as integer                   no-undo.
        define variable lPrimitiveFunction             as logical                   no-undo.
        define variable IPrimitiveSingleEntityFunction as logical                   no-undo.
        define variable oPrimitiveResponse             as Object                    no-undo.
        define variable oParentRelation                as IRelationDefinition       no-undo.    
        define variable iCollectionWhere               as integer                   no-undo.
        define variable oFunction                      as character                 no-undo. 
        define variable lNoContentResponse             as logical                   no-undo.
        define variable oApplyPtocessor            as ApplyRequestProcessor no-undo.
        
        if type-of(pEntity,IBusinessEntity) then 
             oBusinessEntity = cast(pEntity,IBusinessEntity).
        else     
            undo, throw new IllegalArgumentError(subst("Entity of Type &1",pEntity:GetClass():TypeName)).
        
        oClientGetRequest = CreateClientGetRequest(pcMethod,pcServiceURI,pURIQuery).
 
        oBusinessEntity = ResolveGetRequest(oClientGetRequest, oBusinessEntity, output oGetDataRequest, output hDataset).
        case pcMethod:
            // bad urls and query properties will have thrown errors so just return if HEAD 
            when "HEAD" then
               return ?.
            when "GET" then 
            do:
 
              // if oBusinessEntity:CanRead = false then
  
            end.
            otherwise    
                undo, throw new IllegalArgumentError(subst("Method '&1'",pcMethod)).
        end case.
        
        if valid-object(oClientGetRequest:ServiceMethod) 
        and type-of(oClientGetRequest:ServiceMethod,IFunction) then
        do:
            lPrimitiveFunction = cast(oClientGetRequest:ServiceMethod,IFunction):isPrimitive.
            lNoContentResponse = oClientGetRequest:ServiceMethod:NoContentResponse. 
            if lPrimitiveFunction then
                IPrimitiveSingleEntityFunction = cast(oClientGetRequest:ServiceMethod,IFunction):Binding = MethodBindingEnum:Entity.
            if oGetDataRequest:MainRequestedBuffer > "" then 
            do: 
                hRequestedBuffer = hdataset:get-buffer-handle (oGetDataRequest:MainRequestedBuffer).
                if oBusinessEntity:resource <> hRequestedBuffer:serialize-name then
                do:
                    if oBusinessEntity:HasLink then 
                    do:
                        oParentRelation = GetRelation(hRequestedBuffer:serialize-name,oBusinessEntity:Links).
                    end.    
                 
                    // if this is an child function then execute in the actual entity 
                    oBusinessEntity = ServiceManager:CreateBusinessEntity(hRequestedBuffer:serialize-name).
                end.
            end.    
        end.   
         
        if lPrimitiveFunction then 
        do on error undo, throw:   
            if IPrimitiveSingleEntityFunction then
            do: 
                if valid-object(oParentRelation) and oParentRelation:Cardinality = RelationTypeEnum:OneToOne then
                    iCollectionWhere = 2.
                else     
                    iCollectionWhere = 1.
              
                if oClientGetRequest:KeyWheres[extent(oClientGetRequest:Collections) - iCollectionWhere] = "" then
                    undo, throw new UnsupportedOperationError(subst("GET '&1' '&2' primitive bound function without key in the URL",hRequestedBuffer:serialize-name,oClientGetRequest:Collections[extent(oClientGetRequest:Collections)])).
            end.
          
            oPrimitiveResponse = oBusinessEntity:GetData(oGetDataRequest,input dataset-handle hDataset by-reference).
            //bug workaroudn for onetone child -  if this is not done the findunique fails
            // there are likely better workarounds
            hRequestedBuffer:table-handle:has-records.
           
            if IPrimitiveSingleEntityFunction then
            do on error undo, throw:
                // primitive functions must only find a single record in the Requestedbuffer  
                hRequestedBuffer:find-unique().
                catch e1 as Progress.Lang.Error :
                    HandleTableNotFoundForUniqueFunction(hRequestedBuffer,
                                                         iCollectionWhere = 2,
                                                         oClientGetRequest:KeyWheres[extent(oClientGetRequest:Collections) - iCollectionWhere]).
                     
                end catch.     
            end.
            if lNoContentResponse = false then
            do:
                jsonResponse = new JsonObject().
                AddPrimitiveValueToJson( ResponseNodeName, oPrimitiveResponse, jsonResponse).
            end.
            
        end.
        else do:
            oGetDataResponse = oBusinessEntity:GetData(oGetDataRequest,output dataset-handle hDataset by-reference).
            
            //  response select can be assigned by Be in which case we do not support select on URL
            //  It is possible to improve this to to allow client select to reduce the lsit from response    
            if lNoContentResponse = false and oClientGetRequest:Select > "" and oGetDataResponse:Select > "" then
            do: 
                // for beforeimage we allow client override of select for the top buffer, since the deault is just the key and the 
                // client may want more data to display since (if there are no changes of the top-buffer then thid cannot be read fromn the before-umage  
                if oGetDataResponse:WriteBeforeImage then
                do: 
                    if oClientGetRequest:Select > "" then 
                    do:
                        do iSelect = 1 to num-entries(oClientGetRequest:Select):  
                            if lookup(entry(iSelect,oClientGetRequest:Select),oGetDataResponse:Select) = 0 then 
                            do: 
                                oGetDataResponse:Select = oGetDataResponse:Select 
                                                        + (if oGetDataResponse:Select = "" then "" else ",")
                                                        + entry(iSelect,oClientGetRequest:Select).
                            end.    
                        end.
                    end.     
                end.     
                else     
                    undo, throw new BadRequestError(subst("Cannot specify select on '&1' request",oClientGetRequest:Collections[extent(oClientGetRequest:Collections)])) .  
            end.
            
            // reset frim response - requeste may change this 
            hRequestedBuffer = ?. 
            
            if lNoContentResponse = false then
            do: 
            
                if oGetDataResponse:MainRequestedBuffer > "" then
                do:
                    hRequestedBuffer = hdataset:get-buffer-handle (oGetDataResponse:MainRequestedBuffer).
                    // this is an internal error not a notfounderror
                    if not valid-handle(hRequestedBuffer) then
                        undo, throw new ApplicationError(subst("Requested buffer '&1' not found in returned dataset",oGetDataResponse:MainRequestedBuffer)). 
                    lRecursive = valid-handle(DatasetUtil:GetRecursiveRelation(hRequestedBuffer)).
                end.
                
                if valid-object(oClientGetRequest:ExpandedRelations) and oClientGetRequest:ExpandedRelations:size > 0 then
                    oLinks = MergeLinks(oBusinessEntity, oClientGetRequest:ExpandedRelations).
                else       
                    oLinks = oBusinessEntity:Links.   
                
                if oClientGetRequest:Apply > "" then 
                do:
                    if oGetDataResponse:MainRequestedBuffer = "" then 
                        undo, throw new UnsupportedOperationError(subst("'Apply' option for '&1' request that has multiple top entities",oClientGetRequest:Collections[1])). 
                    
                    if oClientGetRequest:Filter > "" then 
                    do: 
                        // temprasry limitation - see comments in method 
                        AssertApplyAndFilter(oGetDataRequest,  hRequestedBuffer, oClientGetRequest:GroupBy).
                    end.                    
                    oApplyPtocessor = new ApplyRequestProcessor().
                    jsonResponse = oApplyPtocessor:ProcessApply(oClientGetRequest,hDataset,hRequestedBuffer).
                end.              
                else do:
                    oJsonWriter = oBusinessEntity:GetJsonWriter(GetMethodRowStateEnum(pcMethod,false),
                                                                if valid-object(oClientGetRequest:ServiceMethod) then oClientGetRequest:ServiceMethod:name else "",
                                                                oClientGetRequest:Collections).
                                                  
                    if lRecursive and not valid-object(oJsonWriter) then 
                        oJsonWriter = new JsonRecursiveWriter().  
                     
                    jsonResponse = GetEntityResponse(pcBaseAppTransportServiceURI, 
                                                     hDataset, 
                                                     oJsonWriter,
                                                     oClientGetRequest, 
                                                     oGetDataRequest:TableRequests, 
                                                     hRequestedBuffer, 
                                                     if oGetDataResponse:Select > "" then oGetDataResponse:Select else oClientGetRequest:Select,
                                                     oLinks,
                                                     oGetDataResponse:RequestType,
                                                     oGetDataResponse:NoError,
                                                     if Pmfo.Core.Common.ServerTypeEnum:TextPlain:Equals(this-object:ServiceManager:ServerType) then ResponseMetaDataEnum:None
                                                     else oClientGetRequest:MetaDataEnum or oGetDataResponse:MetaDataEnum,
                                                     if oGetDataResponse:Select > "" then "" else oClientGetRequest:SelectProp,
                                                     false, // this is not an update response
                                                     oGetDataResponse:WriteBeforeImage
                                                     ).
                    
                    tablerequestloop:
                    do iTable = 1 to extent(oGetDataResponse:TableResponses):
                        oGetDataTableResponse = oGetDataResponse:TableResponses[iTable].
                        
                        if  valid-handle(hRequestedBuffer)
                        and RequestTypeEnum:SingleRow:Equals(oGetDataResponse:RequestType) = false
                        and hRequestedBuffer:table-handle:name = oGetDataTableResponse:TableName then 
                        do:
                            cNextId = oGetDataResponse:TableResponses[iTable]:NextPagingContext.
                            if cNextId > "" then
                            do:
                                cQuery = BuildUrlQuery(oClientGetRequest:URIQuery,oClientGetRequest:ContextProperties).
                                if oClientGetRequest:Skip > 0 or (oClientGetRequest:Count = true and oClientGetRequest:PagingContext = ?) then
                                do:
                                    cNextLinkName = subst(NextPageLinkTemplate,hRequestedBuffer:serialize-name).
                                    cQuery = cQuery + (if cQuery = "" then "?" else "&")
                                           + subst("&1=&2",oClientGetRequest:SkipProp,(if oClientGetRequest:Skip > 0 then oClientGetRequest:Skip else 0) + oClientGetRequest:NumRecords).
                                end.
                                else do:
                                    cNextLinkName = subst(NextBatchLinkTemplate,hRequestedBuffer:serialize-name).        
                                    cQuery = cQuery + (if cQuery = "" then "?" else "&") + subst("&1=&2",oClientGetRequest:PagingContextProp,URI:EncodeQuery(cNextId)).
                                end.
                                jsonResponse:Add(cNextLinkName,subst("&1&2&3",pcBaseAppTransportServiceURI,pcServiceURI,cQuery)).
                            end.
                            if oClientGetRequest:Count = true then
                            do:
                                iTotalCount = cast(oGetDataTableResponse,IGetTableResultCountResponse):NumResults.
                                jsonResponse:Add(subst(TotalCountTemplate,hRequestedBuffer:serialize-name),iTotalCount).
                            end.
                            leave tablerequestloop.
                        end.
                    end.
                end.
            end.  // lNOcoenentResponse =  false
        end.  
        
        if lNoContentResponse = false then      
            AddBusinessEntityProperties(jsonResponse,oBusinessEntity,oClientGetRequest).
        
        if valid-object(oBusinessEntity:WarningMessages) and oBusinessEntity:WarningMessages:HasData then
        do:
            if lNoContentResponse then 
                jsonResponse = new JsonObject(). 
            jsonResponse:Add(WarningEntityName,oBusinessEntity:WarningMessages:ToJsonArray()).        
        end.  
        if valid-object(oGetDataResponse) and oGetDataResponse:HasQuestions then 
        do:
            if not valid-object(JsonResponse) then 
                jsonResponse = new JsonObject(). 
            jsonResponse:Add(QuestionEntityName,oGetDataResponse:Questions:ToJsonArray()).
        end. 
        else if lNoContentResponse then 
            return ?. 
        
        if lPrimitiveFunction then 
            return jsonResponse.
        
        if lNoContentResponse = false and valid-object(oGetDataResponse) and oGetDataResponse:WriteBeforeImage then 
        do:      
            if JsonResponse:has("prods:errors") then 
            do: 
                 ProcessBeforeImageErrors(jsonResponse,hdataset).
            end.     
            
            JsonUtil:MergeDuplicateBeforeImageNodes(jsonResponse, hdataset).
             
            oJson = jsonResponse.
            jsonResponse = new JsonObject().   
            jsonResponse:Add(ResponseNodeName, oJson).
        end.
        else 
            jsonResponse = WrapResponse(jsonResponse, hdataset, hRequestedBuffer, oGetDataResponse:RequestType).
         
        return jsonResponse.
             
    end method.    
    
    // Temporary limitation - odata: 
    // The server filter for apply is supposed to be passed as an apply aggregation while the filter is supposed to 
    // reference data in the aggregatged result sop as for now we only allow filter on the groupby since that is 
    // valid both on server and in the result    
    method protected void AssertApplyAndFilter(pRequest as IGetDataRequest, pRequestedBuffer as handle, pcGroupBy as character):
        define variable oTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        define variable cFieldRefs as character no-undo.
        define variable i as integer no-undo.
        define variable cErrorFields as character no-undo.
        define variable cField      as character no-undo.
        oTableRequest = cast(pRequest:GetTableRequest(pRequestedBuffer),Pmfo.Core.BusinessLogic.IGetDataTableRequest).
        cFieldRefs = oTableRequest:GetPredicateFieldReferences().
        
        if pcGroupBy = "" then 
            undo, throw new NotImplementedError("Filter for Apply with only aggregate."). 
        
        do i = 1 to num-entries(CfieldRefs):
            cField = entry(i,cFieldRefs).
            if Cfield <> "plantid" // we support plantid eq in apply in ApplyoptioinParser, 
            and lookup(cField,pcGroupBy) = 0 
            then 
                cErrorFields = StringUtil:AppendEntry(cErrorFields,entry(i,cFieldRefs)). 
                     
        end.     
        if cerrorFields > "" then 
            undo, throw new NotImplementedError(subst("Filter of field&1 '&2' not referenced in the Apply GroupBy transformation",
                                                     if num-entries(cErrorfields) = 1 then "" else "s",
                                                     StringUtil:GetMessageFromEntries(cerrorFields)
                                                     )
                                              ). 
        
        
    end method. 
    
    method protected void AddPrimitiveValueToJson(pName as character,
                                               pObject as Object,
                                               pJson as JsonObject):
                                                   
        if type-of(pObject,IPrimitiveHolder) then 
            PrimitiveHolderUtil:AddToJson(pName, cast(pObject,IPrimitiveHolder), pJson).
        else if type-of(pObject,IPrimitiveArrayHolder) then 
            PrimitiveHolderUtil:AddToJson(pName, cast(pObject,IPrimitiveArrayHolder), pJson).
        else 
           undo, throw new IllegalStateError(subst("Type &1 returned from call to a BE function defined as primitive",pObject:GetCLass():TypeName)).                                                   
    end method.
    
    method protected IRelationDefinition GetRelation (pcName as character,plinks as ISet) :
        define variable oIterator as IIterator no-undo.
        define variable oRelation as IRelationDefinition no-undo.
        oiterator = plinks:Iterator().
        do while oIterator:HasNext():
            oRelation = cast(oIterator:Next(),IRelationDefinition).
            if oRelation:PublicName = pcname then 
                return oRelation.
        end.            
    end method.    
       
    method protected void ProcessBeforeImageErrors(pjson as JsonObject, phDataset as handle) :
        define variable oJson          as JsonObject        no-undo.
        define variable oJsonErrors    as JsonArray         no-undo.
        define variable cNames         as character  extent no-undo.
        define variable iEntity        as integer           no-undo.
        define variable iError         as integer           no-undo.
        define variable cMessages      as character  extent no-undo.
        define variable cErrorString   as character         no-undo.
        define variable oJsonObject    as JsonObject        no-undo.
        define variable cTypes         as character  extent no-undo.
        define variable iIds           as integer    extent no-undo.
        define variable cFields        as character  extent no-undo.
        define variable oErrorMessages as JsonArray no-undo.
        define variable iMsg           as integer no-undo.
        if pJson:has("prods:errors") then
        do:
            JsonUtil:MergeDuplicateErrorNodes(pJson, phdataset).
            oJson =  pJson:GetJsonObject("prods:errors").
            
            extent(cnames) = ?.
            cNames = ojson:GetNames().
            do iEntity = 1 to extent(cNames): 
                oJsonErrors = oJson:GetJsonArray(cNames[iEntity]).
                do iError = 1 to oJsonErrors:length:
                    oJsonObject    = oJsonErrors:GetJsonObject(iError).
                    cErrorString   = oJsonObject:GetCharacter("prods:error").
                    extent(cMessages) = ?.
                    extent(cTypes) = ?.
                    extent(iIds) = ?.
                    extent(cFields) = ?.
                    cMessages      = ErrorStringUtil:GetErrors(cErrorString, output cTypes, output iIds,  output cFields).
                    oErrorMessages = new JsonArray().
                    oJsonObject:Remove("prods:error").
                    oJsonObject:Add("errors",oErrorMessages).
                    do iMsg = 1 to extent(cMessages):
                        ojson = new JsonObject().
                        oJson:Add("errorType",cTypes[iMsg]).
                        oJson:Add("errorMsg",cMessages[iMsg]).
                        oErrorMessages:Add(oJson).
                    end.
                end.    
            end.  
           
        end.   
    end method.    
    
    // replace request collection 1 and 2 with a direct reference  
    method protected IBusinessEntity RedirectRequest(pcRedirect as character,pClientRequest as IClientRequest):
        define variable oBe            as IBusinessEntity no-undo.
        oBe = this-object:ServiceManager:CreateBusinessEntity(pcRedirect).  
        pClientRequest:Redirect(pcRedirect). 
        return oBe.
    end method.        
            
    method protected void ReplaceXrefWithEntity(pParentJson as JsonObject, pcSerializename as character):
        define variable oEntityJson    as JsonObject no-undo. 
        define variable cNames         as character  extent no-undo.
        define variable oNewJson       as JsonObject no-undo.
        define variable oNewJsonArray  as JsonArray  no-undo.
        define variable oOldJson       as JsonObject no-undo.
        define variable oOldJsonArray  as JsonArray  no-undo.
        define variable iExtent        as integer no-undo.
        define variable iName          as integer no-undo.
        
        if pParentJson:GetType(pcSerializename) = JsonDataType:ARRAY then
        do:
            oNewJsonArray = new JsonArray().
            oOldJsonArray = pParentJson:GetJsonArray(pcSerializename).
            do iExtent = 1 to oOldJsonArray:Length:     
                oOldJson = oOldJsonArray:GetJsonObject(iExtent).
                if oOldjson:Has(pcSerializename) then 
                do:
                    if oOldJson:GetType(pcSerializename) = JsonDataType:ARRAY then
                    do:
                       if oOldJson:GetJsonArray(pcSerializename):length = 1 then 
                           oNewJson = oOldJson:GetJsonArray(pcSerializename):GetJsonObject(1). 
                       else if oOldJson:GetJsonArray(pcSerializename):length = 0 then 
                           oNewJson = ?.
                       else         
                           undo, throw new IllegalStateError(subst("The '&1' entity at the the end of the XRef relation cannot have more than one row",pcSerializename)).
                    end.
                    else 
                       oNewJson = oOldJson:GetJsonObject(pcSerializename).
                    if oNewJson <> ? then 
                        oNewJsonArray:Add(oNewjson).
                end.
            end.     
            pParentJson:set(pcSerializename,oNewJsonArray).     
        end.    
        else do:
            oOldJson = pParentJson:GetJsonObject(pcSerializename).
            if oOldJson:Has(pcSerializename) then
            do:
                if oOldJson:GetType(pcSerializename) = JsonDataType:ARRAY then
                do:
                   if oOldJson:GetJsonArray(pcSerializename):length = 1 then 
                       oNewJson = oOldJson:GetJsonArray(pcSerializename):GetJsonObject(1). 
                   else if oOldJson:GetJsonArray(pcSerializename):length = 0 then 
                       oNewJson = ?.    
                   else      
                       undo, throw new IllegalStateError(subst("The '&1' entity at the the end of the XRef relation cannot have more than one row",pcSerializename)).
                end.
                else 
                   oNewJson = oOldJson:GetJsonObject(pcSerializename).
                
                if oNewJson = ? then 
                    pParentJson:setNull(pcSerializename).
                else
                    pParentJson:set(pcSerializename,oNewJson).    
            end.
            else
               pParentJson:SetNull(pcSerializename).
        end.
        
    end method.     
    
    method protected JsonObject WrapResponse(pJsonResponse as jsonObject, pDataset as handle,pRequestedBuffer as handle, pRequestType as RequestTypeEnum):
        define variable cReplaceName as character no-undo.
        define variable oWrapper as JsonObject no-undo.
        if RequestTypeEnum:MultiTable:Equals(pRequestType) then
        do:
            oWrapper = new JsonObject().
            oWrapper:Add(ResponseNodeName,pJsonResponse).
            return oWrapper.
        end.
        else do:    
            if valid-handle(pRequestedBuffer) then  
                cReplacename =  pRequestedBuffer:serialize-name.
            else 
                cReplacename = pDataset:serialize-name.
                 
            return GetResponseWrapper(pJsonResponse, cReplacename).
        end.
    end method.
    
    /**
    Create the json response from the dataset 
    - Extracts the requested table if the dataset has parent records that only is used to find the data
    - hides fields if selected fields specified
    - Calls the JsonGetResponseProcessor to decorate the json
    -- HATEOAS - adds links and keys
    --- metadataenum controls what to add 
    - Deals with repos tables - adds them at the top level 
    - converts array to object for one to one relation
    Parameters     
    **/
    method protected JsonObject GetEntityResponse(pcBaseURI             as character,
                                                  pDataset              as handle,
                                                  pJsonWriter           as IJsonWriter,  
                                                  pClientGetRequest     as IClientRequest,
                                                  pTableRequests        as IGetDataTableRequest extent,
                                                  pRequestedBuffer      as handle,
                                                  pSelectedFields       as character,
                                                  pLinkCollection       as ICollection,  
                                                  pRequestType          as RequestTypeEnum,
                                                  pNoError              as logical,
                                                  pMetaDataEnum         as ResponseMetaDataEnum,
                                                  pSelectedQueryName    as character,
                                                  plUpdateResponse      as logical,
                                                  plWriteBeforeImage    as logical):
                                                      
        define variable lExtractTable        as logical                  no-undo.
        define variable hHiddenFlds          as handle                   no-undo  extent.
        define variable cHideLaterFields     as character                no-undo.
        define variable jsonResponse         as JsonObject               no-undo.
        define variable oJsonOuterExtra      as JsonObject               no-undo.
        define variable oJsonProcessor       as JsonGetResponseProcessor no-undo.
        define variable cNames               as character extent         no-undo.
        define variable iNode                as integer                  no-undo.
        define variable hUhhiddenFkRelations as handle extent            no-undo. 
        define variable oIterator            as IIterator                no-undo. 
        define variable oRelation            as IRelation                no-undo.
        define variable oRelationDefinition  as IRelationDefinition      no-undo.
        define variable oTopnode             as JsonArray                no-undo.
        define variable iChild               as integer                  no-undo.
        define variable iBuffer              as integer                  no-undo.
        define variable lRecursiveWriter     as logical                  no-undo.
        define variable iRecursiveLevels     as integer                  no-undo.
        define variable iField               as integer no-undo. 
        
        
        if not valid-handle(pRequestedbuffer) then
        do:
            do iBuffer = 1 to pDataset:num-buffers:
                do iChild = 1 to pDataset:get-buffer-handle(iBuffer):num-child-relations:
                //  this is possible, but a bit more work and likely unnecessary 
                //  if a need should occur - consider separatation of this to a different resource instead    
                  if pDataset:get-buffer-handle(iBuffer):get-child-relation (iChild):recursive then 
                     undo, throw new NotImplementedError("Support of recursive relation in dataset with multiple top tables").   
                end.             
           end.
        end.
        if plUpdateResponse then
            lExtractTable = valid-handle(pRequestedBuffer).
            
        else if extent(pTableRequests) <> ? and valid-handle(pRequestedBuffer) then 
            lExtractTable = CheckTableRequestAndAssertUniqueParents(pDataset, pClientGetRequest, pTableRequests, pRequestedBuffer).
           
        if pSelectedFields > "" then do:
            hHiddenFlds = HideColumns(pDataset,if valid-handle(pRequestedBuffer) then pRequestedBuffer:name else "",pSelectedFields,pLinkCollection,pSelectedQueryName,plWriteBeforeImage, output cHideLaterFields).
        end.
      
        jsonResponse = new JsonObject().  
        
        hUhhiddenFkRelations = UnhideForeignKeys(pDataset, if valid-handle(pRequestedBuffer) then pRequestedbuffer:name else "", input-output cHideLaterFields).
        
        if type-of(pClientGetRequest,IClientGetRequest) then
        do:
            iRecursiveLevels = cast(pClientGetRequest,IClientGetRequest):Levels.
            if iRecursiveLevels <> ? then 
            do: 
                if valid-object(pJsonWriter) and not type-of(pJsonWriter,IJsonRecursiveWriter) then
                    undo, throw new BadRequestError(subst("levels=&1. Levels is only supported for entity with recursive relation",iRecursiveLevels)).
                if iRecursiveLevels < 1 then
                    undo, throw new BadRequestError(subst("levels=&1. Levels must be greater than 0 when specified.",iRecursiveLevels)).
            end.             
        end.
        else // update support display of recursive (added for delete, but can be used for atoer cases 
             // tell the recursive writer to build the entire tree  
            iRecursiveLevels = ?.
            
        if plWriteBeforeImage then 
        do: 
            pDataset:write-json ("JsonObject",jsonResponse,no,?,no,yes,yes).
        end.    
        else  
        do:
            if valid-object(pJsonWriter)  then
            do:
                if type-of(pJsonWriter,IJsonRecursiveWriter)  then
                do:
                    JsonResponse = cast(pJsonWriter,IJsonRecursiveWriter):WriteDataset(pDataset,
                                                                                       if extent(pTableRequests) > 0 
                                                                                       then cast(pTableRequests[1],Pmfo.Core.BusinessLogic.IGetDataTableRequest) 
                                                                                       else ?,
                                                                                       iRecursiveLevels).            
                    lRecursiveWriter = true.
                end.
                else 
                    JsonResponse =  pJsonWriter:WriteDataset(pDataset).
            end.    
            else   
                pDataset:write-json ("JsonObject",jsonResponse,no,?,no,yes).
            
            if lExtractTable and plWriteBeforeImage = false then 
            do on error undo, throw:
                JsonResponse = ExtractRequestedTable(jsonResponse,pRequestedBuffer:serialize-name).
                
                // return an empty row if not found
                catch e as NotFoundError :
                    
                    JsonResponse = new JsonObject().
                    oRelation = ?.
                    // if not singlerow (then this should be taken care of below) then check if this is a one-top-ne and return a single node null   
                    if RequestTypeEnum:SingleRow:Equals(pRequestType) = false and valid-object(pClientGetRequest:Relations) and valid-handle(pRequestedBuffer:parent-relation) then
                    do:
                        oRelation = RelationUtil:FindRelationByPublicName(pClientGetRequest:Relations, pRequestedBuffer:parent-relation:parent-buffer:serialize-name,pRequestedBuffer:serialize-name ).
                    end.
                    // if one to one or many to one don't return array - Note that this is somewhat duplication of the logic below for RequestTypeEnum:SingleRow:
                    if valid-object(oRelation) 
                    and (oRelation:Cardinality = RelationTypeEnum:OneToOne or oRelation:Cardinality = RelationTypeEnum:ManyToOne) then
                        JsonResponse:AddNull(pRequestedBuffer:serialize-name).  
                    else do:
                        JsonResponse:Add(pRequestedBuffer:serialize-name,new jsonArray()).
                    end .      
                end catch.
            end.
            
            // the recursive Wetier manages this on its own   
            if RequestTypeEnum:SingleRow:Equals(pRequestType) and not lRecursiveWriter then
            do:
                // collect any other top tables - (this would be respos tables)
                // and add them back separately below   
                oJsonOuterExtra = JsonUtil:ExtractTopSiblings(JsonResponse, pRequestedBuffer:serialize-name).
                if not lRecursiveWriter then
                   jsonResponse = ExtractRequestedRow(jsonResponse, pRequestedBuffer, plUpdateResponse, pTableRequests, pNoError).
            end.
        end.    
        oJsonProcessor = new JsonGetResponseProcessor(pcBaseURI,
                                                      if plWriteBeforeImage then ResponseMetaDataEnum:None else pMetaDataEnum).
         
        oJsonProcessor:processGetResponse(jsonResponse,
                                          pDataset ,
                                          // don't pass requested if not extracted unless this is a row request
                                          // as there may be a repos table that is one of the top-buffers that need to be parsed 
                                          if RequestTypeEnum:SingleRow:Equals(pRequestType) or lExtractTable 
                                          then pRequestedBuffer 
                                          else ?,
                                          pRequestType,
                                          pLinkCollection,
                                          pClientGetRequest:Collections,
                                          pClientGetRequest:KeyWheres,
                                          // we need to keep the hide later fields for the recursive logic 
                                          // TODO - qualified fields should be passed in here also for recursive 
                                          // as of current no recursive dataset has this  
                                          cHideLaterFields).
        
            
        if plWriteBeforeImage = false then
        do:    
            // if any response tables was part of the single row add them to the response   
            if not lRecursiveWriter then 
            do: 
                if valid-object(oJsonOuterExtra) then
                do:
                    define variable cdummy as character extent no-undo.
                    oJsonProcessor:processGetResponse(oJsonOuterExtra,
                                                  pDataset,
                                                  ?,
                                                  ?, 
                                                  new Collection(),
                                                  cdummy,
                                                  cdummy,
                                                  "").
                    if oJsonOuterExtra:Has(pRequestedBuffer:serialize-name) then
                    do:
                        oJsonOuterExtra:Remove(pRequestedBuffer:serialize-name).
                    end. 
                    extent(cnames) = ?.
                    cNames = oJsonOuterExtra:GetNames().
                    do iNode = 1 to extent(cnames):
                        jsonResponse:Add(cNames[iNode],oJsonOuterExtra:GetJsonArray(cNames[iNode])). 
                    end.  
                end.
                if valid-handle(pRequestedBuffer) then
                do:
                    // if one to one replace the array with the first node 
                    if RequestTypeEnum:SingleTopTable:Equals(pRequestType) and valid-object(pLinkCollection) then
                    do: 
                         if jsonResponse:Has(pRequestedBuffer:serialize-name) and jsonResponse:GetType(pRequestedBuffer:serialize-name) = JsonDataType:ARRAY then
                         do:
                             oIterator = pLinkCollection:Iterator().
                             do while oIterator:HasNext():
                                 oRelationDefinition = cast(oIterator:next(),IRelationDefinition).
                                 if oRelationDefinition:PublicName = pRequestedBuffer:serialize-name and RelationTypeEnum:OneToOne:Equals(oRelationDefinition:Cardinality) then
                                 do:
                                     JsonUtil:MoveOnlyNodeUp(jsonResponse,pRequestedBuffer:serialize-name).
                                     leave.
                                 end.
                             end.    
                         end.
                    end.
                end.
                FlattenXRefLinks(JsonResponse,
                                 if valid-handle(pRequestedBuffer) and valid-handle(pRequestedBuffer:parent-relation)
                                 then pRequestedBuffer:parent-relation:parent-buffer:serialize-name 
                                 else if valid-handle(pRequestedBuffer) then pRequestedBuffer:serialize-name
                                 else "",
                                 pLinkCollection).
            end.
        end.
        
        return jsonResponse.
        finally:
            if extent(hHiddenFlds) <> ? then 
                UnhideColumns(hHiddenFlds).
            HideForeignKeysAgain(hUhhiddenFkRelations).    
        end.            
  
    end method.    
        
    /* Turn off foreign key hidden and use the hide later option to avoid
       problems in jsonGetResponseProcessor when building id and linkid 
       for public child with hidden foreignkeys. (there may be cleaner ways to 
       do this, but they all seem to requiring find of buffers to get values )
       @return  - extent of relations with unhidden foreign keys. 
                  The extent size is num-buffers and may have ? entries, 
    */
    method protected handle extent UnhideForeignKeys (phDataset as handle, pRequestedbuffer as character , input-output pcHideLater as character):
        define variable i          as integer no-undo.
        define variable hRelations as handle  extent no-undo.        
        define variable hBuffer    as handle  no-undo.
        define variable iField     as integer no-undo.
        define variable cField     as character no-undo.
        define variable hField     as handle    no-undo.
      
        extent(hRelations) = phDataset:num-buffers.
        do i = 1 to phDataset:num-buffers:
            hBuffer = phDataset:get-buffer-handle(i).
            if valid-handle(hBuffer:parent-relation) and hBuffer:parent-relation:active and hBuffer:parent-relation:foreign-key-hidden then
            do: 
                hRelations[i] = hBuffer:parent-relation.
                // if the parent and child has the same name then skip this 
                // (it is assumed to be an XREf relation so it would be wrong and 
                // it also cuases error in  JsonGetResponseProcessor 
                if hRelations[i]:parent-buffer:serialize-name <>  hRelations[i]:child-buffer:serialize-name then
                do:
                    do iField = 2 to num-entries(hRelations[i]:relation-fields) by 2:
                        cField = entry(iField,hRelations[i]:relation-fields).
                        hField = hBuffer:buffer-field(cField).
                        if not hField:serialize-hidden then
                        do:
                            cField = hField:serialize-name.
                            if hBuffer:name <> pRequestedBuffer then
                                cField = hBuffer:serialize-name + "." + cField.
                            if lookup(cField,pcHideLater) = 0 then
                            do: 
                                pcHideLater = pcHideLater
                                            + (if pcHideLater = "" then "" else ",")
                                            + cField.
                            end.
                        end.
                    end.    
                    hRelations[i]:foreign-key-hidden = false.
                end.
                else do:
                    // if there is an xref relation get rid of the fields added to the parent in a previous iteration 
                    do iField = 1 to num-entries(pcHideLater):
                        if entry(iField,pcHideLater) begins subst("&1.",hRelations[i]:parent-buffer:serialize-name) then
                        do:
                            entry(iField,pcHideLater) = "".
                            pcHideLater = trim(replace(pcHideLater,",,",","),",").
                        end.         
                    end.     
                end.    
            end.
        end.    
        return hRelations.
    end method.
    
    method protected void HideForeignKeysAgain(phRelations as handle extent):
        define variable i as integer no-undo.    
        do i = 1 to extent(phRelations):
            if valid-handle(phRelations[i]) then 
               phRelations[i]:foreign-key-hidden = true.
        end.    
    end method.

    // allow subclass override
    method protected QueryRequestBuilder CreateQueryRequestBuilder():
        return new QueryRequestBuilder().
    end method.
    
    method protected FindRequestBuilder CreateFindRequestBuilder():
        return new FindRequestBuilder().
    end method.
    
    // allow subclass override
    method protected FindParentsRequestBuilder CreateFindParentsRequestBuilder():
        return new FindParentsRequestBuilder().
    end method.
     
    method protected IGetDataRequest CreateRequestForUniqueAction(pUpdateRequest as IClientUpdateRequest, phDataset as handle, piResource as integer):
        define variable oClientGetRequest    as IClientGetRequest no-undo.
        define variable cGetURI          as character no-undo.
        define variable oFindRequestBuilder  as FindRequestBuilder no-undo.
        define variable oRequest             as IGetDataRequest no-undo.
        define variable iResource            as integer no-undo.
        define variable i                    as integer no-undo.
        define variable cStrippedUri         as character no-undo.
        
        cStrippedUri = left-trim(pUpdateRequest:ServiceURI,"/").
        do i = 1 to piResource: 
            cGetURI = left-trim(subst("&1/&2",cGetUri,entry(i,cStrippedUri,"/")),"/").  
        end.    
        
        oClientGetRequest = CreateClientGetRequest("GET",cGetURI,new StringStringMap()).
        // The RequestRouter may have resolved keyWheres with single key entries in the call to GetUpdateRequestDataset 
        // so since we are using the existing dataset we need those keywhere expressions
        // TODO = This is a design problem that likely can be improved: 
        // the RequestBuilder does not have access to the BE and uses KeyValuePArser and BufferUtil:ConvertSingleKeyToExpression
        // However, to resolve single key for some complex relations with dynamic and/or hidden keys the BE now has its own ConvertSingleKeyToExpression 
        // that can be overridden. The RequestRouter uses this and may update the CLientRequest:keywheres     
        do i = 1 to extent(oClientGetRequest:KeyWheres):
            if pUpdateRequest:KeyWheres[i] <> oClientGetRequest:KeyWheres[i] then 
                oClientGetRequest:KeyWheres[i] = pUpdateRequest:KeyWheres[i].
        end.
        
        oFindRequestBuilder = CreateFindRequestBuilder().
        return oFindRequestBuilder:BuildGetDataRequest(oClientGetRequest,phDataset).
            
    end method.
    
    method protected logical AssignKeysFromUrl(pcMethod as character,pClientUpdateRequest as IClientUpdateRequest, pMessageBody as Object,phRequestedBuffer as handle, plAction as logical):
         define variable lRowCreatedFromURL as logical no-undo.
         if valid-object(pMessageBody) then 
              AssertJsonAndAddValues(cast(pMessageBody,JsonObject), phRequestedBuffer, pClientUpdateRequest:Keywheres[extent(pClientUpdateRequest:Keywheres) - if plAction then 1 else 0]). 
         else if pcMethod = "POST" then 
         do:
              CreateRowForKeyExpression(phRequestedBuffer, pClientUpdateRequest:Keywheres[extent(pClientUpdateRequest:Keywheres) - if plAction then 1 else 0]).
              lRowCreatedFromURL = true.
         end.    
         else // TODO consider support of DELETE with just keys (possibly by calling CreateRowForKeyExpression or a variation that sets delete\
              //  - this would probably require some option in the entity   
             undo, throw new UnsupportedOperationError(subst("&1 request with key in URL and no JSON Body",pcMethod)).
    end method.
    
    method protected void CreateRowForKeyExpression(phRequestedBuffer as handle, pcFieldExpression as character):
        define variable oTableFieldValueParser as TableFieldValueParser no-undo.
        oTableFieldValueParser = new TableFieldValueParser(). 
        oTableFieldValueParser:CreateRow(phRequestedBuffer, pcFieldExpression).
    end.
    
    method protected void AssertJsonAndAddValues(pJson as JsonObject,phRequestedBuffer as handle, pcFieldExpression as character):
        define variable oTableFieldValueParser as TableFieldValueParser no-undo.
        define variable cnames as character extent no-undo.
        define variable oRequestNode as JsonConstruct no-undo. 
        oRequestNode = JsonUtil:GetRequestNode(pjson, phRequestedBuffer, true). // true throw erro somewhat mute as it is already checked in JsonReader 
        if type-of(oRequestNode,JsonArray) then 
             undo, throw new UnprocessableEntityError(subst("Cannot submit '&1' as json array with URL '/&1(&2)' that has a key ",phRequestedBuffer:serialize-name,pcFieldExpression)).
        
        oTableFieldValueParser = new TableFieldValueParser(). 
        oTableFieldValueParser:AssertJsonAndAddValues(cast(oRequestNode,JsonObject), phRequestedBuffer, pcFieldExpression).
    end.
     
    method protected logical GetRowForUniqueAction(pBe as IBusinessEntity, pAction as IAction, pClientUpdateRequest as IClientUpdateRequest, phDataset as handle, pRequestedBuffer as handle, pMessageBody as Object):
        define variable oTableRequest    as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        define variable cFieldReferences as character     no-undo.
        define variable iFld             as integer       no-undo.
   //     define variable oJsonRequestNode as JsonObject    no-undo.
        define variable oJsonConstruct   as JsonConstruct no-undo.
        define variable oJsonBody        as JsonObject    no-undo.
        define variable cNames           as character     extent no-undo.
        define variable iNode            as integer no-undo.
        define variable hFld             as handle no-undo.
        define variable hDataset         as handle no-undo.
        define variable hTableRead       as handle no-undo.
        define variable hTableRequest    as handle no-undo.
        define variable oGetRequest      as IGetDataRequest no-undo. 
        define variable cFld             as character no-undo.
        
        if pAction:CompareFields = "" then 
        do:
            if valid-object(pMessageBody) then 
                return false.
            else     
                undo, throw new UnprocessableEntityError(subst("'POST' request for action '&1' must have a valid json object body",pAction:name)).
        end.
              
        if phDataset:num-buffers > 1 
        and pRequestedBuffer:num-child-relations > 0 
        and pBE:Datasets[1]:get-buffer-handle[1]:table-handle = pRequestedBuffer:table-handle then
            hDataset =  pBe:Datasets[1].
        else 
            hDataset = phDataset.
       
        oGetRequest = CreateRequestForUniqueAction(pClientUpdateRequest, hDataset, extent(pClientUpdateRequest:Collections) - 1).
         
        if valid-object(pMessageBody) then 
        do:
            oJsonBody  = cast(pMessageBody,JsonObject).
            oJsonConstruct = JsonUtil:GetRequestNode(oJsonBody,pRequestedBuffer,true).
            if type-of(oJsonConstruct,JsonArray) then 
                return false.
        //    oJsonRequestNode = cast(oJsonConstruct,JsonObject).
            
        end.
        
        else do: 
            oTableRequest = cast(oGetRequest:GetTableRequest(pRequestedBuffer:table-handle:name),Pmfo.Core.BusinessLogic.IGetDataTableRequest).
            
            cFieldReferences = oTableRequest:GetPredicateFieldReferences().
            if valid-handle(pRequestedBuffer:parent-relation) then 
            do iFld = 2 to num-entries(pRequestedBuffer:parent-relation:relation-fields) by 2:
                // NOTE; We don't care about double entries as of current
                cFieldReferences = cFieldReferences
                                 + "," 
                                 + entry(iFld,pRequestedBuffer:parent-relation:relation-fields).
            end.    
            // This is used for action and the record is marked as row -created
            // We set TablRequest select, but this is typicaly ignored so blank out all fields that are not 
            // requested to avoid anyone relying on the non-posted data in action logic 
            // TODO - improve POST of action to handle existing records (which is the most common case)
            do iFld = 1 to num-entries(pAction:CompareFields):
                if lookup(entry(iFld,pAction:CompareFields),cFieldReferences) = 0 then
                do:
                    if valid-object(pMessageBody) then 
                        return false.
                    else     
                        undo, throw new UnprocessableEntityError(subst("'POST' request for action '&1' must have a valid json object body",pAction:name)).
                end.    
            end.     
            // Note that most DS ignores this, but some may use it to improve performance  
            oTableRequest:SelectFields = pAction:CompareFields .
          
           
        end.
                 
        pBe:GetData(oGetRequest, output dataset-handle hDataset by-reference).
        if hDataset <> phDataset then
        do on error undo, throw:
            pRequestedBuffer:find-unique ().
            catch e as Progress.Lang.Error :
                undo, throw new TableNotFoundError(pRequestedBuffer,oGetRequest:TableRequests[1],e).
            end catch.
        end.                                                                                                                     
        else
            AssertUniqueRecords(hDataset, oGetRequest:TableRequests).
        
        // the default behavior is to set all fields to default except the Comparefields 
        // set row-state to row-created 
        // this may not really make sense, since we may read and want and existing record
        // so actions can override this
        // HOWEVER if it is overriden then we have a conflict with the use of select for the case with no messagebody
        // so throw not implemented for now
        // TODO - could just not use select in that case, but should probalby add an option/prop to control this 
        if not valid-object(pMessageBody) then 
        do:
            define variable oParameterList as ParameterList no-undo.  
            oParameterList = new ParameterList(1).
            oParameterList:setParameter(1,"handle","input",pRequestedBuffer).
            if valid-object(pAction:GetClass():GetMethod ("PostFindUnique",Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance,oParameterList)) then 
                undo, throw new NotImplementedError("Action with PostFindUnique override for Action without body. This conflicts with default use of select to only return CompareFields for the data. Please Contact IT if this is needed.") .
        
        end. 
        pAction:PostFindUnique(pRequestedBuffer).
        return true.                                                                                                                                                      
    end method.
    
    method protected void GetParentRowsForDynamicChild(pBe as IBusinessEntity, pClientUpdateRequest as IClientUpdateRequest, phDataset as handle):
        define variable oGetRequest      as IGetDataRequest no-undo. 
        oGetRequest = CreateRequestForUniqueAction(pClientUpdateRequest, phDataset, extent(pClientUpdateRequest:Collections) - 2).
        pBe:GetData(oGetRequest, output dataset-handle phDataset by-reference).
        AssertUniqueRecords(phDataset, oGetRequest:TableRequests).
    end method.
    
    // allow subclass override
    method protected IClientGetRequest CreateClientGetRequest(pcMethod as character, pcServiceUri as character, pUriQuery as IStringStringMap):
        return new ClientGetRequest(pcMethod, pcServiceUri,pUriQuery).
    end.    
    
    method protected logical JsonHasKeysOnly(pBusinessEntity as IBusinessEntity, pJsonRequest as JsonObject) :
         // default to not thwo any errors -  this is a courtesy - nothing should break if this is not resolved      
        return JsonHasKeysOnly(pBusinessEntity, pJsonRequest , false) .
    end method.
    
    method protected logical JsonHasKeysOnly(pBusinessEntity as IBusinessEntity, pJsonRequest as JsonObject, plThrowErrors as logical) :
        define variable cPublicKeys  as character no-undo. 
        define variable oJson        as JsonObject no-undo.
        define variable oJsonArray   as JsonArray no-undo.
        define variable i            as integer no-undo.
        define variable cNames       as character extent no-undo.
        define variable cCheckNames  as character extent no-undo.
        define variable lOnlyKeys    as logical no-undo.
        define variable cTopNames    as character extent no-undo. 
        define variable lHasResourceOnly as logical no-undo.
         
        cTopNames = pjsonRequest:GetNames().
        do i = 1 to extent(cTopNames): 
            // more than one top node return false 
            // todo allow meta props?   
            if lHasResourceOnly = true then 
                return false.
            lHasResourceOnly = (cTopNames[i] = pBusinessEntity:Resource).
        end.
            
        if lHasResourceOnly then 
        do: 
            cPublicKeys = pBusinessEntity:GetPublicKeys().
            if cPublicKeys > "" then 
            do:
                if pjsonrequest:GetType(pBusinessEntity:Resource) = JsonDataType:ARRAY then
                do: 
                    oJsonArray = pjsonRequest:GetJsonArray(pBusinessEntity:Resource).
                    // we spot check the first ones (making the assumption that )
                    if oJsonArray:length > 0 then
                    do:
                        oJson = oJsonArray:GetJsonObject(1).
                    end.
                end.
                else do:
                    oJson = pjsonRequest:GetJsonObject(pBusinessEntity:Resource).
                end.
                if valid-object(oJson) then
                do:
                    cNames = oJson:GetNames().
                    do i = 1 to extent(cNames):
                        // 
                        if lookup(cNames[i],cPublicKeys) = 0 then 
                            return false. 
                        lOnlyKeys = true.    
                    end.       
                end.
                if valid-object(oJsonArray) and lOnlyKeys then 
                do:
                    // paranoia ? - we only get here if the first object only has kesys
                    // so no vierhead for regular posts        
                    do i = 2 to oJsonArray:length: 
                        cChecknames = oJsonArray:GetJsonObject(i):GetNames().
                        if extent(cChecknames) <> extent(cNames) then
                            return false.       
                    end.     
                end.
                return lOnlyKeys.
            end.
        end.    
        return false.
        catch e as Progress.Lang.Error :
           if plThrowErrors then 
               undo, throw e.
       
        end catch. 
    end method.     
     
    // allow subclass override
    method protected IClientUpdateRequest CreateClientUpdateRequest(pBusinessEntity as IBusinessEntity, pcMethod as character, pcServiceUri as character, pUriQuery as IStringStringMap, pJsonRequest as JsonObject):
        return new ClientUpdateRequest(pcMethod, pcServiceUri,pUriQuery, JsonHasKeysOnly(pBusinessEntity,pJsonRequest)).
    end.
    
    /* check if the request is for a specific table in the dataset 
       returns true if a table is to be extracted from the dataset 
       throws error if any of the parents of the table that is to be extracted are not unique 
       public for test purposes. (ok since it is a utility of sorts)  
    */          
    method protected logical CheckTableRequestAndAssertUniqueParents(phDataset as handle,pClientRequest as IClientRequest,pTableRequests as IGetDataTableRequest extent, phRequestedBuffer as handle ):
        define variable iTable           as integer no-undo.
        define variable hBuffer          as handle  no-undo.
        define variable iCollection      as integer no-undo.
        define variable iCollectionTable as integer no-undo.
    
        if extent(pClientRequest:Collections) > 1 then
        do:
            do iTable = 1 to extent(pTableRequests)
            on error undo, throw.
               hBuffer = DatasetUtil:GetBufferByTableName(phDataset, pTableRequests[iTable]:TableName).
               if not valid-handle(hBuffer) then 
                   undo, throw new NotFoundError(subst("Table '&1' not found in dataset '&2'",pTableRequests[iTable]:TableName,phDataset:name)).
               
               // skip the collections that are not table references 
               if iTable = 1 then 
               do:
                  do iCollection = 1 to extent(pClientRequest:Collections):
                      if pClientRequest:Collections[iCollection] = hBuffer:serialize-name then 
                      do:
                         iCollectionTable = iCollection.
                         leave.
                      end.   
                  end.
                 
                  if iCollectionTable = 0 then
                  do:
                      return false.
                  end.    
               end.
               else 
                   iCollectionTable = iCollectionTable + 1.
               
               // we do not check uniqueness of the actual requested table - this is done later 
               if hBuffer = phRequestedBuffer then
               do: 
                   if iTable = 1 then 
                   do:
                        // if this is a specific requested table in a dataset with multiple buffers we may want to extract the table  
                       if phDataset:num-top-buffers > 1 then 
                       do:
                           // extract if there is no repos tables
                           if DatasetUtil:GetNumTopBuffers(phDataset) > 1 then 
                               return true.
                           // or the repos table is the requested table
                           else if valid-handle(hBuffer:parent-relation) then
                               return true.   
                       end.        
                       return false.     
                   end.
                   else 
                       return true.
               end.
               
               /* If there is no key for a prior(parent) table the request is likely wrong, but for this logic it works as long as it is unique
                  We could (in future allow just table in a URI if we know it is always unique in the context of its parent ()
               */    
               
               hBuffer:find-unique().
               
               catch e1 as NotFoundError :
                   undo, throw e1.
               end catch.
               catch e2 as Progress.Lang.Error :
                  
                   // may happen if static DatasetUUtil is out of synch 
                   if not valid-handle(hBuffer) then
                       undo, throw e2. 
                   // 3166 - more than one
                   // 138 - not on file 
                   if iCollectionTable > 0 and pClientRequest:KeyWheres[iCollectionTable] > "" then
                       //                                                                                    true = publicnames 
                       undo, throw new TableNotFoundError(hBuffer,pClientRequest:KeyWheres[iCollectionTable],true,e2).
                       
                   else do:
                       undo, throw new NotFoundError(subst("'&1'",hBuffer:serialize-name),e2).
                   end. 
               end catch.                                
               
            end.
        end.
        return false.
    end method.    
    
    // Assert that all requested tables are unique and return the last buffer for import of child data 
    method protected handle AssertUniqueRecords(phDataset as handle,pTableRequests as IGetDataTableRequest extent ):
        define variable iTable           as integer no-undo.
        define variable hBuffer          as handle  no-undo.
        define variable iCollection      as integer no-undo.
        define variable iCollectionTable as integer no-undo.
    
        do iTable = 1 to extent(pTableRequests)
        on error undo, throw.
            hBuffer = DatasetUtil:GetBufferByTableName(phDataset, pTableRequests[iTable]:TableName).
            if not valid-handle(hBuffer) then 
                undo, throw new NotFoundError(subst("Table '&1' not found in dataset '&2'",pTableRequests[iTable]:TableName,phDataset:name)).
               
               /* If there is no key for a prior(parent) table the request is likely wrong, but for this logic it works as long as it is unique
                  We could (in future allow just table in a URI if we know it is always unique in the context of its parent ()
               */    
            hBuffer:find-unique().
            
            catch e1 as NotFoundError :
                undo, throw e1.
            end catch.
            catch e2 as Progress.Lang.Error :
                 // may happen if static DatasetUUtil is out of synch 
                if not valid-handle(hBuffer) then
                    undo, throw e2. 
                   // 3166 - more than one
                   // 138 - not on file 
                if e2:GetMessageNum(1) = 138 or e2:GetMessageNum(1) = 3166 then
                do: 
                    undo, throw new TableNotFoundError(hBuffer, pTableRequests[iTable], e2).
                end.
                else 
                    undo, throw new NotFoundError(subst("'&1'",hBuffer:serialize-name),e2).
                   
            end catch.                                
             
        end.
        return hBuffer.
    end method.    
        
    method protected override character MethodsAllowedList(pObject as Object):
        define variable cAllowed as character no-undo.
        define variable cDlm     as character no-undo.
        define variable oEntity  as IEntity no-undo.
        cAllowed = "OPTIONS".
        cDlm = ", ".
        cAllowed = cAllowed + cDlm + "HEAD".
        cAllowed = cAllowed + cDlm + "GET".
        if type-of(pObject,IEntity) then
        do:
            oEntity = cast(pObject,IEntity).
            if not oEntity:ReadOnly then
            do:
                cAllowed  = cAllowed + cDlm + "PUT".
                if oEntity:CanCreate then
                    cAllowed  = cAllowed + cDlm + "POST".
                if oEntity:CanDelete then
                    cAllowed  = cAllowed + cDlm + "DELETE".     
            end.   
        end.
        return cAllowed.  
    end method.
    
    method protected character BuildUrlQuery (pQueryMap as IStringStringMap,pProps as char extent):
        define variable cQueryString as character no-undo.
        define variable cDelimiter   as character no-undo.
        define variable i            as integer no-undo.
        define variable cValue       as character no-undo.
        if pQueryMap:Size gt 0 then
        do:
            cDelimiter = ?.
            do i = 1 to extent(pProps):
                cValue = pQueryMap:Get(pProps[i]).
                if cValue <> ? then 
                do:
                    cQueryString = cQueryString + substitute('&1&2=&3':u, cDelimiter,pProps[i],cValue).
                    cDelimiter   = '&'
                    .
                end.    
            end.
            
        end.
        return cQueryString.
    end.      
    
    method protected character BuildUrlQuery (pQueryMap as IStringStringMap):
        define variable cList        as character no-undo.
        define variable oKeyValues   as ICollection no-undo.
        define variable oIterator    as IIterator no-undo.
        define variable cKey         as character no-undo.
        define variable cValue       as character no-undo.
        define variable cDelimiter   as character no-undo.
        define variable cQueryString as character no-undo.
        
        oKeyValues = pQueryMap:KeySet.
        oIterator  = oKeyValues:Iterator().
        
        do while oIterator:HasNext():
            cKey =  cast(oIterator:Next(), String):Value.
            cValue = pQueryMap:Get(cKey).
            if cValue <> ? then 
            do:
                cQueryString = cQueryString + substitute('&1&2=&3', cDelimiter,cKey,cValue).
                cDelimiter   = '&'
               .
            end. 
        end.
        return cQueryString.
    end.
          
    /**
    Handle selected option to specify which fields to return. 
    Supports qualified columns for child tables.
    All fields not listed for a specific table will be hidden 
    No fields will be hidden if a table is not referenced at all - cannot use this option to hide all fields... 
    **/
    method protected handle extent HideColumns (phDataset as handle,pcMainBuffer as char, pSelected as char, pLinks as ICollection, pPropname as char, plHidelaterNotNeeded as logical, output pcHideLater as char): 
        define variable i            as integer no-undo.
        define variable hFields      as handle extent no-undo.
        define variable hFld         as handle  no-undo.
        define variable iLookup      as integer no-undo.
        define variable cBadNames    as character no-undo.
        define variable cBadName     as character no-undo.
        define variable hBuffer      as handle no-undo.
        define variable cSerializeBuffer as character no-undo.
        define variable cSerializeList as character no-undo.
        
        define variable iBuffer      as integer no-undo.
        define variable cName        as character no-undo.
        define variable lAll         as logical no-undo.
      
        define variable iCountfields as integer no-undo.
        define variable iNumbuffers  as integer no-undo.
        define variable cField       as character no-undo.
        define variable cMainSerializename as character no-undo.
        define variable cKeys        as character no-undo.
        define variable oIterator    as IIterator no-undo.
        define variable oRelation    as IRelationDefinition no-undo.
        define variable cForeignKeys as character no-undo.
        define variable cForeignKey  as character no-undo.
        define variable iField       as integer no-undo.
        define variable cPairs       as character no-undo.
        define variable cRecursiveKeys as character no-undo.
        define variable iRel           as integer no-undo.
        define variable cDeactivatedBuffers as character no-undo.
        
        if index(pSelected,".") > 0 then
        do:
            if pcMainbuffer > "" then
                cMainSerializename = phDataset:get-buffer-handle(pcMainBuffer):serialize-name.
            
            // as of current we throw error if select references a deactivated link 
            // this is a bit restrictive and rwequires that expand and select is in synch 
            // This can be reconsidered if problematic
            cDeactivatedBuffers = DatasetUtil:GetDeactivatedSerializeNames(phDataset).
             
            // build a list of referenced buffers 
            do i = 1 to num-entries(pSelected):
                cField = entry(i,pSelected).
                if num-entries(cField,".") = 1 then
                do:
                    if cMainSerializeName = "" then
                    do:
                         if pPropName > "" then
                             undo, throw new BadRequestError(subst("&1=&2 Property must be qualified",pPropname,cField)).
                         else 
                             undo, throw new IllegalArgumentError(subst("Field '&1' in selected parameter must be qualified",cField)).
                       
                    end.
                    cSerializeBuffer =  cMainSerializename.
                end.
                else do:
                    cSerializeBuffer = entry(1,cField,".").
                    if lookup(cSerializebuffer,cDeactivatedBuffers) > 0 
                    or not valid-handle(DatasetUtil:GetBufferBySerializeName(phDataset, cSerializeBuffer)) then
                    do:
                        if pPropName > "" then
                             undo, throw new BadRequestError(subst("Entity reference '&1' in &2=&3 is invalid",cSerializeBuffer,pPropname,cField)).
                        else 
                             undo, throw new IllegalArgumentError(subst("Entity reference '&1' in Field '&1' is invalid",cSerializeBuffer,cField)).
                    end.         
                end.  
                if cSerializeBuffer > "" and lookup(cSerializeBuffer,cSerializeList) = 0 then
                    cSerializeList = cSerializeList 
                                + (if cSerializeList = "" then "" else ",")
                                + cSerializeBuffer.   
                // no need to look ffurther if all tables are referenced                
                if num-entries(cSerializeList) = phDataset:num-buffers then 
                    leave.                
            end.    
            
            // count the fields in the referenced buffer and size the extent 
            // NOTE: it is going to be too big, but the unhideColumns is prepared for that   
            do iBuffer = 1 to phDataset:num-buffers:
                hBuffer = phDataset:get-buffer-handle(iBuffer).
                if lookup(hBuffer:serialize-name,cSerializeList) > 0 then
                    iCountfields = iCountFields + hBuffer:num-fields.
            end.
            extent(hFields) = iCountFields.
        end.
        else 
        do: // no qualified columns - all are for the main buffer 
            // use serialize-name in bufferlist so we can use the same loop
            // for both cases 
            if pcMainBuffer = "" then 
            do:
                if pPropName > "" then
                    undo, throw new BadRequestError(subst("&1=&2",pPropname,pSelected)).
                else 
                    undo, throw new IllegalArgumentError(subst("Selected '&1' parameter is invalid",pSelected)).
            end.     
                
            hBuffer = phDataset:get-buffer-handle(pcMainbuffer).
            extent(hFields) =  hBuffer:num-fields.
            cSerializeList = hBuffer:serialize-name.
        end.
        
        //  build a list of referenced foreign key for many to one relations 
        //  as they are needed to add links to json later and cannot be hidden here 
        if valid-object(pLinks) and pLinks:Size > 0 then
        do:
            oIterator = pLinks:Iterator().
            do while oIterator:HasNext().
                oRelation = cast(oIterator:Next(),IRelationDefinition).
                if lookup(oRelation:ParentName,cSerializeList) > 0 then
                do:
                    if not oRelation:Cardinality:Equals(RelationTypeEnum:OneToMany)  then 
                    do:
                        if type-of(oRelation,IRelation) then
                        do:
                            // foreignkey is used only when only one field since we do not need to know the field name  
                            if cast(oRelation,IRelation):ForeignKey > "" then
                            do :
                                cForeignKeys = cForeignKeys 
                                             + (if cForeignKeys = "" then "" else ",")
                                             + (if oRelation:parentname = cMainSerializeName then "" else oRelation:parentname + ".")
                                             + cast(oRelation,IRelation):ForeignKey.   
                            end.    
                            else do :
                               cPairs = cast(oRelation,IRelation):PairsList().
                               do iField = 1 to num-entries(cPairs) by 2:
                                   cForeignKey = entry(iField,cPairs).
                                   cForeignKeys = cForeignKeys 
                                             + (if cForeignKeys = "" then "" else ",")
                                             + (if oRelation:parentname = cMainSerializeName then "" else oRelation:parentname + ".")
                                             + cForeignKey. 
                               end.     
                            end.
                        end.    
                    end.           
                end.    
            end.
        end.    
       
        do iBuffer = 1 to phDataset:num-buffers:
            hBuffer = phDataset:get-buffer-handle(iBuffer).
            if lookup(hBuffer:serialize-name,cSerializeList) > 0 then
            do: 
                cRecursiveKeys = "".
                // recursive relations need to keep the recursive key for the JSonRecursiveRleationProcessor  
                // the current assumption is that there is only one relation on such a case
                relloop:
                do iRel = 1 to hBuffer:num-child-relations:
                    if hBuffer:get-child-relation(iRel):recursive then
                    do:
                        cPairs = hBuffer:get-child-relation(iRel):relation-fields.
                        do iField = 2 to num-entries(cPairs) by 2:
                            cForeignKey = entry(iField,cPairs).
                            cRecursiveKeys = cRecursiveKeys 
                                           + (if iField = 2 then "" else ",")
                                           + cForeignKey. 
                        end. 
                        leave relloop. // only one recursive relation in dataset
                    end.        
                end.
                cKeys = ServiceInfo:GetKeys(hBuffer).
                do i = 1 to hBuffer:num-fields:
                    hFld = hBuffer:buffer-field(i).
                    if hFld:serialize-hidden = false then 
                    do:              
                        cName = if hBuffer:name = pcMainBuffer 
                                then hfLD:serialize-name
                                else hBuffer:serialize-name + "." + hFld:serialize-name.   
                        iLookup = lookup(cName,pSelected).
                        if iLookup = 0 then 
                        do:
                            if plHidelaterNotNeeded = false
                            and (lookup(hFld:name,cKeys) > 0 
                                 or lookup(if hBuffer:name = pcMainBuffer then hfLD:name
                                      else hBuffer:serialize-name + "." + hFld:name,cForeignKeys) > 0
                                 or lookup(hFld:name,cRecursiveKeys)  > 0)  then
                            do:
                                pcHideLater = pcHideLater
                                            + (if pcHideLater = "" then "" else ",")
                                            + cName.
                            end.
                            else do:
                                hFld:serialize-hidden = true.
                                hFields[i] = hFld.
                            end.
                        end.
                        else // keep track for error handling below
                            entry(iLookup,pSelected) = "".                
                    end.
                end.
            end.
        end.
        pSelected = trim(pSelected,",").
        
        // some fields are not valid
        if pSelected <> "" then 
        do:
            do i = 1 to num-entries(pSelected):
                cBadName = entry(i,pSelected).
                if cBadName > "" then 
                do:
                    cBadNames = cBadNames 
                              + (if cBadNames > "" then "," else "")
                              + cBadName.
                               
                end.   
            end.    
            undo, throw new BadRequestError(subst("&1=&2",pPropname,cBadnames)).
        end.
              
        return hFields.    
    end.
    
    method protected handle UnhideColumns (hFlds as handle extent): 
        define variable i as integer no-undo.
        do i = 1 to extent(hflds):
            if valid-handle(hFlds[i]) then
            do:
                hFlds[i]:serialize-hidden = false.
            end.
        end.     
    end.
    
    // convert a single row json to dataset readable format 
    method protected JsonObject GetJsonRowRequest(pClientUpdateRequest as IClientUpdateRequest, phDataset as handle, pJsonClientRequest as JsonObject ):
        define variable oJsonWrapper as JsonObject no-undo.
        define variable oJsonArray   as JsonArray  no-undo.
        define variable oJsonRequest as JsonObject no-undo.
        
        // wrapper for the request that will be added in an array
        oJsonWrapper = new JsonObject().
        // add an array in the wrapper 
        oJsonArray   = new JsonArray().
        oJsonWrapper:Add(pClientUpdateRequest:Collections[extent(pClientUpdateRequest:Collections)],oJsonArray).
        // add the request to the array that read-json expects for table 
         
        oJsonArray:Add(pJsonClientRequest).
        // we need the data set wrapper since the serialize-name of the single top table is similar  
        // so read-json will assume it is the dataset if we only have the table array
        oJsonRequest = new JsonObject().
        oJsonRequest:Add(phDataset:serialize-name,oJsonWrapper).
        return oJsonRequest.
    end method.    
    
    method protected override integer WriteAllowedHeader(pObject as Object, poResponse as IHttpResponse, pStatusCodeEnum as StatusCodeEnum):
        define variable oHeader as HttpHeader no-undo.
        oHeader = HttpHeaderBuilder:Build('Allow':u):Value(MethodsAllowedList(pObject)):Header.
        poResponse:SetHeader(oHeader).
        return ReturnStatusNoBody(poResponse,pStatusCodeEnum).
    end method. 
  
end class.