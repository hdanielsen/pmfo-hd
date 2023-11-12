 
 /*------------------------------------------------------------------------
    File        : SwaggerJsonWriter = copied from OpenAPI30ServiceWriter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Feb 27 10:41:04 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.BusinessLogic.NamedQuery from propath.
using OpenEdge.Core.SemanticVersion from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.Common.DataService from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Repository.Business.ResourceBE from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Object from propath.

// not much to inherit from here currently - this may change,   
// OpenEdge.Web.DataObject.Writer.OpenAPI30ServiceWriter -> OpenEdge.Web.DataObject.Writer.DataObjectServiceWriter
// since we need to override the entire WriteServiceRoot due to some hardcoded cases
// also the hijach of tags in writeOperations does not seem to fit igensis (?)   
// 
class Pmfo.Core.Service.Writer.SwaggerJsonWriter abstract inherits DataService: 
    {Pmfo/Repository/schema/entity.i}
      
    define protected abstract property description as character no-undo get.
    define protected abstract property title  as character no-undo get.
    
    define protected property ServiceManager as IServiceManager no-undo 
       get():
            return cast(Application:ServiceManager,IServiceManager).
       end.
    
    define public property Value as Object get. protected set. 
    
        
    define protected property termsOfservice as character no-undo
        init "http://swagger.io/terms/"  
        get.   
     
    method protected abstract JsonArray WriteServers():
    method protected abstract JsonObject WriteApiKeyAuth(). 
    
    method public void Write(  ): // this is Open in OpenAPI30ServiceWriter
        // If nothing's passed in, create an instance of the Argument Type
        if not valid-object(this-object:Value) then
            assign this-object:Value = WriteServiceRoot().
        
    end method.
    
    // copied largely from netlib OpenEdge.Web.DataObject.Writer.OpenAPI30ServiceWriter
    // the version has hardcoded cases that we can't use
    method protected JsonObject WriteServiceRoot():
        define variable svcJson as JsonObject no-undo.
        define variable jsonData as JsonObject no-undo.
        define variable childData as JsonObject no-undo.
        define variable arrayData as JsonArray no-undo.
        define variable varData as JsonObject no-undo.
        
        assign svcJson   = new JsonObject().
        
        svcJson:Add('openapi':u,      new SemanticVersion(3,0,1):ToString()).      //REQUIRED. This string MUST be the semantic version number of the OpenAPI Specification version that the OpenAPI document uses. The openapi field SHOULD be used by tooling specifications and clients to interpret the OpenAPI document. This is not related to the API info.version string.
        svcJson:Add('info':u,         WriteInfo()).
        svcJson:Add('externalDocs':u, WriteExternalDocs()).                 //Additional external documentation.                              
        svcJson:Add('servers':u,      WriteServers()).         //An array of Server Objects, which provide connectivity information to a target server. If the servers property is not provided, or is an empty array, the default value would be a Server Object with a url value of /.
        svcJson:Add('tags':u,         WriteTags()).   //A list of tags used by the specification with additional metadata. The order of the tags can be used to reflect on their order by the parsing tools. Not all tags that are used by the Operation Object must be declared. The tags that are not declared MAY be organized randomly or based on the tools' logic. Each tag name in the list MUST be unique.
        svcJson:Add('security':u,     WriteSecurity()).   //A declaration of which security mechanisms can be used across the API. The list of values includes alternative security requirement objects that can be used. Only one of the security requirement objects need to be satisfied to authorize a request. Individual operations can override this definition.
        svcJson:Add('paths':u,        WritePaths()).              //REQUIRED. The available paths and operations for the API.
        svcJson:Add('components':u,   WriteComponents()).              //An element to hold various schemas for the specification.
        return svcJson.
    end method.
    
    method protected JsonObject WriteInfo():
        define variable infoData as JsonObject no-undo.
        
        assign infoData = new JsonObject().
        //string  Required. The title of the application.
        infoData:Add('title':u, this-object:title).
        //string  A short description of the application. GFM syntax can be used for rich text representation.
        infoData:Add('description':u, description).
        
        //string  The Terms of Service for the API.
        infoData:Add('termsOfService':u, termsOfservice).
        
        //Object  The contact information for the exposed API.
        infoData:Add('contact':u, WriteContact()).
        
        //Object  The license information for the exposed API.
        infoData:Add('license':u, WriteLicense()).
       
        // The version of the OpenAPI document (which is distinct from the OpenAPI Specification
        // version or the API implementation version).
        infoData:Add('version':u, '1.0.0':u).   
        
        return infoData.
    end method.
    
    method protected JsonObject WriteContact():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add('name':u, 'Precision Strip':u).
        oJson:Add('url':u, 'https://www.precision-strip.com':u).
        oJson:Add('email':u, 'IT_Help@precision-strip.com':u).
        return oJson.
    end method.    
    
    method protected JsonObject WriteComponents():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:add('schemas',WriteSchemas()).
        oJson:Add('securitySchemes':u, WriteSecuritySchemes()).
        return oJson.
    end method.    
    
    method protected JsonObject WriteSchemas():
        return new JsonObject(). 
    end method.    
    
    method protected JsonObject WriteSecuritySchemes():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add('ApiKeyAuth':u, WriteApiKeyAuth()).
        return oJson.
    end method.    
    
    method protected JsonArray WriteSecurity():
        define variable oJsonArray as JsonArray no-undo.
        define variable oJson as JsonObject no-undo.
        oJsonArray = new JsonArray().
        oJson = new JsonObject().
        oJson:Add('ApiKeyAuth':u,new JsonArray()).
        oJsonArray:Add(oJson).
        return oJsonArray.
    end method.    
    
    
    method protected JsonObject WriteExternalDocs():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add('description':u, 'Find out more about Swagger':u).
        oJson:Add('url':u, 'http://swagger.io':u).
        return oJson.
    end method.    
    
    method protected JsonObject WriteLicense():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add('name':u, 'Apache 2.0':u).
        oJson:Add('url':u, 'http://www.apache.org/licenses/LICENSE-2.0.html':u).
        return oJson.
    end method.    
   
    method protected JsonObject WritePaths():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        AddPaths(oJson).
        return oJson.
        
    end method.     
    
     method protected JsonArray WriteTags():
        define variable oJson as JsonArray no-undo.
        oJson = new JsonArray().
        AddTags(oJson).
        return oJson.
        
    end method.     
    
    method protected void AddTags(pTag as JsonArray):
        define variable cSingularName as character no-undo.
        if not can-find(first ttEntity) then
        do: 
            FetchResources(output table ttEntity).
        end.
        for each ttEntity where ttEntity.Typename matches "*.IBusinessEntity" :
            cSingularName = BufferUtil:SingularizeSerializeName(ttEntity.Resource,true).
            pTag:Add(WriteResourceTag(ttEntity.resource)). 
        end.    
    end method.     
    
    // key for tags and path to ensure the paths shows up under each entity added in tags 
    method protected character GetEndpointName(pcResource as character):
        return BufferUtil:SingularizeSerializeName(pcResource,true).
    end.    
     
    method protected JsonObject WriteResourceTag(pcResource as character):
        define variable oJson as JsonObject no-undo.
      
        oJson = new JsonObject().
        oJson:Add('name',GetEndpointName(pcResource)).
        // Seems the reource plural name with uppercase of first makes sense here (?) 
        oJson:Add('description',subst("Everything about &1",StringUtil:UpperCaseFirst(pcResource))).
        return ojson.
    end method.    
   
     
    method protected void AddPaths(pPath as JsonObject):
        define variable oBe as IBusinessEntity no-undo.
        if not can-find(first ttEntity) then
        do: 
            FetchResources(output table ttEntity).
        end.
        for each ttEntity where ttEntity.Typename matches "*.IBusinessEntity" :
            oBE = ServiceManager:CreateBusinessEntity(ttEntity.Resource). 
            AddResourcePaths(pPath,oBE).
          //  pPath:Add(subst("/&1",ttEntity.Resource),WriteResource(buffer ttentity)). 
        end.    
    end method.     
    
    // allow subclass to override
    method protected character extent GetPathKeys(phbuffer as handle):
        return BufferUtil:GetUniqueIndexes(phBuffer, false).
    end method.     
    
    method protected JsonObject AddResourcePaths(pPath as JsonObject,pBe as IBusinessEntity):
        define variable hbuffer  as handle no-undo.
        define variable cKeys    as character no-undo.
        define variable i        as integer no-undo.
        define variable hFlds    as handle extent no-undo.
        define variable cName    as character no-undo.
        define variable cParam   as character no-undo.
        define variable cIndexes as character extent no-undo.
        define variable iIndex  as integer no-undo.
        define variable lNoname as logical no-undo.
        // add general path for collection
        pPath:Add(subst("/&1",ttEntity.Resource),WriteResourceCollection(pBe)). 
        
        hBuffer = pBe:Datasets[1]:get-buffer-handle(1).
        
        cIndexes = GetPathKeys(hbuffer).
        //cKeys = BufferUtil:GetKeys(hBuffer).
        do iIndex = 1 to extent(cIndexes):
            // aadd entry point for primary key
            cKeys = cIndexes[IIndex].
            //  no name needed in url for primary key and one field 
            lNoName = num-entries(cKeys) = 1 and cKeys = ServiceInfo:GetKeys(hBuffer).   
            if ckeys <> "rowid" and cKeys <> "handle" and cKeys > "" then
            do:
                extent(hFlds) = num-entries(cKeys).
               // add path for keys 
                do i = 1 to num-entries(cKeys):
                   hFlds[i] = hbuffer:buffer-field(entry(i,cKeys)).
                   cName = hFlds[i]:serialize-name.
                   if lNoName then
                   do:
                       cParam = if hflds[i]:data-type = "character" or hFlds[i]:data-type begins "date" then 
                                   quoter(subst("~{&1~}",cName),"'")
                                   else subst("~{&1~}",cName)  .  
                   end.     
                   else do:
                       cParam = cParam 
                              + (if i = 1 then "" else ",")
                              + cName + "="
                              + if hflds[i]:data-type = "character" or hFlds[i]:data-type begins "date" then 
                                   quoter(subst("~{&1~}",cName),"'")
                                   else subst("~{&1~}",cName)  . 
                   end.    
                end.
                pPath:Add(subst("/&1(&2)",ttEntity.Resource,cParam),WriteResourceObject(pBe, hFLds)). 
            end.
        end.    
    end method.    
    
    
    method protected JsonObject WriteResourceCollection(pBe as IBusinessEntity):
        define variable oJson as JsonObject no-undo.
        define variable cResource as character no-undo.
        define variable hdummy as handle extent no-undo.
        cResource = GetEndpointName(pBe:Resource). // link to tags make sure name is consistent 
        oJson = new JsonObject().
        oJson:Add('summary',subst("&1 data",cResource)).
        oJson:Add('get',WriteGetResource(pBe,cResource,true,"",hDummy)).
        return ojson.
    end method. 
    
    method protected JsonObject WriteResourceObject(pBe as IBusinessEntity, pkeyfields as handle extent):
        define variable oJson     as JsonObject no-undo.
        define variable cResource as character no-undo.
        define variable iFld      as integer no-undo.
        define variable cLabels   as character no-undo.
        define variable cDlm      as character no-undo. 
        cResource = GetEndpointName(pBe:Resource). // link to tags make sure name is consistent 
        oJson = new JsonObject().
        
        do iFld = 1 to extent(pkeyfields):
            assign
                cLabels = cLabels + cDlm + BufferUtil:GetFieldLabel(pKeyfields[iFld]). 
                cDlm = ",".
                     
        end.
        cLabels = StringUtil:GetMessageFromEntries(cLabels).
        oJson:Add('summary',subst("Get &1 by &2",cResource,cLabels)).
        
        oJson:Add('get',WriteGetResource(pBe,cResource,false,cLabels,pKeyfields)).
        return ojson.
    end method. 
    
    
    method protected JsonObject WriteGetResource(pBe as IBusinessEntity,pResource as character, plCollection as logical, pcLabel as character,phKeys as handle extent):
        define variable oJson as JsonObject no-undo.
        define variable oJsonTags as JsonArray no-undo.
        oJson = new JsonObject().
        oJsonTags = new JsonArray().
        oJsonTags:Add(pResource).
        oJson:Add("tags",oJsonTags).
        if plCollection then
            oJson:Add('summary',subst("Get &1 data",pResource)).
        else 
            oJson:Add('summary',subst("Get &1 record by &2",pResource, if pcLabel > "" then pcLabel else "key")).
            
        oJson:Add('description','').
        oJson:Add('operationId','').
        if plCollection then 
        do:
            oJson:Add('parameters',WriteGetCollectionParameters(pBe)).
        end.
        else  
        do:
            oJson:Add('parameters',WriteGetObjectParameters(pBe,phKeys)).
        end.   
        oJson:Add('responses',WriteGetResponses(plCollection)).
        return ojson.
    end method.
    
    method protected JsonArray WriteGetCollectionParameters( pBe as IBusinessEntity):
        define variable oJsonArray as JsonArray no-undo.
        oJsonArray = new JsonArray().
         
        oJsonArray = new JsonArray().
        
        oJsonArray:Add(writeGetParameter("top","query","Max Number of Records",false,"integer","20")).
        oJsonArray:Add(writeGetParameter("filter","query","Filter",false,"character","")).
        oJsonArray:Add(writeGetParameter("select","query","Select Fields",false,"character","")).
        oJsonArray:Add(writeGetParameter("orderBy","query","Sort Fields",false,"character","")).
        oJsonArray:Add(writeGetParameter("metadata","query","Meta Data",false,"character","none")).
        
        return oJsonArray.
             
    end method.
    
    method protected JsonArray WriteGetObjectParameters( pBe as IBusinessEntity, phKeys as handle extent):
        define variable oJsonArray as JsonArray no-undo.
        define variable cKeys as character no-undo.
        define variable hBuffer as handle no-undo.
        define variable hFLd as handle no-undo.
        define variable i as integer no-undo.
        define variable cDataType as character no-undo.
        oJsonArray = new JsonArray().
        do i = 1 to extent(phKeys):
            oJsonArray:Add(writeGetParameter(phKeys[i]:serialize-name,"path",BufferUtil:GetFieldLabel(phKeys[i]),true,phKeys[i]:data-type,"")).
        end.    
        
        oJsonArray:Add(writeGetParameter("metadata","query","Meta Data",false,"character","none")).
        
        return oJsonArray.
             
    end method.
   
    
    method protected JsonObject WriteGetParameter(pname as character,pIn as character,pDescription as character,plRequired as logical,pType as character, pcExample as char):
        define variable oJson as JsonObject no-undo.
        define variable oJsonSchema as JsonObject no-undo.
         
        oJson = new JsonObject().
        oJsonSchema = new JsonObject().
        oJson:Add("name",pname).
        oJson:Add("in",pIn).
        oJson:Add("description",if pDescription > "" then pDescription else pname).
        oJson:Add("required",plRequired).
        case pType:
            when "integer" then
            do: 
               if pcExample > "" then
                   oJson:Add("example",integer(pcExample)).
               oJsonSchema:Add("type",lc(pType)).
               oJsonSchema:Add("format","int64").
            end.
            when "character" then
            do: 
               if pcExample > "" then
                   oJson:Add("example",pcExample).
               oJsonSchema:Add("type","string").
            end. 
            when "logical" then
            do: 
               if pcExample > "" then
                   oJson:Add("example",logical(pcExample)).
               oJsonSchema:Add("type","boolean").
            end.
            when "decimal" then
            do: 
               if pcExample > "" then
                   oJson:Add("example",decimal(pcExample)).
               oJsonSchema:Add("type","number").
               oJsonSchema:Add("format","float"). // double?? 
            end.
            when "date" then
            do: 
               if pcExample > "" then
                   oJson:Add("example",decimal(pcExample)).
               oJsonSchema:Add("type","string").
               oJsonSchema:Add("format","date").  
            end.  
            // this may be pointless, but to avoid error if we get here
            when "handle" then  
            do: 
               if pcExample > "" then
                   oJson:Add("example",int64(pcExample)).
               oJsonSchema:Add("type","integer").
               oJsonSchema:Add("format","int64").  
            end.   
            // this is likely pointless, but to avoid error if we get here
            when "rowid" then  
            do: 
               if pcExample > "" then
                   oJson:Add("example",pcExample).
               oJsonSchema:Add("type","string").
            end.   
            
            when "datetime" or when "datatime-tz" then
            do: 
               if pcExample > "" then
                   oJson:Add("example",decimal(pcExample)).
               oJsonSchema:Add("type","string").
               oJsonSchema:Add("format","date-time").  
            end.  
            otherwise
                oJsonSchema:Add("type",lc(pType)).
            
        end.     
        oJson:Add("schema",oJsonSchema).
        
        return oJson.
             
    end method.
   
    method protected JsonObject WriteGetResponses(plCollection as logical):
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        AddResponseStatus(oJson,StatusCodeEnum:OK).
        if not plCollection then 
           AddResponseStatus(oJson,StatusCodeEnum:NotFound).
     //   AddResponseStatus(oJson,StatusCodeEnum:BadRequest). 
     //   AddResponseStatus(oJson,StatusCodeEnum:InternalServerError).
        oJson:Add("default",WriteDefaultResponse()). 
        return oJson.
    end method.
    
    
    method protected void AddResponseStatus(pJson as JsonObject, pEnum as StatusCodeEnum):
        pJson:Add(string(pEnum:GetValue()),WriteGetResponse(penum)). 
    end method.
    
    method protected JsonObject WriteDefaultResponse():
       define variable oJson as JsonObject no-undo.
         oJson = new JsonObject().
       oJson:Add("description","Default error response"). 
       return oJson.
        
    end method.
   
    
    method protected JsonObject WriteGetResponse(pEnum as StatusCodeEnum):
       define variable oJson as JsonObject no-undo.
       define variable oJsonContent as JsonObject no-undo.
       define variable oJsonContentType as JsonObject no-undo.
       oJson = new JsonObject().
       oJsonContent = new JsonObject().
       oJsonContentType = new JsonObject().
       oJsonContent:add("application/json",oJsonContentType). 
       oJson:Add("description",StringUtil:CamelToWords(penum:ToString())). 
       oJson:Add("content",oJsonContent).
         
        
       return oJson.
        
    end method.
   
    method protected JsonArray LoadPlantCities() :
        define variable oBe as IBusinessEntity no-undo.
        define variable hDataset as handle no-undo.
        oBe = ServiceManager:CreateBusinessEntity("plants").
        oBe:GetData(output dataset-handle hDataset).
         
    end method.
    
    method protected void FetchResources(output table-handle phTable):
        define variable hDataset as handle no-undo.
        define variable oGetDataRequest as IGetDataRequest no-undo.
        define variable oResourceBe as ResourceBE no-undo.
        define variable hTable as handle no-undo.
        // use CreateBusinessEntity method to allow subclass override  
        //if valid-object(ResourceBE) then 
        //    delete object ResourceBE. 
            
        oResourceBE = cast(ServiceManager:CreateBusinessEntity("resources"),ResourceBE).
        oGetDataRequest = new GetDataRequest(new NamedQuery("JSON")).
        oResourceBE:GetData(oGetDataRequest,output dataset-handle hDataset).
        phTable = hDataset:get-buffer-handle("ttentity"):table-handle.
    end method.    
   
    
end class.