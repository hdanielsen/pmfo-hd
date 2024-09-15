 
/*------------------------------------------------------------------------
   File        : OpenAPIWriter = copied from OpenAPI30ServiceWriter
   Purpose     : 
   Syntax      : 
   Description : 
   Author(s)   : hdaniels
   Created     : Sun Feb 27 10:41:04 EST 2022
                 June 4th 2024 Renamed to OpenAPIWriter
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
using OpenEdge.Core.Collections.IMap from propath.
using Pmfo.Util.DatasetUtil from propath.
using Psi.Core.Manager.ISessionManager from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.Common.IFunction from propath.
using OpenEdge.Core.Collections.IMapEntry from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.Common.ISignature from propath.
using OpenEdge.Core.DataTypeEnum from propath.
using Pmfo.Core.BusinessLogic.MethodBindingEnum from propath.
using OpenEdge.Core.DataTypeHelper from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Progress.Reflect.Method from propath.
using Pmfo.Util.Array from propath.
using Progress.Reflect.Parameter from propath.
using Pmfo.Core.Service.OpenAPISchemaNameEnum from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.BusinessLogic.IDynamicChild from propath.
 

// not much to inherit from here currently - this may change,   
// OpenEdge.Web.DataObject.Writer.OpenAPI30ServiceWriter -> OpenEdge.Web.DataObject.Writer.DataObjectServiceWriter
// since we need to override the entire WriteServiceRoot due to some hardcoded cases
// also the hijach of tags in writeOperations does not seem to fit igensis (?)   
// 
class Pmfo.Core.Service.Writer.OpenAPIWriter abstract inherits DataService: 
    {Pmfo/Repository/schema/entity.i}
    define protected property FilterEntities as character  no-undo get. set.
    
    
    define temp-table ttFixedMember  no-undo
        field key as character format "x(35)"
        field memberObject as Object
        field hasChildAction   as logical
        field hasChildFunction as logical
        field hasContainerAction   as logical
        field hasContainerFunction as logical
        index key as primary unique key. 
    
    // some child relations may be also be parent on of a fied member
    // this is added once and cecked when the links are added to path
    // 
     
    // Used to sort first path segment after resource alphabetically 
    // stolen from LinkRouter, but with isaction to allow actions and functions 
    // at the same time use logical to sort functions first 
    define temp-table ttMember  no-undo 
        field key as character format "x(35)"
        field isAction       as logical // only used for methods - used to allow two records woth same key since actions and functions can have the same a name   
        // for linked members store the real EntityName to join to ttFixedLinkMemeber  
        field sourceEntity   as character format "x(35)"
        field memberObject   as Object // method object if applicable
        field isBound        as logical init ? // init ? for cases where it is not in use 
                                              // need key from parent - not in use for IsLinkedmethod, which is a place holder that may have both 
        field IsLinkedMethod as logical // created from ttFixedMember to be sorted with other meebers 
        field cardinality    as Object  // used for regular links  
            // actions and function can have same name (should really be method  like post or get)
        index key as primary unique key isAction . 
    
    // merge with table above? 
    define temp-table ttLinkedMember  no-undo 
        field parentKey as character format "x(35)"
        field childKey as character format "x(35)"
        field key as character format "x(35)"
        field memberObject as Object
        field isBound      as logical  // need key of parent
        field isAction     as logical 
            // actions and function can have same name (should really be method  like post or get)
        index key as primary unique parentKey childKey key isAction.
         
     //same as ttLinlkedmember. but separate since it creat4d once and never deleted      
    define temp-table ttFixedLinkedMember like ttLinkedMember. 
      
    define protected property schemas  as JsonObject no-undo get. set.
    // TODO - define as abstract in psi version of SwaggerJsonWriter 
    define protected abstract property ProductName as character  no-undo get.
    
    // Set in subclass constructor (a property woth no getter is fast and we check this for every class)
     // The current framework does not have a good way to detect 
    // this since CanCreate, CanDelete CanUpdate  may be defined as true
    // for cases where they really only support functions or actions  
    // (The BEOH may do fuhther checking)
    define protected property MethodOnlyEntities as character  no-undo get. set.
    
    define protected property SCHEMAS_REFERENCE as character no-undo 
        init "#/components/schemas"
        get.
     
     // TODO - move to psi version of SwaggerJsonWriter
    define protected property title as character no-undo
        get ():
            return subst("&1 API",ProductName).
        end.
        
    // TODO - move to psi version of SwaggerJsonWriter
    define protected property description as character no-undo
        get ():
            return subst("This is the &1 server. You can find out more about Swagger at [http://swagger.io](http://swagger.io) or on [irc.freenode.net, #swagger](http://swagger.io/irc/).",
                ProductName 
                ). 
        end get.
       
    define protected property ServiceManager as IServiceManager no-undo 
        get():
            return cast(Application:ServiceManager,IServiceManager).
        end.
    define protected property SessionManager as ISessionManager no-undo 
        get():
            return cast(Application:SessionManager,ISessionManager).
        end.
    define public property Value as Object get. protected set. 
    
    define protected property termsOfservice as character no-undo
        init "http://swagger.io/terms/"  
        get.   

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    constructor public OpenAPIWriter (  ):
        super ().
    end constructor.
    
    constructor public OpenAPIWriter ( pcEntities as character ):
        super(). 
        FilterEntities = pcEntities.
    end constructor.
     
      
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
        
        svcJson:Add('openapi':u,      new SemanticVersion(3,1,0):ToString()).      //REQUIRED. This string MUST be the semantic version number of the OpenAPI Specification version that the OpenAPI document uses. The openapi field SHOULD be used by tooling specifications and clients to interpret the OpenAPI document. This is not related to the API info.version string.
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
    
    method protected abstract JsonObject WriteContact(). 
   
    method protected JsonObject WriteComponents():
        define variable oJson as JsonObject no-undo.
        define variable oJsonSchema as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:add('schemas',WriteSchemas()).
        oJsonSchema = WriteSecuritySchemes().
        if valid-object(oJsonSchema) then
            oJson:add('securitySchemes':u, WriteSecuritySchemes()).
        else     
            oJson:addNull('securitySchemes').
        return oJson.
    end method.    
    
    method protected JsonObject WriteSchemas():
        define variable oBe as IBusinessEntity no-undo.
        // allow load from path building
        if valid-object(schemas) then 
            return schemas.
        else do:
            if not can-find(first ttEntity) then
            do: 
                FetchResources(output table ttEntity).
            end.
            schemas = new JsonObject().
            for each ttEntity where ttEntity.Typename matches "*.IBusinessEntity":
                oBE = ServiceManager:CreateBusinessEntity(ttEntity.Resource). 
                if oBe:Public then
                do:
                    AddSchemaObjects(schemas,oBE).
                end.    
            end.  
        end.        
        return new JsonObject(). 
    end method.    
    
    // final - override WriteAPiKeyAuth to control this
    // this is a bit convoluted and can likely  be done better,  
    // but WriteApiKeyAuth is abstact and implemented by thr application writer 
    // WriteComponents will add securitySchemes null if this return null 
    // (ABL json syntax to add null is odd)
    method protected final JsonObject WriteSecuritySchemes():
        define variable oJson as JsonObject no-undo.
        define variable oJsonAuth as JsonObject no-undo.
        oJsonAuth = WriteApiKeyAuth().
        if valid-object(oJsonAuth) then
        do: 
            oJson = new JsonObject().
            oJson:Add('ApiKeyAuth':u, WriteApiKeyAuth()).
        end.    
        return oJson. // return null if not defind
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
        define variable oRootTag as JsonObject no-undo.
        define variable cFixedMethodParents as character no-undo.
        if not can-find(first ttEntity) then
        do: 
            FetchResources(output table ttEntity).
        end.
        // sort the tags on the visual name
        // (WriteResourceTag will uppper case the first, but this not necessary
        // for the sort since it would be the same (and if it was not we would
        // prefere to have case insensitive sort even if the Swagger filter
        // is case sensitive) 
        
        //  we want to add a tag for the fixed parent also if it is not public 
        //  since we will add tag references for every case we encounter as links during 
        //  add path of the parents
        cFixedMethodParents = GetFixedMethodParents().
        for each ttEntity where 
              ttEntity.Public = true  
              or (cFixedMethodParents > "" and lookup(ttEntity.Resource,cFixedMethodParents) > 0)  
            by if ttEntity.Typename matches "*.IBusinessEntity" 
            then BufferUtil:SingularizeSerializeName(ttEntity.resource)
            else ttEntity.resource:  
           
            pTag:Add(WriteResourceTag(buffer ttEntity)). 
        end. 
      
          
    end method.     
    
    // key for tags and path to ensure the paths shows up under each entity added in tags 
    method protected character GetEndpointName(buffer ttEntity for ttEntity):
         
        if ttEntity.Typename matches "*.IBusinessEntity" then
            return BufferUtil:SingularizeSerializeName(ttEntity.resource,true).
        else
            return StringUtil:UpperCaseFirst(ttEntity.resource).    
    end.    
    
    // key for tags and path to ensure the paths shows up under each entity added in tags 
    method protected character GetEndpointName(pEntity as IEntity):
        if type-of(pEntity,IBusinessEntity) then
            return BufferUtil:SingularizeSerializeName(pEntity:resource,true).
        else   
            return StringUtil:UpperCaseFirst(pEntity:resource).
    end.    
    
    method protected character GetEndpointName(pcServiceName as character, plIsBE as logical):
         
        if plIsBE then
            return BufferUtil:SingularizeSerializeName(pcServiceName,true).
        else
            return StringUtil:UpperCaseFirst(pcServiceName).    
    end.    
     
    method protected JsonObject WriteResourceTag(buffer ttEntity for ttEntity):
        define variable oJson as JsonObject no-undo.
        define variable cEndPointName as character no-undo. 
        oJson = new JsonObject().
        cEndPointName = GetEndpointName(buffer ttEntity). 
        oJson:Add('name',cEndPointName).
        // Seems the reource plural name with uppercase of first makes sense here (?) 
        oJson:Add('description',subst("Everything about &1",cEndPointName)).
        return ojson.
    end method.    
    
    method protected void AddTagLink(pJson as JsonObject,pcObjectName as character):
        define variable cObjectNameasArray as character extent 1 no-undo.
        cObjectNameAsArray[1] = pcObjectName. 
        AddTagLinks(pJson,cObjectNameasArray).
    end method.    
    
    method protected void AddTagLinks(pJson as JsonObject,pcObjectNames as character extent):
        define variable oJsonTags as JsonArray no-undo.
        define variable iTag as integer no-undo.
        oJsonTags = new JsonArray().
        do iTag = 1 to extent(pcObjectNames):
            oJsonTags:Add(pcObjectNames[iTag]).
        end.
        pJson:Add("tags",oJsonTags).
    end method.    
     
    method protected void AddPaths(pPath as JsonObject):
        define variable oBe as IBusinessEntity no-undo.
        define variable oService as IServiceEntity no-undo.
        if not can-find(first ttEntity) then
        do: 
            FetchResources(output table ttEntity).
        end.
        CreateFixedMembers(). // create ttFixedmember records to use for linked paths to actions and functions
        CreateFixedLinkedMembers().
     //   CreateChildMethodMembers(pBe:Respource, input table ttFixedMember by-reference, input table ttMember by-reference, false).
        
       
        schemas = new JsonObject().
        // note that AddTags controls the sorting of the  top level 
        for each ttEntity where
            (FilterEntities = "" or lookup(ttEntity.Resource,FilterEntities) > 0)            
            and
            (ttEntity.Typename matches "*.IBusinessEntity"
            or 
            ttEntity.Typename matches "*.IServiceEntity") 
            and ttEntity.Public = true:    
            
            if ttEntity.Typename matches "*.IBusinessEntity" then
            do:
                oBE = ServiceManager:CreateBusinessEntity(ttEntity.Resource). 
                // collect now to be returned from writeSchemas
                // no perfromance benefit
                AddResourcePaths(pPath,oBE).
                AddSchemaObjects(schemas,oBE).
                
            end.    
            else do:
                oService = ServiceManager:CreateServiceEntity(ttEntity.Resource).

                AddResourcePaths(pPath,oService).
            end.
        end.    
    end method.     
    
    // allow subclass to override
    method protected character extent GetPathKeys(phbuffer as handle):
        return BufferUtil:GetUniqueIndexes(phBuffer, false).
    end method.     
   
    method protected character extent GetObjectKeyParameters(pBe as IBusinessEntity):
        define variable hbuffer  as handle no-undo.
        define variable cKeys    as character no-undo.
        define variable i        as integer no-undo.
        define variable hFlds    as handle extent no-undo.
        define variable cName    as character no-undo.
        define variable cParam   as character no-undo.
        define variable cIndexes as character extent no-undo.
        define variable iIndex  as integer no-undo.
        define variable lNoname as logical no-undo.
      
        define variable cBEKeys as character no-undo.   cIndexes = GetPathKeys(hbuffer).
        define variable cKeyParameters as character extent no-undo.
        //cKeys = BufferUtil:GetKeys(hBuffer).
        hBuffer = pBe:Datasets[1]:get-buffer-handle(1).
        cBekeys = pBe:GetKeys().
        // if the BE has no keys it does not handle request for any of the indexed fields
        // (in that case most likely from a different table in the dataset) 
        if cBeKeys > "" then
        do iIndex = 1 to extent(cIndexes):
            // aadd entry point for primary key
            cKeys = cIndexes[IIndex].
            //  no name needed in url for primary key and one field 
            lNoName = num-entries(cKeys) = 1 and cKeys = cBEKeys.   
             
            if ckeys <> "rowid" and cKeys <> "handle" and cKeys > "" then
            do:
                extent(hFlds) = num-entries(cKeys).
                cParam = "".
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
                extent(cKeyParameters) = (if extent(cKeyParameters) = ? then 0 else extent(cKeyParameters)) + 1.
                cKeyParameters[extent(cKeyParameters)] = cParam.
            end.
        end.    
    end method.   
    
    // defines entities that may have actions for all parent(key)/entity 
    method protected character GetFixedChildActionLinks():
        return "".
    end method.
    
    // defines entities that may have functions for all parent(key)/entity 
    method protected character GetFixedChildFunctionLinks():
        return "".
    end method.
    
    // defines entities that may have actions for all parent/entity 
    method protected character GetFixedContainerActionLinks():
        return "".
    end method.
    
    // defines entities that may have functions for all parent/entity 
    method protected character GetFixedContainerFunctionLinks():
        return "".
    end method.
    
    // defines a fixed parent relations to the Fixed*Links above 
    // typically for entities that are not public but only accessed from a parent
    // One to one idis simple since it does not need a key 
    // to do one to many... (this could in thery bbe nested...)
     
    method protected character GetFixedMethodParents():
        return "".
    end method.
    
    method protected void AddResourcePaths(pPath as JsonObject,pBe as IBusinessEntity):
        define variable hbuffer        as handle no-undo.
        define variable cKeys          as character no-undo.
        define variable i              as integer no-undo.
        define variable hFlds          as handle extent no-undo.
        define variable cParam         as character no-undo.
        define variable cIndexes       as character extent no-undo.
        define variable iIndex         as integer no-undo.
        define variable lNoname        as logical no-undo.
        define variable cBEKeys        as character no-undo.
        define variable cValue         as character no-undo.
        define variable iDataset       as integer no-undo.
        define variable hPrimeKeys     as handle extent no-undo.
        define variable cPrimeParams   as character no-undo.
        define variable oFixedChildBEs as IBusinessEntity extent no-undo.
        define variable oMethod        as IMethod no-undo.
        
        empty temp-table ttMember.
        
        // add general crud path for collection
        if lookup(pBe:Resource,MethodOnlyEntities) = 0 then 
            pPath:Add(subst("/&1",pBe:Resource),WriteResourceCollectionPath(pBe)). 
      
        hBuffer = pBe:Datasets[1]:get-buffer-handle(1).
         
        cIndexes = GetPathKeys(hbuffer).
       
        cBekeys = pBe:GetKeys().
        // loads lazy loaded actions and such if applicable 
        pBe:PrepareCatalog().
        define variable lerror as logical no-undo.
        if pBe:HasFunction then
            CreateFunctionMembers(pBe, input table ttMember by-reference).
        
        if pBe:HasAction then
            CreateActionMembers(pBe, input table ttMember by-reference).
        
        if pBe:HasLink then
            CreateLinkMembers(pBe, input table ttMember by-reference).
        
        CreateDatasetMembers(pBe, input table ttMember by-reference).
        
        CreateChildMethodMembers(pBe:Resource, input table ttLinkedMember by-reference, input table ttMember by-reference, false).
        
        
        // if the BE has no keys it does not handle request for any of the indexed fields
        // (in that case most likely from a different table in the dataset) 
        if cBeKeys > "" then
        indexloop:
        do iIndex = 1 to extent(cIndexes):
            // aadd entry point for primary key
            cKeys = cIndexes[IIndex].
            //  no name needed in url for primary key and one field 
            lNoName = num-entries(cKeys) = 1 and cKeys = cBEKeys.   
             
            if ckeys <> "rowid" and cKeys <> "handle" and cKeys > "" then
            do:
                extent(hFlds) = num-entries(cKeys).
                cParam = "".
               // add path for keys 
                do i = 1 to num-entries(cKeys):
                    hFlds[i] = hbuffer:buffer-field(entry(i,cKeys)).
                    if hFlds[i]:serialize-hidden then
                        next indexloop.
                    cValue = if hflds[i]:data-type = "character" or hFlds[i]:data-type begins "date" 
                        then quoter(subst("~{&1~}",hFlds[i]:serialize-name),"'")
                        else subst("~{&1~}",hFlds[i]:serialize-name) .
                    if lNoName then
                        cParam = cValue.  
                      
                    else  
                        cParam = StringUtil:AppendEntry(
                            cParam, 
                            subst("&1=&2",
                            hFlds[i]:serialize-name,
                            cValue
                            ) 
                            ).
                     
                    
                end.
                if lookup(pBe:Resource,MethodOnlyEntities) = 0 then 
                    pPath:Add(subst("/&1(&2)",pBe:Resource,cParam),WriteResourceObjectPath(pBe, hFLds)).
                // only add for prime key (for now?)
                if cKeys = cBEKeys   then
                do:
                    hPrimeKeys   = hFlds.
                    cPrimeParams = cParam.
                    for each ttMember: 
                        if valid-object(ttMember.memberObject) then 
                        do:
                            oMethod = cast(ttMember.memberObject,IMethod).
                            if MethodBindingEnum:Entity:Equals(oMethod:Binding) and extent(hPrimeKeys) <> ? then
                            do:      
                                if ttMember.isAction = false then
                                    AddFunctionObjectPath(pPath, pBe, cast(oMethod,IFunction), cPrimeParams, hPrimeKeys).
                                else
                                    AddActionObjectPath(pPath, pBe, cast(oMethod,IAction), cPrimeParams, hPrimeKeys).
                            end.
                        end.
                        else do:
                            // child links 
                            if ttMember.IsBound then
                            do:    
                                pPath:Add(
                                    subst("/&1(&2)/&3",pBe:Resource,cPrimeParams,ttMember.key),
                                    WriteResourceObjectChildPath(
                                    pBe, 
                                    ttMember.key,
                                    cast(ttMember.cardinality,RelationTypeEnum),
                                    hPrimeKeys)
                                    ).
                                    
                                for each ttFixedLinkedMember where ttFixedLinkedMember.parentKey = ttMember.sourceEntity
                                                           and ttFixedLinkedMember.isbound = true
                             //        and ttFixedLinkedMember.isAction = true
                                    :
                                        find ttFixedmember where ttfixedmember.key = ttFixedLinkedMember.childkey no-error.
                                        
/*                                message subst("/&1(&2)/&3",pBe:Resource,cPrimeParams,ttMember.key) skip                           */
/*                                        ttMember.sourceEntity skip                                                                */
/*                                        subst("/&1(&2)/&3/&4",pBe:Resource,cPrimeParams,ttMember.key,ttFixedLinkedMember.childkey)*/
/*                                        ttFixedLinkedMember.memberObject:GetCLass():TypeName  skip                                */
/*                                        ttFixedMember.memberObject:GetCLass():TypeName  skip                                      */
/*                                         view-as alert-box     .                                                                  */
                                end.
                                
                              //  if avail ttFixedLinkedMember then
                                // message subst("/&1(&2)/&3/&4",pBe:Resource,cPrimeParams,ttMember.key,ttMember.key)
                                // view-as alert-box.
                                /* 
                                    pPath:Add(
                                       subst("/&1(&2)/&3/&4",pBe:Resource,cPrimeParams,ttMember.key,ttMember.key),
                                       WriteResourceObjectChildPath(
                                       pBe, 
                                       ttMember.key,
                                       cast(ttMember.cardinality,RelationTypeEnum),
                                       hPrimeKeys)
                                    ). */
                            end.
                            else if ttMember.IsLinkedMethod then
                            do: 
                                 AddChildResourceMethodPaths(pPath,pBe,cPrimeParams, hPrimeKeys).
                            /*                                                                                                                   */
                            /*                                define variable iDummy as integer no-undo.                                         */
                            /*                                iDummy = iDummy + 1.                                                               */
                            /*                                pPath:Add(                                                                         */
                            /*                                    subst("/&1(&2)/&3/XX",pBe:Resource,cPrimeParams,ttMember.key) + string(iDummy),*/
                            /*                                    WriteResourceObjectChildPath(                                                  */
                            /*                                         pBe,                                                                      */
                            /*                                         ttMember.key,                                                             */
                            /*                                         cast(ttMember.cardinality,RelationTypeEnum),                              */
                            /*                                         hPrimeKeys)                                                               */
                            /*                                ).*/
                                                                                                              
                            end.    
                        end.            
                    end.           
                end.    
            end.
        end.
        
        for each ttMember: 
            
            if valid-object(ttMember.memberObject) then 
            do:
                oMethod = cast(ttMember.memberObject,IMethod).
                if (MethodBindingEnum:Any:Equals(oMethod:Binding) 
                    or 
                    MethodBindingEnum:Collection:Equals(oMethod:Binding)) then 
                do:
                    if ttMember.isAction = false then
                        AddFunctionCollectionPath(pPath, pBe, cast(oMethod,Ifunction)).
                    else
                        AddActionCollectionPath(pPath, pBe, cast(oMethod,IAction)).
                end.
            end.
            else do:
                if ttMember.IsLinkedMethod then
                do: 
                    pPath:Add(subst("/&1/&2/XX",pBe:Resource,ttMember.key),
                        WriteSubResourcePath(pBe,ttMember.key)).
                end.    
                else if ttMember.IsBound = false then
                do:
                    /*                                                                                                         */
                    /*                if ttMember.IsBound then                                                                 */
                    /*                do:                                                                                      */
                    /*                    if pPath:Has(subst("/&1(&2)/&3",pBe:Resource,cPrimeParams,ttMember.key)) = false then*/
                    /*                    pPath:Add(subst("/&1(&2)/&3",pBe:Resource,cPrimeParams,ttMember.key),                */
                    /*                          WriteSubResourcePath(pBe,ttMember.key)).                                       */
                    /*                                                                                                         */
                    /*                end.                                                                                     */
                    /*                else                                                                                     */
                    pPath:Add(subst("/&1/&2",pBe:Resource,ttMember.key),
                        WriteSubResourcePath(pBe,ttMember.key)).
                end.            
            end.
        end.      
      
    end method.    
    
    method protected void AddChildResourceMethodPaths(pPath as JsonObject,pBe as IBusinessEntity,pcParam as character, phKeys as handle extent):
        define variable hDummy as handle extent no-undo.
        define variable cPath as character no-undo.
        define variable oJson as JsonObject no-undo. 
        for each ttFixedMember: 
            for each ttLinkedMember where ttLinkedMember.parentKey = pBe:Resource
                and ttLinkedMember.childKey  = ttFixedMember.key:
                if ttLinkedMember.isAction  then 
                do:
                    cPath = subst("/&1(&2)/&3/&4",pBe:Resource,PcParam,ttFixedMember.key,cast(ttLinkedMember.memberObject,IMethod):name).
                    oJson = new JsonObject().
                    oJson:Add("post",WritePostResourceTwoTags(pBE,cast(ttFixedMember.memberObject,IBusinessentity), false, pcParam, phKeys)).           
                    pPath:Add(cPath,
                    oJson
                    ).
                end.     
            /**                               
                AddActionPath(pPath,
                             subst("/&1(&2)/&3)",pBe:Resource,PcParam,ttFixedMember.key),
                             cast(ttFixedMember.memberObject,IBusinessEntity), 
                             cast(ttLinkedMember.memberObject,IAction),
                             false,
                             "",
                             hDummy 
                             ).
             **/                
            
            end.   
        end.

    end method.  
    
    method protected void AddChildResourceFunctionPaths(pPath as JsonObject,pBe as IBusinessEntity):

    end method.  
    
    
        
    method protected void AddResourcePaths(pPath as JsonObject,pService as IServiceEntity):
        define variable iProp   as integer no-undo.
        define variable iMethod as integer no-undo.
        define variable iParam as integer no-undo.
        define variable oMethods as Method extent no-undo.
        define variable oParameters  as Parameter  extent no-undo.
        define variable lSupportOneSegment as logical no-undo.
        define variable cValue as character no-undo.
        define variable cParam as character no-undo.
        define variable cPublicParamName as character no-undo.
        define variable cMethodName as character no-undo.
         
        oMethods = pService:GetMethods().
        // nothing to execute then this is a one segment service only 
        if extent(oMethods) = ? then
            lSupportOneSegment = true.
         // if execute then the method is hidden so only expose the name 
        else if (extent(oMethods) = 1 and oMethods[1]:name = "Execute") then
        do:
            // if not allow action and update then we use a json body 
          //  if pService:AllowAction = false and pService:ReadOnly = false then
         //       lSupportOneSegment = true.
          //  else do:   
            oParameters = oMethods[1]:GetParameters().
            if extent(oParameters) = ? then
                lSupportOneSegment = true.
          //  end.
        end.
        // a multi method may have public properties for get  
        else if extent(oMethods) > 0 and extent(pService:Properties) > 0 
            and pService:ReadOnly then
            lSupportOneSegment = true.
            
        if lSupportOneSegment then     
            pPath:Add(subst("/&1",pService:Resource),WriteResourceServicePath(pService)). 
        
        /*        if pService:CanRead then*/
        /*        do:                     */
        /*                                */
            
        // if no methods then public properties are the only thing that can be exposed/reached
        if extent(oMethods) = ? 
        // if tere are methods, but no execute any public properties are assumed to   
        // be  accessible without callng any method (otherwise there is no point of having them public)
        // but check for readonly if many methods since props may be used for content type text
        //   
        //  specificall assumed to be useful for application level Session that will expose 
        // userid as property and also may have access level method (Get or check Role or group and such)  
            or (extent(oMethods) > 1 and pService:ReadOnly) or (extent(oMethods) = 1 and oMethods[1]:name <> "Execute") then
       
        // (so as of current a single execute methods cannot expose only 
        // one of the properties - mainly because the service would not call execute in that case (?)) 
        // if one need this one may name the method something else than execute (not tested ?)
        do iProp = 1 to extent(pService:Properties):
            pPath:Add(subst("/&1/&2",
                pService:Resource,
                pService:Properties[iProp]:name
                ),
                WriteResourceServicePath(pService,pService:Properties[iProp]:name)
                ). 
        end.
        do iMethod = 1 to extent(oMethods):
            extent(oParameters) = ?.
            oParameters = oMethods[iMethod]:GetParameters().
            cParam = "".
            
            do iParam = 1 to extent(oParameters): 
                cPublicParamName = pService:GetPublicParameterName(oParameters[iParam]:name).
                cValue = if oParameters[iParam]:extent = 0 
                    then if Progress.Reflect.DataType:Character:Equals(oParameters[iParam]:datatype)
                      //   or oParameters[iParam]:datatype begins "date" 
                    then quoter(subst("~{&1~}",cPublicParamName),"'")
                    else subst("~{&1~}",cPublicParamName) 
                    else subst("@&1",cPublicParamName).
                cParam = StringUtil:AppendEntry(
                    cParam,
                    subst("&1=&2",
                    cPublicParamName,
                    cValue) 
                    ). 
            end.  
            cMethodName = if oMethods[iMethod]:name <> "Execute" 
                then oMethods[iMethod]:name  
                else "". 
            
            // if no method or param then we have added the path on top
            if cMethodName > "" or cParam > "" then
            do:    
                pPath:Add(
                    subst("/&1&2&3",
                    pService:Resource,
                    if cMethodName > "" then subst("/&1",cMethodName) else "",
                    if cParam > "" then subst("(&1)",cParam) else ""
                    ),
                    WriteResourceServicePath(pService,oMethods[iMethod]:name,oParameters)). 
                 
            end.
        end.        
  
    end method.       
    
    method protected void AddFunctionCollectionPath(pPath as JsonObject,pBe as IBusinessEntity, poFunction as IFunction):
        define variable hDummy as handle extent no-undo.
        AddFunctionPath(pPAth,pBe,poFunction,true,"",hDummy).
    end.
    
    method protected void AddFunctionObjectPath(pPath as JsonObject,pBe as IBusinessEntity,poFunction as IFunction,pcParam as character, phKeys as handle extent):
        AddFunctionPath(pPAth,pBe,poFunction,false,pcParam,phKeys).
    end.
    
    method protected void AddFunctionPath(pPath as JsonObject,pBe as IBusinessEntity, poFunction as IFunction,plCollection as logical, pcParam as character, phKeys as handle extent):
        define variable iFunction as integer no-undo.
        define variable iSignature as integer no-undo.
        define variable oSignature as ISignature no-undo.
      //  define variable oIterator as IIterator no-undo.
    //    define variable oFunction as IFunction  no-undo.
      //  define variable oMapEntry as IMapEntry no-undo.
        define variable cFunction as character no-undo.
        define variable cFunctionParam as character no-undo.
        define variable iSignatureParam as integer no-undo.
        define variable oFunctionMap  as IMap  no-undo.
        define variable cValue as character no-undo.
        
      //  oFunctionMap = pBe:Functions.
      //  oIterator = oFunctionMap:EntrySet:Iterator().
        
      //  do while oIterator:HasNext():
        //    oMapEntry = cast(oIterator:Next(),IMapEntry). 
        cFunction = poFunction:name.
          //  oFunction = cast(oMapEntry:Value,IFunction). 
     //      cast(poFunction,Pmfo.Core.Common.Method):CreateParameters().
        poFunction:InitializeSignatures().  
        if (plCollection
            and (MethodBindingEnum:Any:Equals(poFunction:Binding) 
            or 
            MethodBindingEnum:Collection:Equals(poFunction:Binding))
            )        
            or (plCollection = false 
            and MethodBindingEnum:Entity:Equals(poFunction:Binding)) then
        do: 
            do iSignature = 1 to if extent(poFunction:Signatures) = ? then 1 else extent(poFunction:Signatures):
                cFunctionParam = "".
                     
                if extent(poFunction:Signatures) <> ? then
                do iSignatureParam = 1 to extent(poFunction:Signatures[iSignature]:Parameters):
                    if IsArray(poFunction:Signatures[iSignature]:Parameters[iSignatureParam]:DataType) then
                    do:
                            // add value in path as array 
                        cValue = subst("@&1",poFunction:Signatures[iSignature]:Parameters[iSignatureParam]:name).
                    end.
                    else do:   
                        cValue = subst("~{&1~}",poFunction:Signatures[iSignature]:Parameters[iSignatureParam]:name).
                            // add quotes if necessary
                        case poFunction:Signatures[iSignature]:Parameters[iSignatureParam]:DataType:
                            when DataTypeEnum:Character 
                            or 
                            when DataTypeEnum:DateTime  
                            or 
                            when DataTypeEnum:DateTime-tz  
                            or
                            when DataTypeEnum:Date then  
                                do:
                                    cValue = quoter(cValue,"'").
                                end.    
                        end.
                    end.
                       // case type-of(oFunction:Signatures[iSignature]:Parameters[iSignatureParam])):
                       // end.    
                    cFunctionParam = StringUtil:AppendEntry(
                        cFunctionParam,
                        subst("&1=&2",
                        poFunction:Signatures[iSignature]:Parameters[iSignatureParam]:name,
                        cValue
                        )  
                        ).
                              
                end. 
                if cFunctionParam > "" then 
                    cFunctionParam = subst("(&1)",cFunctionParam).   
                   
                   
                if plCollection then
                    pPath:Add(subst("/&1/&2&3",
                        ttEntity.Resource,
                        cFunction,
                        cFunctionParam
                        ),
                        WriteResourceCollectionFunctionPath(
                        pBe,
                        poFunction,
                        if extent(poFunction:Signatures) = ? 
                        then 0 
                        else iSignature)).  
                else do:
                        
                    pPath:Add(subst("/&1(&2)/&3&4",
                        ttEntity.Resource,
                        pcParam,
                        cFunction,
                        cFunctionParam
                        ),
                        WriteResourceObjectFunctionPath(
                        pBe,
                        poFunction,
                        if extent(poFunction:Signatures) = ? 
                        then 0 
                        else iSignature,
                        phKeys)).  
                end.                            
            end.            
        end.    
       
    end method.    
    
    method protected void AddActionCollectionPath(pPath as JsonObject,pBe as IBusinessEntity,poAction as IAction):
        define variable hDummy as handle extent no-undo.
        AddActionPath(pPath,pBe:Resource,pBe,poAction,true,"",hDummy).
    end.
    
    method protected void AddActionObjectPath(pPath as JsonObject,pBe as IBusinessEntity,poAction as IAction,pcParam as character, phKeys as handle extent):
        define variable hDummy as handle extent no-undo.
        AddActionPath(pPath,pBe:Resource,pBe,poAction,false,pcParam,phKeys).
    end.
    
    method protected void AddActionPath(pPath as JsonObject,pcResourcePath as character,pBe as IBusinessEntity,poAction as IAction, plCollection as logical, pcParam as character, phKeys as handle extent):
        define variable iAction as integer no-undo.
        define variable iSignature as integer no-undo.
        define variable oSignature as ISignature no-undo.
        define variable oIterator as IIterator no-undo.
        define variable cAction  as character no-undo.
        define variable cActionParam as character no-undo.
        define variable iSignatureParam as integer no-undo.
        define variable iTrueSignature as integer no-undo.
        define variable oActionMap  as IMap  no-undo.
        define variable iNumEndPoints as integer no-undo.
        define variable cValue as character no-undo.
        define variable cPath as character no-undo.
        define variable oPathJson as JsonObject no-undo.
        
        cAction = poAction:name. 
        poAction:InitializeSignatures().
         
        iNumEndPoints = if extent(poAction:Signatures) <> ? then extent(poAction:Signatures) else 1.
        
        do iSignature = 1 to iNumEndPoints:
            cActionParam = "".
            
            if extent(poAction:Signatures) <> ? then
            do: 
                iTrueSignature = iSignature.    
                do iSignatureParam = 1 to extent(poAction:Signatures[iSignature]:Parameters):
                    if IsArray(poAction:Signatures[iSignature]:Parameters[iSignatureParam]:DataType) then
                    do:
                        // add value in path as array 
                        cValue = subst("@&1",poAction:Signatures[iSignature]:Parameters[iSignatureParam]:name).
                    end.
                    else do:   
                        cValue = subst("~{&1~}",poAction:Signatures[iSignature]:Parameters[iSignatureParam]:name).
                        case poAction:Signatures[iSignature]:Parameters[iSignatureParam]:DataType:
                            when DataTypeEnum:Character 
                            or 
                            when DataTypeEnum:DateTime  
                            or 
                            when DataTypeEnum:DateTime-tz  
                            or
                            when DataTypeEnum:Date then  
                                do:
                                    cValue = quoter(cValue,"'").
                                end.    
                              
                        end.
                    end.
                   // case type-of(oFunction:Signatures[iSignature]:Parameters[iSignatureParam])):
                   // end.    
                    cActionParam = StringUtil:AppendEntry(
                        cActionParam,
                        subst("&1=&2",
                        poAction:Signatures[iSignature]:Parameters[iSignatureParam]:name,
                        cValue
                        )  
                        ).
                
                end.
                
            end.
            else 
                iTrueSignature = 0.
                 
            if cActionParam > "" then 
                cActionParam = subst("(&1)",cActionParam).   
            
           
            if plCollection then
            do:
                cPath = subst("/&1/&2&3",
                    pcResourcePath,
                    cAction,
                    cActionParam
                    ). 
                if pPath:Has(cPath) then 
                do:
                    oPathJson = pPath:GetJsonObject(cPath).
                    AddResourceCollectionActionPath(oPathJson,pBe,poAction,iTrueSignature).
                end.
                else
                    pPath:Add(cPath, 
                        WriteResourceCollectionActionPath(pBe,poAction,iTrueSignature)). 
                    
            end.
            else do: 
                cPath = subst("/&1(&2)/&3&4",
                    pcResourcePath,
                    pcParam,
                    cAction,
                    cActionParam
                    ).
                if pPath:Has(cPath) then 
                do:
                    oPathJson = pPath:GetJsonObject(cPath).
                    AddResourceObjectActionPath(oPathJson,pBe,poAction,iTrueSignature,phKeys).
              
                end.
                else    
                    pPath:Add(cPath, 
                        WriteResourceObjectActionPath(pBe,poAction,iTrueSignature,phKeys)
                        ).     
                        
                    
            end.
        end.   
    end method.    
    
    method protected void CreateActionMembers (pBe as IBusinessEntity, input table ttMember):
        CreateMethodMembers(pbe,pBe:Actions,true, input table ttMember by-reference).
    end method.
    
    method protected void CreateFunctionMembers (pBe as IBusinessEntity, input table ttMember):
        CreateMethodMembers(pBe,pBe:Functions,false, input table ttMember by-reference).
    end method.
    
    method protected void CreateMethodMembers (pBe as IBusinessEntity, pMethodMap as IMap, plIsAction as logical, input table ttMember):
        define variable oMethod as IMap  no-undo.
        define variable oIterator as IIterator no-undo.
        define variable oMapEntry as IMapEntry no-undo.
        oIterator = pMethodMap:EntrySet:Iterator().
         
        do while oIterator:HasNext() on error undo, throw:
            oMapEntry = cast(oIterator:Next(),IMapEntry).
            create ttMember.
            assign 
                ttMember.key          = cast(oMapEntry:Key,String):Value
                ttMember.isAction     = plIsAction
                ttMember.memberObject = cast(oMapEntry:Value,IMethod)
                .
        end.     
    
    end method.
    
    method protected void CreateDatasetMembers (pBe as IBusinessEntity, input table ttMember):
        define variable iDataset     as integer no-undo.
        define variable oMap         as IMap no-undo.
        define variable oKeyIterator as IIterator no-undo.
        
        if extent(pBe:Datasets) <> ? then
        do iDataset = 2 to extent(pBe:Datasets):
            if pBe:Resource <> pBe:Datasets[iDataset]:serialize-name then
            do:
                create ttMember.
                assign 
                    ttMember.key = pBe:Datasets[iDataset]:serialize-name
                    isBound = false
                    .
                       
            end.
        end.
        
        if pBe:HasRedirectMap then
        do:
            oMap = pBe:RedirectMap.
            oKeyIterator = oMap:KeySet:Iterator().
            do while oKeyIterator:HasNext() on error undo, throw:
                create ttMember.
                assign 
                    ttMember.key = cast(oKeyIterator:Next(),String):Value.
            
                 
            end.
            
        end.     
    end method.
    
    method protected void CreateLinkMembers (pBe as IBusinessEntity, input table ttMember):
        
        define variable oIterator as IIterator no-undo.
        define variable oRelation as IRelation no-undo.
        define variable cError    as character no-undo.      
        if pBe:HasLink then
        do:
            oIterator = pBe:Links:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                // some relations may be defin3ed for a child table in the dataset
                if oRelation:parentName = pBe:resource then
                do on error undo, throw:
                    create ttMember.
                    assign 
                        ttMember.key          = oRelation:PublicName
                        ttMember.sourceEntity = oRelation:name
                        ttMember.cardinality  = oRelation:Cardinality 
                        ttMember.IsBound = true.
                    catch e as Progress.Lang.Error :
                        if e:GetMessageNum(1)  = 132 then
                            cError = subst("OpenAPI Error: Entity &1 has more than one link to &2. This is currently allowed if they have different Cardinality, but only one of these links are correct and that may not be then one shown in the UI. The &3 should be corrected.",
                                pBe:Resource,
                                ttMember.key,
                                pbe:GetClass():TypeName
                                ).
                        else 
                            cerror = subst("OpenAPI Error: Failed to create link from Entity &1 to &2 due to en error.~n&3",
                                pBe:Resource,
                                ttMember.key,
                                e:GetMessage(1)
                                ).
      
                        if session:remote then 
                            message cError.
                        else                           
                            // it is considered to expensive to check this at Be:Initialize  
                            message cError    
                                view-as alert-box error.
                        if avail ttMember then
                            delete ttMember.
                    end catch.    
                end.    
            end.
        end.
    end method.
    
    method protected void CreateFixedLinkedMembers ():
        define variable cFixedMethodParents as character no-undo.
        define variable hDummy as handle no-undo.
        cFixedMethodParents = GetFixedMethodParents().
        define variable i as integer no-undo.
        do i = 1 to num-entries(cFixedMethodParents):
            CreateChildMethodMembers(entry(i,cFixedMethodParents), input table ttFixedLinkedMember by-reference, input table-handle hDummy by-reference, true).    
        end.    
        
    end method.
    
    method protected void CreateFixedMembers ():
        CreateFixedMembers(GetFixedChildActionLinks(),"childAction").
        CreateFixedMembers(GetFixedChildFunctionLinks(),"childFunction").
        CreateFixedMembers(GetFixedContainerActionLinks(),"containerAction").
        CreateFixedMembers(GetFixedContainerFunctionLinks(),"containerFunction").
    end method.     
    
    method protected void CreateFixedMembers (pcLinks as character, pType as character):
        define variable i as integer no-undo.
        do i = 1 to num-entries(pcLinks):
            find ttFixedMember where ttFixedMember.key = entry(i,pcLinks) no-error.
            if not avail ttFixedMember then
            do:
                create ttFixedMember.
                assign 
                    ttFixedMember.memberObject = this-object:serviceManager:CreateBusinessEntity(entry(i,pcLinks))
                    ttFixedMember.key = entry(i,pcLinks)
                    . 
            end.
            // todo use enum? 
            case pType: 
                when "childAction" then
                    ttFixedMember.hasChildAction = true.
                when "childFunction" then
                    ttFixedMember.hasChildFunction = true.
                when "containerAction" then
                    ttFixedMember.hasContainerAction = true.
                when "containerFunction" then
                    ttFixedMember.hasChildFunction = true.
            end.
          
        end.    
        
    end method.     
    
    method protected void CreateChildMethodMembers (pParentResource as character, input table ttLinkedMember, input table ttMember, plNoMember as logical ):
        define variable cChildActionLinks as  character no-undo.
        define variable cChildFunctionLinks as  character no-undo.
        define variable cContainerActionLinks as  character no-undo.
        define variable cContainerFunctionLinks as  character no-undo.
        
        define variable oFunctions  as IFunction extent no-undo.
        define variable oActions    as IAction extent no-undo.
        define variable oFixedChild as IDynamicChild no-undo.
        define variable iChild as integer no-undo.
        define variable iMethod as integer no-undo.
        define variable iTime as integer no-undo.
        
        empty temp-table ttLinkedMember.
        for each ttFixedMember:
            if type-of(ttFixedMember.memberObject,IDynamicChild) then 
            do:
                oFixedChild = cast(ttFixedMember.memberObject,IDynamicChild).
                if plNoMember = false then
                do:  
                    // create in ttmember for correct sort of paths
                    create ttMember.
                    assign 
                        ttmember.key            = ttFixedMember.Key
                        ttMember.IsLinkedMethod = true
                        .
                        
                end. 
                   
                if ttFixedMember.hasChildAction or ttFixedMember.hasContainerAction then
                do: 
                    oActions   = oFixedChild:GetActionsForEntity(pParentResource). 
                    do iMethod = 1 to extent(oActions):
                        create ttLinkedMember.
                        assign 
                            ttLinkedMember.parentKey    = pParentResource
                            ttLinkedMember.childKey     = ttFixedMember.key
                            ttLinkedMember.Key          = oActions[iMethod]:name
                            ttLinkedMember.memberObject = oActions[iMethod]
                            ttLinkedMember.isAction     = true
                            ttLinkedMember.isBound      = ttFixedMember.hasChildAction
                            . 
                    end.
                end.
                if ttFixedMember.hasChildFunction or ttFixedMember.hasContainerFunction then
                do: 
                    oFunctions = oFixedChild:GetFunctionsForEntity(pParentResource).
                    do iMethod = 1 to extent(oFunctions):
                        create ttLinkedMember.
                        assign 
                            ttLinkedMember.parentKey    = pParentResource
                            ttLinkedMember.childKey     = ttFixedMember.key
                            ttLinkedMember.Key          = oFunctions[iMethod]:name
                            ttLinkedMember.memberObject = oFunctions[iMethod]
                            ttLinkedMember.isAction     = false
                            ttLinkedMember.isBound      = ttFixedMember.hasChildFunction
                            . 
                    end.
                end.
            end.
        end.
        
    end method.
    
    method protected void AddSchemaObjects(pSchemas as JsonObject,pBe as IBusinessEntity):
        define variable hBuffer as handle no-undo.
        define variable oObject as JsonObject  no-undo.
        define variable oProps as JsonObject  no-undo.
        
        define variable oObjectArray as JsonObject  no-undo.
           
        if DatasetUtil:GetNumTopBuffers(pbe:Datasets[1]) = 1 then
        do:
            hBuffer = pBe:Datasets[1]:get-buffer-handle(1).
            oProps = Writeproperties(hBuffer).
             
            if valid-object(oProps) then
            do:
                oObject = new JsonObject().
                oObject:Add("type","object").
                oObject:add("properties",oProps). 
                
                pSchemas:Add(
                    GetSchemaName(OpenAPISchemaNameEnum:Properties,hBuffer),
                    oObject).
                
                oObjectArray = new JsonObject().
                oObjectArray:Add("type","array").
                oObjectArray:add("items",oObject). 
                
                pSchemas:Add(
                    GetSchemaName(OpenAPISchemaNameEnum:PropertiesInArray,hBuffer),
                    oObjectArray).
                
                // post and delete can use this as requestBody for single object
                pSchemas:Add(GetSchemaName(OpenAPISchemaNameEnum:ObjectBody,hBuffer),
                    WriteWrapper(pBe,hBuffer:serialize-name,OpenAPISchemaNameEnum:Properties)).
                
                // post and delete will use this as requestBody for collection/array 
                pSchemas:Add(GetSchemaName(OpenAPISchemaNameEnum:ArrayBody,hBuffer),
                    WriteWrapper(pBe,hBuffer:serialize-name,OpenAPISchemaNameEnum:PropertiesInArray)).
                
               // pSchemas:Add(hBuffer:serialize-name + "_prods",WriteModifiedProperties(hBuffer:serialize-name,oProps)).
                pSchemas:Add(GetSchemaName(OpenAPISchemaNameEnum:ProdsProperties,hBuffer),
                    WriteModifiedProperties(hBuffer:serialize-name,oProps)).
               
                pSchemas:Add(GetSchemaName(OpenAPISchemaNameEnum:ArrayModifiedBody,hBuffer),
                    WriteModifiedWrapper(pBe,hBuffer:serialize-name)).
                
            end.
        end.  
    end method.
    
    method protected character GetSchemaName(pSchemaNameEnum as OpenAPISchemaNameEnum, phBuffer as handle):
        return GetSchemaName(pSchemaNameEnum,phBuffer:serialize-name).
    end.    
    
    method protected character AddSchemaReference(pJson as JsonObject,pSchemaNameEnum as OpenAPISchemaNameEnum, pcSerializeName as character):
        pJson:Add("$ref",
            subst("&1/&2",
            SCHEMAS_REFERENCE, 
            GetSchemaName(
            pSchemaNameEnum, 
            pcSerializename)
            )     
            ).
        
    end.      
    
    method protected character GetSchemaName(pSchemaNameEnum as OpenAPISchemaNameEnum, pcSerializeName as character):
        case pSchemaNameEnum:
            when OpenAPISchemaNameEnum:ArrayBody then
                return subst("&1_&2",pcSerializeName,"collection").  
            when OpenAPISchemaNameEnum:ArrayModifiedBody then
                return subst("&1_&2",pcSerializeName,"mod").  
            when OpenAPISchemaNameEnum:ProdsProperties then
                return subst("&1_&2",pcSerializeName,"prods").  
            when OpenAPISchemaNameEnum:ObjectBody then
                return subst("&1_&2",pcSerializeName,"body").  
            when OpenAPISchemaNameEnum:Properties then
                return pcSerializeName.  
            when OpenAPISchemaNameEnum:PropertiesInArray then
                return subst("&1_&2",pcSerializeName,"array").  
            otherwise 
            undo, throw new IllegalArgumentError("Invalid OpenAPISchemaNameEnum").
        end.
        
    end.    
    
    // allow subclasses to add meta property to body
    method protected void AddRequestBodyMetaProperties(pRoot as JsonObject,pBe as IBusinessEntity):
    end method.
    
    method protected JsonObject WriteWrapper(pBe as IBusinessEntity,pcSerializename as character,pReferenceEnum as OpenAPISchemaNameEnum):
        define variable oBodyObject as JsonObject  no-undo.
        define variable oBodyObjectProps as JsonObject  no-undo.
        define variable oBodyObjectPropRef as JsonObject  no-undo.
        oBodyObject = new JsonObject(). 
        oBodyObject:Add("type","object").
        oBodyObjectProps = new JsonObject().
        oBodyObject:add("properties",oBodyObjectProps). 
        oBodyObjectPropRef = new JsonObject().
        oBodyObjectProps:add(pcSerializename,oBodyObjectPropRef) .
        AddSchemaReference(oBodyObjectPropRef, pReferenceEnum, pcSerializeName).
        
        AddRequestBodyMetaProperties(oBodyObjectProps,pBe).
        return oBodyObject.   
    end method. 
    
    method protected JsonObject WriteModifiedWrapper(pBe as IBusinessentity, pcSerializename as character):
        define variable oBodyObject as JsonObject  no-undo.
        define variable oBodyObjectProps as JsonObject  no-undo.
        define variable oBodyObjectPropRef as JsonObject  no-undo.
        define variable oHasChanges as JsonObject  no-undo.
        define variable oBefore as JsonObject no-undo.
        define variable oBodyObjectBeforeProps as JsonObject  no-undo.
        define variable oBodyObjectBeforePropRef as JsonObject  no-undo.
        define variable ostep as JsonObject  no-undo.
        
        oBodyObject = new JsonObject(). 
        oBodyObject:Add("type","object").
        
        oBodyObjectProps = new JsonObject().
        oBodyObject:add("properties",oBodyObjectProps). 
        oHasChanges = new JsonObject().
        oHasChanges:Add("type","boolean").
        oBodyObjectProps:add("prods:hasChanges",oHasChanges). 
        
        oBodyObjectPropRef = new JsonObject().
        oBodyObjectProps:add(pcSerializename,oBodyObjectPropRef) .
        
        AddSchemaReference(oBodyObjectPropRef, 
            OpenAPISchemaNameEnum:ProdsProperties, 
            pcSerializeName).
        
        
        oBefore = new JsonObject().
        oBodyObjectBeforeProps = new JsonObject().
        oBodyObjectBeforePropRef = new JsonObject().
        oStep = new JsonObject().
        oBodyObjectProps:add("prods:before",oBefore). 
        oBefore:Add("properties",oBodyObjectBeforeProps).
        //oStep:Add("cc",oBodyObjectBeforeProps).
        oBodyObjectBeforeProps:add(pcSerializename,oBodyObjectBeforePropRef) .
        
        AddSchemaReference(oBodyObjectBeforePropRef, 
            OpenAPISchemaNameEnum:ProdsProperties, 
            pcSerializeName).
        
        AddRequestBodyMetaProperties(oBodyObjectProps,pBe).
        
        return oBodyObject.   
    end method.  
    
    method protected JsonObject WriteModifiedProperties(pcSerializename as character, pProps as JsonObject):
        define variable oBodyObject as JsonObject  no-undo.
        define variable oBodyObjectProps as JsonObject  no-undo.
        define variable oBodyObjectPropRef as JsonObject  no-undo.
        define variable oProp as JsonObject  no-undo.
        define variable oProps as JsonObject  no-undo.
       
       //*********array begin
        define variable oObjectArray as JsonObject no-undo.
        
        oObjectArray = new JsonObject().
        oObjectArray:Add("type","array").
        /// array end      
            
        oBodyObject = new JsonObject().
        // array beginn
        oObjectArray:add("items",oBodyObject).  
        // array end 
        oBodyObject:Add("type","object").
        oBodyObjectProps = new JsonObject().
        oProps = cast(pProps:Clone(),JsonObject).
        oBodyObject:add("properties",oProps). 
        
        oProp = new JsonObject().
        oProps:add("prods:rowState",oProp). 
        oProp:Add("type","string").
    
        oProp:Add("example","modified").
        
        oProp = new JsonObject().
        oProps:add("prods:id",oProp). 
        oProp:Add("type","string").
    
        oProp:Add("example",pcSerializename + "1").
   
        return oObjectArray.
    end method.   
    
    method protected JsonObject WriteProperties(phBuffer as handle):
        define variable oProps as JsonObject  no-undo.
        define variable oProp as JsonObject  no-undo.
        define variable i as integer no-undo.
        define variable hField as handle no-undo.
        oProps = new JsonObject().
       
        do i  = 1 to phbuffer:num-fields:
            hField = phbuffer:buffer-field(i). 
            if valid-handle(hField) 
                and hField:serialize-hidden = false 
                and index(hField:serialize-name,'@') = 0 then
            do:  
                 // some schames may still have more than one TBD... 
                if not oprops:Has(hField:serialize-name) then
                do:
                    oProp = new JsonObject().
                    oProps:add(hField:serialize-name,oProp). 
               
                    case hField:data-type:
                        when "integer" then
                            do:
                                oProp:Add("type",lc(hField:data-type)).
                            end.
                        when "int64" then
                            do:
                                oProp:Add("type","integer").
                            end.
                        when "decimal" then
                            do:
                                oProp:Add("type","number").
                            end.
                        when "logical" then
                            do:
                                oProp:Add("type","boolean").
                                oProp:Add("example",false).
                            end.
                        when "date" then
                            do:
                                oProp:Add("type","string").
                                oProp:Add("format","date").
                            end.
                        when "datetime" or when "datetime-tz" then
                            do:
                                oProp:Add("type","string").
                                oProp:Add("format","date-time").
                            
                            end.
                        otherwise
                        do:
                            oProp:Add("type","string").
                            oProp:Add("example","").
                        end.    
                    end case.

                end.   
            end.
        end.    
        return oProps.  
    end method.
     
    method protected JsonObject WriteResourceCollectionPath(pBe as IBusinessEntity):
        define variable oJson as JsonObject no-undo.
        define variable hdummy as handle extent no-undo.
        
        oJson = new JsonObject().
        oJson:Add('get',WriteGetResource(pBe,true,"",hDummy)).
        
        if pBe:ReadOnly = false then 
        do:
            oJson:Add('put',WritePutResource(pBe,true,"",hDummy)).
            if pBe:CanCreate  then 
                oJson:Add('post',WritePostResource(pBe,true,"",hDummy)).
            if pBe:CanDelete then 
                oJson:Add('delete',WriteDeleteResource(pBe,true,"",hDummy)).   
        end. 
        return ojson.
    end method. 
    
    method protected JsonObject WriteSubResourcePath(pBe as IBusinessEntity, pcSubSerializeName as character):
        define variable oJson as JsonObject no-undo.
         
        oJson = new JsonObject().
        oJson:Add('get',WriteGetSubResource(pBe,pcSubSerializeName)).
        
        return ojson.
    end method.
    
    method protected JsonObject WriteResourceServicePath(pService as IServiceEntity):
        return WriteResourceServicePath(pService,"").
    end method. 
    
    method protected JsonObject WriteResourceServicePath(pService as IServiceEntity, pcPropertyName as character):
        define variable pDummy as Parameter extent no-undo.
        return WriteResourceServicePath(pService,pcPropertyName,pDummy).
    end method. 
    
    
    method protected JsonObject WriteResourceServicePath(pService as IServiceEntity, pcMemberName as character,pParameters as Parameter extent):
        define variable oJson as JsonObject no-undo.
        
        oJson = new JsonObject().
        if pService:CanRead then
        do:
            if pService:MethodOperationType(pcMemberName) = "GET" then
                oJson:Add('get',WriteGetResource(pService,pcMemberName,pParameters)).
        end.
        if pService:CanCreate = true then
        do: 
            if pService:MethodOperationType(pcMemberName) = "POST" then
                oJson:Add('post',WritePostResource(pService,"label here",pParameters)).
        end.
        if pService:CanDelete = true then
        do: 
            if pService:MethodOperationType(pcMemberName) = "DELELE" then
                oJson:Add('delete',WritePostResource(pService,"label here",pParameters)).
        end.
        if pService:CanUpdate = true then
        do: 
            if pService:MethodOperationType(pcMemberName) = "PUT" then
                oJson:Add('put',WritePostResource(pService,"label here",pParameters)).
        end.
        return ojson.
    end method. 
    
    method protected JsonObject WriteResourceCollectionActionPath(pBe as IBusinessEntity,pAction as IAction,piSignatureNumber as integer):
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        AddResourceCollectionActionPath(oJson, pBe, pAction, piSignatureNumber).
        return ojson.
    end method.
    
    // separate add - since it may need to be added to path created by function
    method protected void AddResourceCollectionActionPath(pJson as JsonObject,pBe as IBusinessEntity,pAction as IAction,piSignatureNumber as integer):
        define variable hdummy as handle extent no-undo.
        
        pJson:Add('post',WriteAction(pBe,pAction,piSignatureNumber,true,"",hDummy)).
    end method.
    
    method protected JsonObject WriteResourceObjectActionPath(pBe as IBusinessEntity, pAction as IAction,piSignatureNumber as integer,pkeyfields as handle extent):
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        AddResourceObjectActionPath(oJson, pBe, pAction, piSignatureNumber, pkeyfields).
        return ojson.
    end method. 
    
    // separate add - since it may need to be added to path created by function
    method protected void AddResourceObjectActionPath(pJson as JsonObject,pBe as IBusinessEntity, pAction as IAction,piSignatureNumber as integer,pkeyfields as handle extent):
        define variable cLabels   as character no-undo.
        cLabels = GetFieldLabels(pkeyfields).
        pJson:Add('post',WriteAction(pBe,pAction,piSignatureNumber,false,cLabels,pkeyfields)).
    end method. 
    
    method protected JsonObject WriteResourceCollectionFunctionPath(pBe as IBusinessEntity,pfunction as Ifunction,piSignatureNumber as integer):
        define variable oJson as JsonObject no-undo.
        define variable hdummy as handle extent no-undo.
        oJson = new JsonObject().
        oJson:Add('get',WriteFunction(pBe,pFunction,piSignatureNumber,true,"",hdummy)).
        return ojson.
    end method.
  
    method protected JsonObject WriteResourceObjectFunctionPath(pBe as IBusinessEntity, pfunction as Ifunction,piSignatureNumber as integer,pkeyfields as handle extent):
        define variable oJson as JsonObject no-undo.
         
        define variable cLabels   as character no-undo.
        
        cLabels = GetFieldLabels(pkeyfields).
       
        oJson = new JsonObject().
        oJson:Add('get',WriteFunction(pBe,pFunction,piSignatureNumber,false,cLabels,pkeyfields)).
        
        return ojson.
    end method. 
    
    method protected character GetFieldLabels(pkeyfields as handle extent):
        define variable cLabels   as character no-undo.
        define variable cDlm      as character no-undo. 
        define variable iFld    as integer no-undo.
        do iFld = 1 to extent(pkeyfields):
            assign
                cLabels = cLabels + cDlm + BufferUtil:GetFieldLabel(pKeyfields[iFld]). 
            cDlm = ",".
                     
        end.
        return StringUtil:GetMessageFromEntries(cLabels).
    end method.    
    
    method protected JsonObject WriteResourceObjectPath(pBe as IBusinessEntity, pkeyfields as handle extent):
        define variable oJson     as JsonObject no-undo.
        define variable cLabels   as character no-undo.
        oJson = new JsonObject().
        
        cLabels = GetFieldLabels(pkeyfields).
        oJson:Add('get',WriteGetResource(pBe,false,cLabels,pKeyfields)).
        /** seem pointless since these mwthods works fine with the key in the json and not in the URL 
          TODO - are there cases that needs key -  body with child?   
         if pBe:ReadOnly = false then 
         do:
             oJson:Add('put',WritePutResource(pBe,false,cLabels,pKeyfields)).
             if pBe:CanCreate  then 
                 oJson:Add('post',WritePostResource(pBe,false,cLabels,pKeyfields)).
             if pBe:CanDelete then 
                 oJson:Add('delete',WriteDeleteResource(pBe,false,cLabels,pKeyfields)).   
         end.
         **/ 
        return ojson.
    end method.
    
    method protected JsonObject WriteResourceObjectChildPath(pBe as IBusinessEntity, pcChild as character, pCardinality as RelationTypeEnum, pkeyfields as handle extent):
        define variable oJson     as JsonObject no-undo.
        define variable cLabels   as character no-undo.
        oJson = new JsonObject().
        
         
        cLabels = GetFieldLabels(pkeyfields).
        oJson:Add('get',WriteGetResourceAndChild(pBe,pcChild,pCardinality,cLabels,pKeyfields)).
        /** seem pointless since these mwthods works fine with the key in the json and not in the URL 
          TODO - are there cases that needs key -  body with child?   
         if pBe:ReadOnly = false then 
         do:
             oJson:Add('put',WritePutResource(pBe,false,cLabels,pKeyfields)).
             if pBe:CanCreate  then 
                 oJson:Add('post',WritePostResource(pBe,false,cLabels,pKeyfields)).
             if pBe:CanDelete then 
                 oJson:Add('delete',WriteDeleteResource(pBe,false,cLabels,pKeyfields)).   
         end.
         **/ 
        return ojson.
    end method.
     
    method protected JsonObject WriteGetResource(pService as IServiceEntity, pcSecondSegment as character,pParameters as Parameter extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectName as character no-undo.
        define variable cDescription as character no-undo.
        define variable cDescriptiveMethodNames as character init "info,options" no-undo.
        define variable oJsonParameters as JsonArray no-undo.
        cObjectName = GetEndpointName(pService).
        oJson = new JsonObject().
        
        if pcSecondSegment > "" and pcSecondSegment <> "Execute" then
        do: 
            // if the service name des not end with a descritiove name then  add info
            cDescription = StringUtil:CamelToWords(pcSecondSegment).
            if lookup(entry(num-entries(cDescription,""),cDescription,""),cDescriptiveMethodNames) = 0 then
                cDescription = subst("&1 Info",cDescription). 
            
            // we will add "get" to the descrition so remove from the description if present  
            if cDescription begins "get " then 
            
                cDescription = substr(cDescription,5).
              
        end.        
        AddTagSummaryAndDescription(oJson,
            cObjectName,
            right-trim(
            subst("Get &1 &2",
            cObjectName,
            cDescription
            )
            ),
            "" //description 
            ).
        if pcSecondSegment = "" or pcSecondSegment = "Execute" and pService:IsDeprecated then
            oJson:Add("deprecated",true).
         
        if extent(pParameters) > 0 then 
            oJsonParameters = WriteMethodParameters(pService,pParameters).
        else 
            oJsonParameters = new JsonArray().
            
        AddApplicationGetServiceParameters(oJsonParameters,pService). 
        
        if oJsonParameters:length > 0 then      
            oJson:Add('parameters',oJsonParameters).  
        
        oJson:Add('responses',WriteGetResponses(true)).
        return ojson.
    end method.
    
    method protected JsonObject WriteGetResource(pBe as IBusinessEntity, plCollection as logical, pcLabel as character,phKeys as handle extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectName as character no-undo.
        
        cObjectName = GetEndpointName(pBe).
        oJson = new JsonObject().
        AddTagSummaryAndDescription(oJson,
            cObjectName,
            if plCollection 
            then subst("Get &1 data",cObjectName)
            else subst("Get &1 record by &2",cObjectName, if pcLabel > "" then pcLabel else "key"),
            "").
         
        if plCollection then 
        do:
            if DatasetUtil:GetNumTopBuffers(pbe:Datasets[1]) = 1 then
                oJson:Add('parameters',WriteGetCollectionParameters(pBe)).
            else   
                oJson:Add('parameters',WriteGetResourceParameters(pBe)).
        end.
        else  
        do:
            oJson:Add('parameters',WriteGetObjectParameters(pBe,phKeys)).
        end.   
        oJson:Add('responses',WriteGetResponses(plCollection)).
        return ojson.
    end method.
    
    method protected JsonObject WriteGetResourceAndChild(pBe as IBusinessEntity, pcChild as character, pCardinality as RelationTypeEnum, pcLabel as character,phKeys as handle extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectNames as character extent no-undo.
        define variable cChildinfo as character no-undo.
        oJson = new JsonObject().
        
        if RelationTypeEnum:OneToMany:Equals(pCardinality)  
        // dont add tag reference if child is not public
        // (they end up unsorted at the end of the UI since they were not added in AddTagd ) 
        and can-find(ttEntity where ttEntity.Resource = pcChild and ttEntity.public ) then 
        do: 
            // if one to many then add this also to the Tag for the child  
            extent(cObjectNames) = 2.
            cObjectNames[2] = GetEndpointName(pcChild,true).
            cChildInfo = StringUtil:UpperCaseFirst(pcChild). // keep plural in label
        end.
        else do: 
            // only add to the parent tag - seems unnecessary to add this to the child tag for many to one
            //  (it also adds a lot of endpoints for come entities) 
            extent(cObjectNames) = 1.
            cChildinfo = BufferUtil:SingularizeSerializeName(pcChild, true). 
        end.    
         
        cObjectNames[1] = GetEndpointName(pBe).
        AddTagsSummaryAndDescription(oJson,
            cObjectNames,
            subst("Get the &1 of the &2 by &3",
            cChildInfo,
            cObjectNames[1], 
            if pcLabel > "" then pcLabel else "key"),
            "").
         
        oJson:Add('parameters',WriteGetObjectParameters(pBe,phKeys)).
        oJson:Add('responses',WriteGetResponses(false)).
        return ojson.
    end method.
    
    method protected JsonObject WriteGetSubResource(pBe as IBusinessEntity,pcSerializeName as character):
        define variable oJson as JsonObject no-undo.
        define variable cObjectName as character no-undo.
        
        cObjectName = GetEndpointName(pBe).
        oJson = new JsonObject().
        AddTagSummaryAndDescription(oJson,
            cObjectName,
            subst("Get &1 &2 data",
            cObjectName,
            BufferUtil:SingularizeSerializeName(pcSerializeName,true)
            ),
            ""     
            ).        
                                     
         
        oJson:Add('parameters',WriteGetResourceParameters(pBe)).
        oJson:Add('responses',WriteGetResponses(false)).
        return ojson.
    end method.
    
    method protected JsonObject WriteFunction(pBe as IBusinessEntity, poFunction as IFunction,piSignatureNumber as integer, plCollection as logical, pcLabel as character,phKeys as handle extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectName as character no-undo.
        define variable cFunctionLabel as character no-undo.
        
        cObjectName = GetEndpointName(pBe).
        oJson = new JsonObject().
        cFunctionLabel = StringUtil:CamelToWords(poFunction:name).
        if entry(1,cFunctionLabel," ") = "get" then
            cFunctionLabel = StringUtil:RemoveEntryFromList(1,cFunctionLabel,"").
        AddTagSummaryAndDescription(oJson,
            cObjectName,
            if plCollection 
            then subst("Get &1 &2",
            cObjectName,
            cFunctionLabel
            )
            else subst("Get &1 &2 for one record by &3",
            cObjectName, 
            cFunctionLabel,
            if pcLabel > "" then pcLabel else "key"),
            "").
         
        
        if piSignatureNumber > 0 then 
            oJson:Add('parameters',WriteFunctionParameters(pBe,poFunction:signatures[piSignatureNumber], phKeys )).
        else 
            oJson:Add('parameters',WriteFunctionNoParamParameters(pBe,poFunction,phKeys)).
       
        oJson:Add('responses',WriteGetResponses(plCollection)).
        return ojson.
    end method.
    
    method protected JsonObject WriteAction(pBe as IBusinessEntity, poAction as IAction,piSignatureNumber as integer, plCollection as logical, pcLabel as character,phKeys as handle extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectName as character no-undo.
        define variable cActionLabel as character no-undo.
        
        cObjectName = GetEndpointName(pBe).
        oJson = new JsonObject().
        cActionLabel = StringUtil:CamelToWords(poAction:name).
        
        AddTagSummaryAndDescription(oJson,
            cObjectName,
            if plCollection 
            then subst("Submit &1 &2",
            cObjectName,
            cActionLabel
            )
            else subst("Submit &1 &2 for one record by &3",
            cObjectName, 
            cActionLabel,
            if pcLabel > "" then pcLabel else "key"),
            if poAction:IsDeprecated and poAction:DeprecatedWarning > ""  
            then subst("&1 &2",
                       poAction:DeprecatedWarning,
                       if poAction:DeprecatedRemoveCallInfo > ""
                       then poAction:DeprecatedRemoveCallInfo
                       else "")       
            else ""
            ).
         
        if poAction:IsDeprecated then 
            oJson:Add("deprecated",true).
         
        if poAction:CompareFields > "" and poAction:IsCreate then 
        do:
            
        
        end. 
        if piSignatureNumber > 0 then   
            oJson:Add('parameters',WriteActionParameters(pBe,poAction:signatures[piSignatureNumber], phKeys )).
        else if extent(phKeys) > 0 then 
            oJson:Add('parameters',WriteKeyParameters(pBe, phKeys)).
      //  else 
       //     oJsonArray = new JsonArray().    
               
        oJson:Add('responses',WriteGetResponses(plCollection)).
        if poAction:BeforeImageRequired then
        do:
            oJson:Add('requestBody',WritePutRequestBody(pBe,plCollection)).
        end.
        else do: 
            if  poAction:IsEmpty =  false 
                and MethodBindingEnum:Entity:Equals(poAction:Binding) = false
                and (poAction:IsCreate = false or poAction:CompareFields = "") then
            do:
                oJson:Add('requestBody',WritePostRequestBody(pBe,plCollection)).
            end.  
            
        end.
        return ojson.
    end method.
    
    
    method protected JsonObject WriteDeleteResource(pBe as IBusinessEntity, plCollection as logical, pcLabel as character,phKeys as handle extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectName as character no-undo.
        
        cObjectName = GetEndpointName(pBe).
        oJson = new JsonObject().
        AddTagSummaryAndDescription(oJson,
            cObjectName,
            if plCollection 
            then subst("Delete submitted &1 data",cObjectName)
            else subst("Delete submitted &1 record by &2",cObjectName, if pcLabel > "" then pcLabel else "key"),
            "").
         
        if plCollection = false then 
            oJson:Add('parameters',WriteKeyParameters(pBe, phKeys)).  
            
        oJson:Add('responses',WriteDeleteResponses(plCollection)).
        oJson:Add('requestBody',WriteDeleteRequestBody(pBe,plCollection)).
        return ojson.
    end method.
    
    method protected JsonObject WritePutResource(pBe as IBusinessEntity, plCollection as logical, pcLabel as character,phKeys as handle extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectName as character no-undo.
        
        cObjectName = GetEndpointName(pBe).
        oJson = new JsonObject().
        AddTagSummaryAndDescription(oJson,
            cObjectName,
            if plCollection 
            then subst("Update &1 data",cObjectName)
            else subst("Update &1 record by &2",cObjectName, if pcLabel > "" then pcLabel else "key"),
            "").
         
        if plCollection = false then 
            oJson:Add('parameters',WriteKeyParameters(pBe, phKeys)).  
            
        oJson:Add('responses',WriteGetResponses(plCollection)).
        oJson:Add('requestBody',WritePutRequestBody(pBe,plCollection)).
        return ojson.
    end method.
    
    method protected JsonObject WritePostResource(pBe as IBusinessEntity, plCollection as logical, pcLabel as character,phKeys as handle extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectName as character no-undo.
        
        cObjectName = GetEndpointName(pBe).
        oJson = new JsonObject().
        AddTagSummaryAndDescription(oJson,
            cObjectName,
            if plCollection 
            then subst("Create &1 data",cObjectName)
            else subst("Create &1 record by &2",cObjectName, if pcLabel > "" then pcLabel else "key"),
            "").
         
        if plCollection = false then 
            oJson:Add('parameters',WriteKeyParameters(pBe, phKeys)).  
            
        oJson:Add('responses',WriteGetResponses(plCollection)).
        oJson:Add('requestBody',WritePostRequestBody(pBe,plCollection)).
        return ojson.
    end method.
    
    method protected JsonObject WritePostResourceTwoTags(pBe as IBusinessEntity, pChildBe as IBusinessEntity,plCollection as logical, pcLabel as character,phKeys as handle extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectNames as character extent 2 no-undo.
        
        cObjectNames[1] = GetEndpointName(pBe).
        cObjectNames[2] = GetEndpointName(pChildBe).
        oJson = new JsonObject().
        
        AddTagsSummaryAndDescription(oJson,
            cObjectNames,
            if plCollection 
            then subst("XXX &1 data",cObjectNames[1])
            else subst("XXX &1 record by &2",cObjectNames[1], if pcLabel > "" then pcLabel else "key"),
            "").
         
        if plCollection = false then 
            oJson:Add('parameters',WriteKeyParameters(pBe, phKeys)).  
            
        oJson:Add('responses',WriteGetResponses(plCollection)).
        oJson:Add('requestBody',WritePostRequestBody(pBe,plCollection)).
        return ojson.
    end method.
    
    
    method protected JsonObject WritePostResource(pService as IServiceEntity, pcLabel as character,pParameters as Parameter extent):
        define variable oJson as JsonObject no-undo.
        define variable cObjectName as character no-undo.
        
        cObjectName = GetEndpointName(pService).
        oJson = new JsonObject().
        AddTagSummaryAndDescription(oJson,
            cObjectName,
            subst("Submit &1",cObjectName), 
            "").
         
        oJson:Add('parameters',WriteMethodParameters(pService,pParameters)).
           
        oJson:Add('responses',WriteGetResponses(true)).
     //   oJson:Add('requestBody',WritePostRequestBody(pBe,plCollection)).
        return ojson.
    end method.
    
    
    method protected JsonObject WritePutRequestBody(pBe as IBusinessEntity, plCollection as logical):
        return WriteUpdateRequestBody(pBe,"put",plCollection).
    end method.
    
    method protected JsonObject WriteDeleteRequestBody(pBe as IBusinessEntity, plCollection as logical):
        return WriteUpdateRequestBody(pBe,"delete",plCollection).
    end method.   
     
    method protected JsonObject WritePostRequestBody(pBe as IBusinessEntity, plCollection as logical): 
        return WriteUpdateRequestBody(pBe,"post",plCollection).
    end method.
     
    method protected JsonObject WriteUpdateRequestBody(pBe as IBusinessEntity, pcOperation as character, plCollection as logical):
        define variable oJson as JsonObject no-undo.
        define variable oContent as JsonObject no-undo.
        define variable oJsonAppContent as JsonObject no-undo.
        define variable oJsonAppSchema as JsonObject no-undo.
        define variable oJsonAppSchemaref as JsonObject no-undo.
        define variable cOperationText as character no-undo.
        oJson = new JsonObject().
        oContent = new JsonObject().
        oJsonAppContent = new JsonObject().
        oJsonAppSchema = new JsonObject().
        case pcoperation:
            when "put" then
                cOperationText = "modify in".
            when "delete" then
                cOperationText = "delete from".
            when "post" then
                cOperationText = "create in".
                
        end.
        
        oJson:add("description",
            subst("&1 to &2 the database",
            BufferUtil:SingularizeSerializeName(pBe:Resource),
            cOperationText
            )
            ).
        oJson:add("required",true).
        oJson:add("content",oContent).
        
        oContent:add("application/json",oJsonAppContent).
        oJsonAppContent:add("schema",oJsonAppSchema).
         
        if pcOperation = "put" then 
            AddSchemaReference(oJsonAppSchema, 
                OpenAPISchemaNameEnum:ArrayModifiedBody, 
                pBe:resource).
         
        else    
            AddSchemaReference(oJsonAppSchema, 
                OpenAPISchemaNameEnum:ArrayBody, 
                pBe:resource).
       
        return oJson.
  
    end method.    
    
    method protected void AddTagSummaryAndDescription(pJson as JsonObject,pcObjectName as character,pcSummary as character, pcDescription as character ):
        define variable cNamesAsExtent as character extent 1 no-undo.
        cNamesAsExtent[1] = pcObjectName.
        AddTagsSummaryAndDescription(pJson, cNamesAsExtent,pcSummary, pcDescription).
    end method.
    
    method protected void AddTagsSummaryAndDescription(pJson as JsonObject,pcObjectNames as character extent, pcSummary as character,pcDescription as character):
        AddTagLinks(pJson, pcObjectNames).
        pJson:Add('summary',pcSummary).
        pJson:Add('description',pcDescription).
        pJson:Add('operationId','').
    end method.
    
    method protected JsonArray WriteGetCollectionParameters( pBe as IBusinessEntity):
        define variable oJsonArray as JsonArray no-undo.
        oJsonArray = new JsonArray().
        
        oJsonArray:Add(WriteParameter("top","query","Max Number of Records",false,"integer","20")).
        oJsonArray:Add(WriteParameter("filter","query","Filter",false,"character","")).
        oJsonArray:Add(WriteParameter("expand","query","Expand",false,"character","")).
        oJsonArray:Add(WriteParameter("select","query","Select Fields",false,"character","")).
        oJsonArray:Add(WriteParameter("orderBy","query","Sort Fields",false,"character","")).
        // let subclasses add their own before timeout and metadata
        AddApplicationGetCollectionParameters(oJsonarray, pBe).
        oJsonArray:Add(WriteParameter("timeout","query","Timeout in Seconds",false,"integer",string(SessionManager:QueryTimeout))).
        
        oJsonArray:Add(WriteParameter("metadata","query","Meta Data",false,"character","none")).
        
        return oJsonArray.
             
    end method.
    
    method protected JsonArray WriteFunctionNoParamParameters( pBe as IBusinessentity, pFunction as IFunction, phKeys as handle extent):
        define variable oJsonArray as JsonArray no-undo.
        
        if extent(phKeys) <> ? then
            oJsonArray = WriteKeyParameters(pBe, phKeys).
        else 
            oJsonArray = new JsonArray().    
        
        if pFunction:IsPrimitive then
        do: 
            if pFunction:name = "Count" then 
            do:
                oJsonArray:Add(WriteParameter("filter","query","Filter",false,"character","")).
             //   AddApplicationGetObjectParameters(oJsonarray, pBe).
                oJsonArray:Add(WriteParameter("timeout","query","Timeout in Seconds",false,"integer",string(SessionManager:QueryTimeout))).
            end.
        end.
        else do: 
            oJsonArray:Add(WriteParameter("expand","query","Expand",false,"character","")).
            oJsonArray:Add(WriteParameter("select","query","Select Fields",false,"character","")).
            oJsonArray:Add(WriteParameter("orderBy","query","Sort Fields",false,"character","")).
            // let subclasses add their own before timeout and metadata
         //   AddApplicationGetObjectParameters(oJsonarray, pBe).
            oJsonArray:Add(WriteParameter("timeout","query","Timeout in Seconds",false,"integer",string(SessionManager:QueryTimeout))).
        
            oJsonArray:Add(WriteParameter("metadata","query","Meta Data",false,"character","none")).
      
        end. 
        return oJsonArray.
    end method.
    
    method protected JsonArray WriteGetResourceParameters( pBe as IBusinessEntity):
        define variable oJsonArray as JsonArray no-undo.
        oJsonArray = new JsonArray().
        oJsonArray:Add(WriteParameter("select","query","Select Fields",false,"character","")).
        // let subclasses add their own before metadata
        AddApplicationGetObjectParameters(oJsonarray, pBe).
        oJsonArray:Add(WriteParameter("metadata","query","Meta Data",false,"character","none")).
        
        return oJsonArray.
             
    end method.
    
    method protected JsonArray WriteGetObjectParameters( pBe as IBusinessEntity, phKeys as handle extent):
        define variable oJsonArray as JsonArray no-undo.
      
        define variable cDataType as character no-undo.
        
        // returns array also if no keys
        oJsonArray = WriteKeyParameters( pBe, phKeys).
          
        oJsonArray:Add(WriteParameter("expand","query","Expand",false,"character","")).
        oJsonArray:Add(WriteParameter("select","query","Select Fields",false,"character","")).
        // let subclasses add ther won before metadats
        AddApplicationGetObjectParameters(oJsonarray, pBe).
        oJsonArray:Add(WriteParameter("metadata","query","Meta Data",false,"character","none")).
        
        return oJsonArray.
             
    end method.
   
    method protected JsonArray WriteKeyParameters( pBe as IBusinessEntity, phKeys as handle extent):
        define variable oJsonArray as JsonArray no-undo.
        define variable i as integer no-undo.
      
        oJsonArray = new JsonArray().
        do i = 1 to extent(phKeys):
            oJsonArray:Add(WriteParameter(phKeys[i]:serialize-name,"path",BufferUtil:GetFieldLabel(phKeys[i]),true,phKeys[i]:data-type,"")).
        end.    
        return oJsonArray.
             
    end method.
    
    method protected JsonArray WriteActionParameters(pBe as IBusinessEntity, poSignature as ISignature, phKeys as handle extent ):
        define variable oJsonArray as JsonArray no-undo.
        if extent(phKeys) <> ? then
            oJsonArray = WriteKeyParameters(pBe, phKeys).
        else 
            oJsonArray = new JsonArray().    
            
        AddMethodParameters(oJsonArray,poSignature,false).
        
     //   AddFunctionParameters(oJsonarray, pBe).
        
        return oJsonArray.
             
    end method.
    
    method protected JsonArray WriteFunctionParameters(pBe as IBusinessEntity, poSignature as ISignature, phKeys as handle extent ):
        define variable oJsonArray as JsonArray no-undo.
        if extent(phKeys) <> ? then
            oJsonArray = WriteKeyParameters(pBe, phKeys).
        else 
            oJsonArray = new JsonArray().    
        AddMethodParameters(oJsonArray,poSignature,true).
        
        AddApplicatioFunctionParameters(oJsonarray, pBe).
        
        return oJsonArray.
             
    end method.
    
    method protected JsonArray WriteMethodParameters( poService as IServiceEntity, poParameters as Parameter extent ):
        define variable oJsonArray as JsonArray no-undo.
        define variable i as integer no-undo.
        define variable cPublicname as character no-undo.
        oJsonArray = new JsonArray().
        do i = 1 to extent(poParameters):
       //     message poService:resource poParameters[i]:name poParameters[i]:extent
         //   view-as alert-box.
            cPublicName = poService:GetPublicParameterName(poParameters[i]:name).
            oJsonArray:Add(WriteParameter(
                cPublicName,
                "path",
                StringUtil:CamelToWords(cPublicName),
                true,
                               // the extent is ? here if array 
                if poParameters[i]:extent = 0 
                then poParameters[i]:dataType:toString()  
                else subst("&1Array",poParameters[i]:dataType:toString()),  
                ""
                )
                ).
        end.
        
        return oJsonArray.
    end method.
    
    method protected JsonArray WriteMethodParameters( poSignature as ISignature, plIsfunction as logical ):
        define variable oJsonArray as JsonArray no-undo.
        oJsonArray = new JsonArray().
        AddMethodParameters(oJsonArray, poSignature, plIsfunction).
        return oJsonArray.
    end method.
    
    method protected void AddMethodParameters( poJsonArray as JsonArray, poSignature as ISignature, plIsfunction as logical ):
     
        define variable i as integer no-undo.
        do i = 1 to extent(poSignature:parameters):
            if IsArray(poSignature:Parameters[i]:DataType) then 
            do:
                poJsonArray:Add(
                    WriteParameter(
                    subst("@&1",poSignature:Parameters[i]:name),
                    "query",
                    StringUtil:CamelToWords(poSignature:Parameters[i]:name),
                    true,
                    poSignature:Parameters[i]:DataType:ToString(),
                    ""
                    )
                    )  .
            end.   
            else 
                poJsonArray:Add(WriteParameter(
                    poSignature:Parameters[i]:name,
                    "path",
                    StringUtil:CamelToWords(poSignature:Parameters[i]:name),
                    true,
                    poSignature:Parameters[i]:DataType:ToString(),
                    ""
                    )
                    ).
        end.    
              
    end method.
    
    // bug in ABL core dataTypeHelper does no use GetMask - fixed in some future version
    method protected logical IsArray(input poDataType as DataTypeEnum):
        return (entry(num-entries(DataTypeHelper:GetMask(poDataType), ' ':u), 
            string(DataTypeHelper:GetMask(poDataType)), ' ':u) eq 'extent':u).
    end method. 
     
    // for subclass override
    method protected void AddApplicationGetObjectParameters(pJsonarray as JsonArray,pBe as IBusinessEntity) :
    end method.
    
     // for subclass override
    method protected void AddApplicationGetCollectionParameters(pJsonarray as JsonArray,pBe as IBusinessEntity) :
    end method.
    
      // for subclass override
    method protected void AddApplicationGetResourceParameters(pJsonarray as JsonArray,pBe as IBusinessEntity) :
    end method.
    
     // for subclass override
    method protected void AddApplicatioFunctionParameters(pJsonarray as JsonArray,pBe as IBusinessEntity) :
    end method.
     
    // for subclass override
    method protected void AddApplicationGetServiceParameters(pJsonarray as JsonArray,pService as IServiceEntity) :
    end method.
    
    method protected JsonObject WriteParameter(
        pname as character,
        pIn as character,
        pDescription as character,
        plRequired as logical,
        pType as character, 
        pcExample as char
        ):
        define variable oJson as JsonObject no-undo.
        define variable oJsonSchema as JsonObject no-undo.
        define variable oJsonSchemaItems as JsonObject no-undo. 
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
                    oJsonSchema:Add("format","int32").
                end.
            when "integerArray" then
                do: 
                    if pcExample > "" then
                        oJson:Add("example",integer(pcExample)).
                    oJsonSchema:Add("type","array").
            //   oJsonSchema:Add("explode",false).
                    oJsonSchemaItems = new JsonObject().
                    oJsonSchema:Add("items",oJsonSchemaItems).
                    oJsonSchemaItems:Add("type","integer").
                end.
            when "int64" then
                do: 
                    if pcExample > "" then
                        oJson:Add("example",int64(pcExample)).
                    oJsonSchema:Add("type",lc("integer")).
                    oJsonSchema:Add("format","int64").
                end.
            when "character" then
                do: 
                    if pcExample > "" then
                        oJson:Add("example",pcExample).
                    oJsonSchema:Add("type","string").
                end. 
            when "characterArray" then
                do: 
                    if pcExample > "" then
                        oJson:Add("example",integer(pcExample)).
                    oJsonSchema:Add("type","array").
                    oJsonSchemaItems = new JsonObject().
                    oJsonSchema:Add("items",oJsonSchemaItems).
                    oJsonSchemaItems:Add("type","string").
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
            when "decimalArray" then
                do: 
                    if pcExample > "" then
                        oJson:Add("example",integer(pcExample)).
                    oJsonSchema:Add("type","array").
                    oJsonSchemaItems = new JsonObject().
                    oJsonSchema:Add("items",oJsonSchemaItems).
                    oJsonSchemaItems:Add("type","number").
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
     //   oJson:Add("default",WriteDefaultResponse()). 
        return oJson.
    end method.
    
    method protected JsonObject WriteDeleteResponses(plCollection as logical):
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        
        oJson:Add(string(StatusCodeEnum:NoContent:GetValue()),WriteResponseDescription(StatusCodeEnum:NoContent)). 
        if not plCollection then 
            AddResponseStatus(oJson,StatusCodeEnum:NotFound).
        return oJson.
    end method.
    
    
    method protected void AddResponseStatus(pJson as JsonObject, pStatusCodeEnum as StatusCodeEnum):
        pJson:Add(string(pStatusCodeEnum:GetValue()),WriteGetResponse(pStatusCodeEnum)). 
    end method.
    
    method protected JsonObject WriteDefaultResponse():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add("description","Default error response"). 
        return oJson.
        
    end method.
   
    method protected JsonObject WriteGetResponse(pEnum as StatusCodeEnum):
        define variable oJson as JsonObject no-undo.
     //  define variable oJsonContent as JsonObject no-undo.
     //  define variable oJsonContentType as JsonObject no-undo.
        oJson = new JsonObject().
    //   oJsonContent = new JsonObject().
    //   oJsonContentType = new JsonObject().
     //  oJsonContent:add("application/json",oJsonContentType). 
        oJson:Add("description",StringUtil:CamelToWords(penum:ToString())). 
     //  oJson:Add("content",oJsonContent).
      
        return oJson.
    end method.
    
    method protected JsonObject WriteResponseDescription(pEnum as StatusCodeEnum):
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:Add("description",StringUtil:CamelToWords(penum:ToString())). 
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

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    destructor public OpenAPIWriter ( ):
        for each ttFixedMember:
            if valid-object(ttFixedMember.memberObject) then
                delete object ttFixedMember.memberObject.
        end.
        empty temp-table ttFixedMember.     
    end destructor.
   
    
end class.