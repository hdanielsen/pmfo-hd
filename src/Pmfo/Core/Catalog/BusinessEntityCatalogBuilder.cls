 
 /*------------------------------------------------------------------------
    File        : ResourceCatalogBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Apr 11 07:39:02 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IDatasetRelation from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.Catalog.IBusinessEntityCatalogBuilder from propath.
using Pmfo.Core.Catalog.ICatalogRequest from propath.
using Pmfo.Core.Catalog.ResourceCatalogBuilder from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.DatasetRelation from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.MapEntry from propath.
using Pmfo.Core.Common.IFunction from propath.
using Progress.Reflect.Method from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Core.Catalog.BusinessEntityCatalogBuilder inherits ResourceCatalogBuilder implements IBusinessEntityCatalogBuilder: 
  
    define protected property Links             as ISet          no-undo get. set. 
    define protected property Functions         as IMap          no-undo get. set. 
    define protected property Actions           as IMap          no-undo get. set. 
    define protected property Resource          as character     no-undo get. set.
    define protected property InheritsResource  as character     no-undo get. set.
    define protected property PrimitiveFunctionMap as IStringStringMap no-undo 
        get():
            if not valid-object(PrimitiveFunctionMap) then 
                PrimitiveFunctionMap = new StringStringMap().
            return PrimitiveFunctionMap.        
        end. 
        set.
    define protected variable ResourceTextField as character no-undo.
     
    define public property ServiceManager as IServiceManager no-undo
        get():
            return cast(Application:ServiceManager,IServiceManager).
        end get.
    
    // used to sort properties 
    define temp-table ttField 
         field name as character
         field fieldhandle as handle 
         index name as primary unique name.          
          
          
        /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public BusinessEntityCatalogBuilder ( ):
        super ().
    end constructor.
    
    // no override allowed - subclasses overrides BuildCatalog(IBusinessEntity) 
    method public final override JsonObject BuildCatalog(pRequest as ICatalogRequest, pEntity as IEntity):
        define variable oBusinessEntity as IBusinessEntity no-undo.
        Request = pRequest.
        if type-of(pEntity,IBusinessEntity) then
        do: 
            return BuildCatalog(cast(pEntity,IBusinessEntity)).
        end.
        else 
            undo, throw new IllegalArgumentError(subst("Illegal type '&1'. Must be IBusinessEntity.",pEntity:GetClass():TypeName)).
    end method.
    
    /* Stores a map of the potential primitive functions returntype to show in Functions
       scalr functions:  public, decalredonly and returntype not an object or handle   
     */
    // This should be improved 
    // - as of current the businessentoity is not stored but all properties aree drrrived form the entity 
    // - we may store the entity and then use more target dsue of reflection when the build needs the method info 
    method protected void AddPrimitiveMethodstoMap(pEntity as IBusinessEntity):
        define variable oMethods as Method extent no-undo.
        define variable i        as integer no-undo.
        oMethods = pEntity:GetClass():GetMethods(Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance or Progress.Reflect.Flags:DeclaredOnly).
        do i = 1 to extent(oMethods):
            if oMethods[i]:ReturnTypeName = ? and Progress.Reflect.DataType:Void <> oMethods[i]:ReturnType and Progress.Reflect.DataType:Handle <> oMethods[i]:ReturnType then
            do: 
                PrimitiveFunctionMap:put(oMethods[i]:name,oMethods[i]:ReturnType:ToString() + if oMethods[i]:ReturnExtent > 0 then " extent" else "").    
            end.  
        end.  
    end method.    
    
    method protected JsonObject BuildCatalog(pEntity as IBusinessEntity):
        define variable oBusinessEntity as IBusinessEntity no-undo.
         
        pentity:PrepareCatalog(). // loads lazy laoded actions and such oif applicable 
        Links = pEntity:Links.
        Functions = pEntity:Functions.
        Actions = pEntity:Actions.
        Resource = pEntity:Resource.
        ResourceTextField = pEntity:GetPublicTextField().
        InheritsResource = pEntity:InheritsResource.
        AddPrimitiveMethodstoMap(pEntity).
        
        return BuildCatalog(pEntity:Resource, pEntity:DataSets, MethodsAllowedList(pEntity) ,if pEntity:HasRedirectMap then pEntity:RedirectMap else ?).
        
    end method.
    
    method protected JsonObject BuildCatalog(pResource as char, pDataSets as handle extent, pcOperations as char extent, pRedirectMap as IStringStringMap ):
        define variable json               as JsonObject no-undo.     
        define variable jsonResourceArray  as JsonArray  no-undo.
        define variable idataset           as integer    no-undo.
        define variable istart             as integer    no-undo.
        define variable cPath              as character  no-undo.
        define variable cEmpty             as character  extent no-undo.
        define variable jsonChild          as JsonObject no-undo.
        define variable jsonfunctions      as JsonArray  no-undo.
        define variable jsonActions        as JsonArray  no-undo.
       
        define variable lTypeSet as logical no-undo.
        if pDatasets[1]:serialize-name = pResource then 
        do:
            json = GetDataset(pDatasets[1],"",false). // blank means add path - ? is no path
            iStart = 2.                        
        end.
        if not valid-object(json) then
        do:
            json = new JsonObject().
            json:Add("name",pResource).
            json:Add("type","container").
            AddPath(json,"/" + pResource).
            iStart = 1.
        end.
        
        json:Add("methods",GetJsonArray(pcOperations)).
        
      //  json:Add("metricSupport",pMetricSupport:size > 0).
        if extent(pDatasets) >= iStart then
        do:
            if json:GetCharacter("type") = "entity" then
                json:Set("type", "entity & container").
            lTypeSet = true.    
            jsonResourceArray = new JsonArray().
            json:Add("resources",jsonResourceArray).
         
            do iDataset = iStart to extent(pDatasets):     
                jsonChild = GetDataset(pDatasets[iDataset],"/" + pResource,false).
                jsonChild:Add("methods",GetJsonArray(Array:ToArray("GET"))).
                jsonResourceArray:Add(jsonChild).                                   
            end.    
        end.  
        if valid-object(pRedirectMap) and pRedirectMap:size > 0 then
        do:
            if not lTypeSet then
            do:
                if json:GetCharacter("type") = "entity" then
                    json:Set("type", "entity & container").
            end.
            if not valid-object(jsonResourceArray) then
            do:
                jsonResourceArray = new JsonArray().
                json:Add("resources",jsonResourceArray).
            end.    
            AddRedirect(jsonResourceArray, pRedirectMap,"/" + pResource). 
        end.  
        // hook for subclasses 
        AddResourceProperties(json).
        jsonFunctions = GetFunctions("/" + pResource,pDatasets[1]:get-buffer-handle(1)).
        if valid-object(jsonfunctions) and jsonFunctions:length > 0 then
        do:
            json:Add("functions",jsonFunctions).
        end.
        jsonActions = GetActions("/" + pResource,pDatasets[1]:get-buffer-handle(1)).
        if valid-object(jsonActions) and jsonActions:length > 0 then
        do:
            json:Add("actions",jsonActions).
        end.
        
        return json.
  
    end method.
    
    method protected JsonArray GetActions(pcPath as char, phBuffer as handle):
        return GetMethods(pcPath,phBuffer,Actions). 
    end method.    
    
    method protected JsonArray GetFunctions(pcPath as char,phBuffer as handle):
        return GetMethods(pcPath,phBuffer,Functions). 
    end method.    
    
    method protected JsonArray GetMethods(pcPath as char, phBuffer as handle,pMethods as IMap):
        define variable jsonArray     as JsonArray  no-undo.
        define variable oIterator     as IIterator  no-undo.
        define variable oMethod     as IMethod    no-undo.
        
        jsonArray = new jsonArray().
        if valid-object(pMethods) and pMethods:Size > 0 then 
        do:
            oIterator = pMethods:Values:Iterator().
            do while oiterator:hasNext():
                oMethod = cast(oIterator:Next(),IMethod).
                jsonArray:Add(GetMethod(oMethod,pcPath,phBuffer)).
            end. 
        end.   
        return jsonArray.
    end method.    
    
    
    method protected JsonObject GetDataset (phDataset as handle, pcPath as character, plRedirect as logical):
        if phDataset:num-top-buffers = 1 and phDataset:get-top-buffer(1):serialize-name = phDataset:serialize-name then 
        do:  
            return GetEntity(phDataset:get-top-buffer(1),pcPath,?).
        end.
        else do:
            return GetContainer(phDataset,pcPath,plRedirect).     
        end.    
    end.
    
    method protected JsonObject GetContainer (phDataset as handle):
        return GetContainer(phDataset,?,false).
    end method.    
    
   
    method protected JsonObject GetContainer (phDataset as handle,pcPath as character, plRedirect as logical):
        define variable json        as JsonObject no-undo.
        define variable jsonArray   as JsonArray  no-undo.
        define variable i           as integer    no-undo.
        define variable cPath       as character no-undo.
        
        if pcPath <> ? then
        do:
            if plRedirect then
                cPath = pcPath.
            else 
                cPath = pcPath + "/" + phDataset:serialize-name.
        end.
        else 
            cPath = phDataset:serialize-name.
        
        json = new JsonObject().
        json:Add("name",phDataset:serialize-name).
        if pcPath <> ? then 
        do:
            AddPath(json,cPath).
        end.
        json:Add("type","container").
        // hook for subclasses
        AddContainerProperties(json, phDataset). 
        jsonArray = new JsonArray().
        json:Add("resources",jsonArray).
        do i = 1 to phdataset:num-top-buffers:
            jsonArray:Add(GetEntity(phDataset:get-top-buffer(i),pcPath,?)). 
        end.
         
        return json.    
        
    end method.    
    
    method protected character GetKeyTemplate(pcKeys as character extent):
        define variable cValue as character no-undo.
        define variable i      as integer no-undo.
        define variable cDlm   as character no-undo.
        if extent(pcKeys) = 1 then
            cValue = "@" + pcKeys[1].
        else do i = 1 to extent(pcKeys):
            cValue = cValue 
                   + cDlm
                   + subst("&1=&2",pcKeys[i],"@" + pcKeys[i]).
            cDlm = ",".   
        end.           
        return subst("(&1)",cValue).         
    end method.    
    
    method protected JsonObject GetEntity(phBuffer as handle, pcPath as character,phRelation as handle):
        define variable json          as JsonObject no-undo.
        define variable jsonRecursive as JsonObject no-undo.
        define variable iNumChildren  as integer    no-undo.
        define variable iChild        as integer    no-undo.
        define variable jsonArray     as JsonArray  no-undo.
        define variable cObjectPath   as character  no-undo init ?.
        define variable hChildRel     as handle     no-undo.
        define variable cKeys         as character  no-undo extent.
        define variable oIterator     as IIterator  no-undo.
        define variable jsonLinks     as JsonArray  no-undo.
        define variable oRelation     as IRelation  no-undo.
        define variable cRowPath      as character  no-undo.
        define variable oEntity       as IBusinessEntity no-undo.        
        define variable cInherits     as character no-undo.
        define variable cMethodnames  as character extent no-undo.
        define variable oJsonActionArray as JsonArray no-undo.
        define variable oJsonFunctionArray as JsonArray no-undo.
        define variable i as integer no-undo.
        define variable cTextField   as character no-undo.
        json = new JsonObject().
        
        json:Add("name",phBuffer:serialize-name).
        
        if valid-object(phBuffer:parent-relation) then
        do:
            if valid-object(Links) and Links:Size > 0 then 
            do:
                oIterator = Links:Iterator().
                jsonLinks = new jsonArray().
                do while oiterator:hasNext():
                    oRelation = cast(oIterator:Next(),IRelation).
                  
                    if oRelation:ParentName = phBuffer:parent-relation:parent-buffer:serialize-name
                    and oRelation:PublicName = phBuffer:serialize-name then
                    do:
                        if type-of(oRelation,IDatasetRelation) then 
                        do:
                            json:Add("deactivatedMethods",GetJsonArray(cast(oRelation,IDatasetRelation):DeactivateMethods)).
                            cMethodnames = cast(oRelation,IDatasetRelation):MethodNames.
                            if extent(cMethodnames) <> ? then
                            do:
                                oJsonActionArray = new jsonArray().
                                oJsonFunctionArray = new jsonArray().
                                do i = 1 to extent(cMethodnames):
                                    if Actions:ContainsKey(new String(cMethodNames[i])) then
                                    do:
                                        oJsonActionArray:Add(cMethodNames[i]).
                                    end.
                                    if Functions:ContainsKey(new String(cMethodNames[i])) then
                                    do:
                                       oJsonFunctionArray:Add(cMethodNames[i]).
                                    end.    
                                end.
                                if oJsonActionArray:Length > 0 then
                                    json:Add('activatedParentActions',oJsonActionArray).
                                     
                                if oJsonFunctionArray:Length > 0 then
                                    json:Add('activatedParentFunctions',oJsonFunctionArray).
                                    
                            end.
                        end.    
                    end.    
                end. 
            end.   
        end.
        
        cObjectPath = subst("&1/&2",pcPath,phBuffer:serialize-name).
        AddPath(json,cObjectPath).
        json:Add("type","entity").
        if phBuffer:serialize-name = Resource then
        do:
            cInherits = InheritsResource.
            cTextField = ResourceTextField.
        end.    
        else do on error undo, throw:
            oEntity = cast(ServiceManager:CreateBusinessEntity(phBuffer:table-handle:serialize-name),IBusinessEntity).
            cInherits = oEntity:InheritsResource.
            cTextField = oEntity:GetPublicTextField().
            catch e as Progress.Lang.Error :
            //        
            end catch.
        end.
        if cInherits > "" then
             json:Add("inherits",cInherits).
        
        cKeys = Array:Split(ServiceInfo:getPublicKeys(phBuffer)).
        
        // no key add empry array
        if extent(cKeys) = 1 and ckeys[1] = "" then
            json:Add("key",new JsonArray()).
        else do: 
            cRowPath = subst("&1&2",cObjectPath,GetKeyTemplate(cKeys)).
            json:Add("key",GetJsonArray(ckeys)).
        end.
        
        json:Add("textField",cTextField).
        
        if valid-object(phRelation) then
        do:
            AddRelation(json,phRelation).
        end.
        // hook for subclasses
        
        AddBufferProperties(json, phBuffer,oEntity).
        
        json:Add("properties",GetProperties(phBuffer,oEntity)).
        
        if cRowPath > "" and phBuffer:num-child-relations > 0 then 
        do: 
            jsonArray  = new JsonArray().
            do iChild = 1 to phBuffer:num-child-relations:
                hChildRel = phBuffer:get-child-relation(iChild).
                if hChildRel:recursive then
                do:
                    jsonRecursive = new JsonObject().
                    jsonRecursive:Add("name",hChildRel:child-buffer:serialize-name).
                    jsonRecursive:Add("type","entity").
                    jsonRecursive:Add("recursive",true).
                    AddRelation(jsonRecursive,hChildRel).
                    jsonArray:Add(jsonRecursive).
                end.
                else if hchildRel:reposition = false then
                do:
                    jsonArray:Add(GetEntity(phBuffer:get-child-relation(iChild):child-buffer,cRowPath,hChildrel )). 
                end.
            end.
            if JsonArray:length > 0 then
                json:Add("entities",jsonArray).
            
        end.
        if cRowPath > "" and valid-object(Links) and Links:Size > 0 then 
        do:
            oIterator = Links:Iterator().
            jsonLinks = new jsonArray().
            do while oiterator:hasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                // skip dataset relations since they are listed as entities 
                if type-of(oRelation,IDatasetRelation) = false
                and oRelation:ParentName = phBuffer:serialize-name then
                    jsonLinks:Add(GetLink(phBuffer,oRelation,cRowPath)).
            end. 
            if jsonLinks:length > 0 then
                json:Add("links",jsonLinks).            
        end.   
        return json.
    end.
    
    method protected void AddRelation (pJson as JsonObject,phRelation as handle):
        define variable cParentJoin as character no-undo.
        define variable cForeignKey as character no-undo.
        // as of current we  are hiding the foreignkey for recursive on the operationhandler
        pjson:Add("foreignKeyHidden",phRelation:recursive or phRelation:foreign-key-hidden).
        BufferUtil:SplitJoinFields(phRelation, output cParentJoin, output cForeignKey).
        pjson:Add("foreignKey",GetJsonArray(Array:Split(cForeignKey))).
        pjson:Add("parentJoin",GetJsonArray(Array:Split(cParentJoin))).
    end method.    
    
    method protected void AddRedirect (pJsonArray as JsonArray,pRedirect  as IStringStringMap, pcPath as character):
        define variable oIterator as IIterator  no-undo.
        define variable oMapEntry as MapEntry  no-undo.
        define variable oEntity   as IBusinessEntity no-undo.
        define variable cResource as character no-undo.
        define variable hDataset  as handle no-undo.
        define variable Jsonchild as JsonObject no-undo.
        
        oIterator = pRedirect:EntrySet:Iterator().
        do while oIterator:HasNext().
            oMapEntry = cast(oIterator:Next(),MapEntry).
            cResource = cast(oMapEntry:Value,String):Value.
            oEntity = cast(ServiceManager:CreateBusinessEntity(cResource),IBusinessEntity) no-error.
            if valid-object(oEntity) then
            do: 
                jsonChild = GetDataset(oEntity:Datasets[1],pcPath + "/" + oMapEntry:Key:ToString(),true).
                jsonChild:Add("methods",GetJsonArray(Array:ToArray("GET"))).
                pjsonArray:Add(jsonChild).
                                        
            end.  
        end.      
    end method.
    
    method protected JsonObject GetLink(phBuffer as handle,pRelation as IRelation,pcPath as character):
        define variable json as JsonObject no-undo.
        define variable i as integer no-undo.
        define variable cSearchPath as character no-undo init ?.
      
        define variable cExpression as character no-undo.
        define variable oEntity     as IBusinessEntity no-undo.
        define variable oFunction   as IMethod no-undo.
        define variable cDlm        as character no-undo.
        define variable cLinkName   as character no-undo.
        define variable cEntityName as character no-undo.
        define variable cPath       as character no-undo.
        json = new JsonObject().
        
        if not RelationTypeEnum:OneToMany:Equals(pRelation:Cardinality) then
        do:
            cLinkName   = if pRelation:AliasName > "" then pRelation:AliasName else pRelation:Name. 
            cEntityName = pRelation:Name.     
        end.
        else do:
            cLinkName   = pRelation:Name. 
            cEntityName = pRelation:Name.     
        end.
        
        if pRelation:Public then
            cPath = subst("&1/&2",Request:Path,cEntityName).
        else      
            cPath = subst("&1&2/&3",Request:Path,pcPath,cEntityName).
        
        json:Add("name",cLinkName).        
        json:Add("entity",cEntityName).
        json:Add("type",pRelation:Cardinality:ToString()).
        
        oEntity = cast(ServiceManager:CreateBusinessEntity(pRelation:name),IBusinessEntity) no-error.
        
        if valid-object(oEntity) then 
        do:    
        
            AddLinkProperties(json,oEntity).
                    
            // addd attributes for ManyToOne - Note: OneToOne is treated as a regular child here     
            if RelationTypeEnum:ManyToOne:Equals(pRelation:Cardinality) then
            do:
                cSearchPath = cPath.
                if pRelation:LookupFunction > "" then
                do on error undo, throw:
                    json:Add("searchPath",pRelation:LookupFunction). 
                    oFunction = cast(oEntity:Functions:Get(new String(pRelation:LookupFunction)),IMethod).
                    cSearchPath = subst("&1/&2",cSearchPath,oFunction:GetPublicLinkTemplate(1)).
                    catch e as Progress.Lang.Error :
                        cSearchPath = subst("&1/&2(- missing info -)",cSearchPath,pRelation:LookupFunction).
                    end catch.
                end.    
                cExpression = pRelation:GetPublicKeyTemplate(phBuffer).   
                // no lookup for one to one - (?) not likely to pick from a list in that case                                                                           
                json:Add("lookupPath",cSearchPath).         
                cPath = subst("&1(&2)",cPath,cExpression).
                json:Add("pathTemplate",cPath).
            end.
            else do:
                json:Add("path",cPath).
            end.
        end.
        return json.
    end.
    
    method protected void AddLinkProperties(pJson as jsonObject,pEntity as IBusinessEntity):
    end method.    
    
    method protected character GetPropertyNames(phBuffer as handle, pcFieldList as char):
        define variable iField as integer no-undo.
        define variable hField as handle no-undo.
        define variable cList as character no-undo.
        do iField = 1 to num-entries(pcFieldList):
            hField = phBuffer:buffer-field(entry(iField,pcFieldList)) no-error.
            cList = cList
                  + (if iField = 1 then "" else ",")
                  + if valid-handle(hField) then hField:serialize-name else "null".

        end.
        return cList.
    end method.
    
    method protected JsonObject GetMethod(pMethod as IMethod,pcPath as character, phbuffer as handle):
        define variable json as JsonObject no-undo.
        define variable i as integer no-undo.
        define variable cSearchPath as character no-undo init ?.
        define variable cJoinPath   as character no-undo.
        define variable cExpression as character no-undo.
        define variable cDlm as character no-undo.
        define variable jsonSignatures as JsonArray no-undo.
        define variable cFieldList as character no-undo.
        define variable cFuncType as character no-undo.
        json = new JsonObject().
        json:Add("name",pMethod:name).
        if type-of(pMethod,IFunction) and cast(pMethod,IFunction):isPrimitive then 
        do:
            json:Add("type","primitive").
            cFuncType = PrimitiveFunctionMap:Get(pMethod:name).
            json:Add("datatype",lc(entry(1,cFuncType," "))). 
            json:Add("isArray",cFuncType matches " extent").  // hack 
            
        end. 
        else
            json:Add("type",if pMethod:SingleObject then "object" else "collection").
        
        if type-of(pMethod,Ifunction) then 
        
            json:Add("binding",cast(pMethod,IFunction):Binding:ToString()).
        
        jsonSignatures = GetSignatures(pMethod,pcPath,phBuffer).
         
        if valid-object(jsonSignatures) then
        do:
           json:add("signatures",jsonSignatures). 
        end.
        
        if type-of(pMethod,IAction) then
        do:
            AddActionProperties(phBuffer,cast(pMethod,IAction),json).
        end.   
        return json.
    end.
    
    // allow override by subclass
    method protected void AddActionProperties(phBuffer as handle, pAction as IAction, pJson as JsonObject):
        define variable cFieldList as character no-undo.
        cFieldList = pAction:CompareFields.
        if cFieldList > "" and valid-handle(phBuffer) then
            cFieldList = GetPropertyNames(phBuffer,cFieldList).
        else 
            cFieldList = "*".
        
        pjson:Add("noContentResponse",pAction:NoContentResponse).   
        pjson:Add("requiredProperties",cFieldList). 
    end method.    
    
    method protected JsonArray GetSignatures(pMethod as IMethod,pcPath as character, phBuffer as handle):
        return pMethod:DescribeSignatures(GetFullPath(pcPath),phbuffer).
    end method.
    
    method protected JsonObject GetProperties (phBuffer as handle, pEntity as IBusinessEntity):
        define variable tableJson    as JsonObject no-undo.
        define variable propsJson    as JsonObject  no-undo.
        define variable fieldJson    as JsonObject no-undo.
        define variable extentJson   as JsonObject no-undo.
        define variable idJson       as JsonObject no-undo.
        define variable cObjectType  as character  no-undo.
        define variable cSchemaXML   as longchar   no-undo.
        define variable hField       as handle     no-undo.
        define variable i            as integer    no-undo.
        define variable iError       as integer no-undo.
        empty temp-table ttField.
        
        propsJson = new JsonObject().
         /* Add fields to the table object  */
        do i = 1 to phBuffer:num-fields:
            hField = phBuffer:buffer-field(i).
            if hField:serialize-hidden = false then
            do:
                create ttField.
                assign 
                    ttField.name = hfield:name
                    ttfield.fieldhandle = hField
                    . 
            end.    
        end.
        for each ttField by ttField.name :        
            hField = ttfield.fieldhandle.
            fieldJson = new JsonObject().
            if hField:extent gt 0 then do:
                fieldJson:Add("type", "array").
                fieldJson:Add("maxItems", hField:extent).
            end. /* Extent */
            else do:
                fieldJson:Add("type", "object").
            end. /* Normal */
            
            fieldJson:Add("datatype", getJsonType(hField:data-type)).
            if hField:default-value <> ? and not hField:data-type begins "Progress.Lang" then 
                fieldJson:Add("default", hField:default-value).   
            do on error undo, throw:         
                propsJson:Add(hField:serialize-name, fieldJson).
                catch e as Progress.Lang.Error :
                   iError = iError + 1.
                   if e:GetMessageNum(1) = 16056 then
                       undo, throw new IllegalStateError(subst("&1 has duplicate fields with name &2",phbuffer:serialize-name,hField:name),e).
                   else 
                       undo, throw e.    
                end catch.
            end.
            AddFieldProperties(fieldJson,hField).
        end.
        return propsjson.
    end method. /* getTable */
    
    // hook for application subclass override 
    method protected void AddFieldProperties(pFieldJson as JsonObject,phField as handle):
    end method.    
    
    // hook for application subclass override 
    method protected void AddResourceProperties(pResourceJson as JsonObject):
    end method.
    
    // hook for application subclass override 
    method protected void AddContainerProperties(pResourceJson as JsonObject,phDataset as handle):
    end method.
    
    // hook for application subclass override 
    method protected void AddBufferProperties(pResourceJson as JsonObject,phBuffer as handle,pEntity as IBusinessEntity):
    end method.
    
end class.