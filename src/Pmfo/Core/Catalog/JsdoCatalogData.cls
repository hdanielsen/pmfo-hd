/*------------------------------------------------------------------------
    Purpose     : create catalog from temp-table with classes 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Notes       : purely here for reference
                  The JSDO catalog cannot handle our relationships 
                  out of the box.   
                  It can handle the BEs with a single dataset           
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
 
block-level on error undo, throw.

class Pmfo.Core.Catalog.JsdoCatalogData: 
    
    define temp-table ttResource no-undo
        field Name as char 
        field className as char
        index idxresource as primary unique className.
        
    define public property ServiceName as character no-undo 
        get.
        set. 

    define public property ServiceURI as character no-undo 
        get.
        set. 

    define public property UseRequest as logical initial true no-undo 
        get.
        set. 
     
    define public property Transport as character init "web" no-undo 
        get.
        
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public JsdoCatalogData (pcService as char ):
        super ().
        ServiceName = pcService.
        ServiceURI = subst("/&1/&2",Transport,pcService). /* if using web handler */ 
                   
         
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method public JsonObject getCatalog ( ):
        define variable catalogJson as JsonObject no-undo.
        assign catalogJson = new JsonObject().
        
        catalogJson:Add("version", 1.0). /* not sure */
        catalogJson:Add("lastModified", now).
        catalogJson:Add("services", getServices()).
        return catalogJson.
    end method. /* getCatalog */

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method protected JsonArray getServices ( ):
        
        define variable serviceArr  as JsonArray no-undo.
        define variable serviceJson as JsonObject no-undo.
        define variable settingsJson as JsonObject no-undo.
        
        assign 
            serviceArr = new JsonArray()
            serviceJson = new JsonObject()
            settingsJson = new JsonObject().
        
        serviceJson:Add("name", ServiceName).
        serviceJson:Add("address", ServiceURI).
/*        settingsJson:Add("useRequest", UseRequest).*/
/*        settingsJson:Add("sendOnlyChanges", false).*/
/*        settingsJson:Add("unwrapped", false).      */
/*        settingsJson:Add("useXClientProps", false).*/
        serviceJson:Add("settings", settingsJson).
        serviceJson:Add("resources", getResources()).

        serviceArr:Add(serviceJson).

        return serviceArr.
    end method. /* getServices */
    
     method protected JsonArray getResources ( ):
        define buffer bResource for ttResource.
        define variable resourceArr as JsonArray  no-undo.
        define variable resourceJson as JsonObject  no-undo.
           
        resourceArr = new JsonArray().
        for each bResource :
            resourceJson = getResource(bResource.Name,bResource.classname).
            resourceArr:Add(resourceJson).
        end.
      
        return resourceArr.
    end method. /* getResources */
    
    method protected JsonObject getResource  (pcName as character , className as char):
        define variable resourceJson as JsonObject no-undo.  
        define variable oObject as IBusinessEntity no-undo.
         
        oObject = dynamic-new className().
         
        assign resourceJson = new JsonObject().
                resourceJson:Add("name", pcName).
                resourceJson:Add("path", "/" + pcName).
                resourceJson:Add("autoSave", false).
                /* TODO: support true key - JSDO handles any key */
                resourceJson:Add("idProperty", "id"). /* case sensitive name */
                resourceJson:Add("displayName", pcName).
/*                resourceJson:Add("schema", getSchema(oObject:DatasetHandle)).                       */
/*                resourceJson:Add("relations", getRelations(oObject:DatasetHandle)).                 */
/*                resourceJson:Add("operations", getOperations(className,oObject:DatasetHandle:name)).*/
         return resourceJson.
          
    end method.
     
    method protected JsonArray getRelations ( input phDataset as handle):
        define variable oRelationArr  as JsonArray  no-undo.
        define variable oRelationJson as JsonObject no-undo.
        define variable oFieldArr     as JsonArray  no-undo.
        define variable ofieldJson    as JsonObject no-undo.
        define variable cDatasetNames as character  no-undo.
        define variable cDummy        as character  no-undo.
        define variable iRel          as integer    no-undo.
        define variable iFld          as integer    no-undo.
        define variable iRelCount     as integer    no-undo.
        define variable hRelation     as handle     no-undo.
        define variable cFields       as character  no-undo. 
        
        assign oRelationArr = new JsonArray().
        
        if valid-handle(phDataset) then do:
            assign iRelCount = phDataset:num-relations.    
            do iRel = 1 to iRelCount:
                assign 
                    hRelation = phDataset:get-relation(iRel)
                    cFields   = hRelation:relation-fields
                    oRelationJson = new JsonObject()
                    oFieldArr    = new JsonArray()
                    .

                oRelationJson:Add("relationName", hRelation:name).
                oRelationJson:Add("parentName", hRelation:parent-buffer:name).
                oRelationJson:Add("childName", hRelation:child-buffer:name).
                
                /* probable BUG: should use field  serialize-name */  
                
                do iFld = 1 to num-entries(cFields) by 2:
                    ofieldJson = new JsonObject().
                    ofieldJson:Add("parentFieldName", entry(iFld, cFields)).
                    ofieldJson:Add("childFieldName", entry(iFld + 1, cFields)).
                    oFieldArr:Add(ofieldJson).
                end.
                oRelationJson:Add("relationFields", oFieldArr).
                oRelationArr:Add(oRelationJson).
            end.
        end. /* valid-handle */
      
        return oRelationArr.
    end method. /* getRelations */
     
    method protected JsonObject getSchema ( input phDataset as handle  ):
        define variable schemaJson    as JsonObject no-undo.
        define variable propJson      as JsonObject no-undo.
        define variable i              as integer    no-undo.
        define variable j              as integer no-undo.
        define variable datasetJson    as JsonObject no-undo.
        define variable tableJson      as JsonObject no-undo.
        define variable hBuffer        as handle no-undo.
        
        assign 
            schemaJson = new JsonObject() 
            propJson = new JsonObject().
        
        schemaJson:Add("type", "object").
        schemaJson:Add("additionalProperties", false).
        /* can in theory have multiple datasets */
        do i = 1 to 1:
           datasetJson = new JsonObject().
           datasetJson:Add("type", "object").
           datasetJson:Add("additionalProperties", false).
           tableJson = new JsonObject().
           do j = 1 to phDataset:num-buffers:
               hBuffer = phDataset:get-buffer-handle (j).
               tableJson:Add(hBuffer:serialize-name, getTable(hBuffer)).
           end.
           datasetJson:Add("properties", tableJson).
           propJson:Add(phDataset:name, datasetJson).    
        end.
        
        schemaJson:Add("properties", propJson). 

        return schemaJson.
    end method. /* getSchema */
    
    method protected JsonArray getOperations(className as character,pcDatasetname  as character ):
        
        /* TODO add reflection logic */
        if false then
            return getReflectionOperations(className,pcDatasetname).         
        else
            return getDefaultOperations(className,pcDatasetname).         
           
    end method.
    
    method protected JsonArray getReflectionOperations (pcClassName as character,pcDatasetname  as character):
        undo, throw new AppError("Get Reflection operations not implemented").
    end method.     
    
    method protected JsonArray getDefaultOperations (pcClassName as character,pcDatasetname  as character):
      
        define variable operationArr  as JsonArray         no-undo.
        define variable operationJson as JsonObject        no-undo.
        define variable paramArr      as JsonArray         no-undo.
        define variable ParamJson     as JsonObject        no-undo.
               
        operationArr  = new JsonArray() .   
        assign 
            operationJson = new JsonObject().
            operationJson:Add("path", "?filter=~{filter~}").
            operationJson:Add("type", "read").   
            operationJson:Add("verb", "get").
            operationJson:Add("useBeforeImage", true).
            
            AddProperties(operationJson,pcClassName,"read").  
            
            paramArr = new JsonArray().
            
            ParamJson = new JsonObject().
            ParamJson:Add("name","filter").
            ParamJson:Add("mode", "Input").
            ParamJson:Add("xType", "character").
            ParamJson:Add("type", "QUERY").
            paramArr:Add(ParamJson).
            
            ParamJson = new JsonObject().
            ParamJson:Add("name","numRecs").
            ParamJson:Add("mode", "Output").
            ParamJson:Add("xType", "integer").
            ParamJson:Add("type", "RESPONSE_BODY").
            paramArr:Add(ParamJson).
            
            ParamJson = new JsonObject().
            ParamJson:Add("name",pcDatasetname).
            ParamJson:Add("mode", "Output").
            ParamJson:Add("xType", "dataset").
            ParamJson:Add("type", "RESPONSE_BODY").
            paramArr:Add(ParamJson).
            
            operationJson:Add("params", paramArr).
            operationArr:Add(operationJson).        
    
            paramArr = new JsonArray().
            ParamJson = new JsonObject().
            ParamJson:Add("name", pcDatasetname).
            ParamJson:Add("mode", "InputOutput").
            ParamJson:Add("xType", "dataset").
            ParamJson:Add("type", "REQUEST_BODY,RESPONSE_BODY").
            paramArr:Add(ParamJson).   
        
            operationJson = new JsonObject().
            operationJson:Add("path", "").
            operationJson:Add("type", "create").    
            operationJson:Add("verb", "post").
            operationJson:Add("useBeforeImage", true).

            AddProperties(operationJson,pcClassName,"post").
          
            operationJson:Add("params", paramArr).
            operationArr:Add(operationJson).
            
            operationJson = new JsonObject().
            operationJson:Add("path", "").
            operationJson:Add("type", "update").    
            operationJson:Add("verb", "put").
            operationJson:Add("useBeforeImage", true).
            AddProperties(operationJson,pcClassName,"update").
          
            operationJson:Add("params", paramArr).
            operationArr:Add(operationJson).            
                        
            operationJson = new JsonObject().
            operationJson:Add("path", "").
            operationJson:Add("type", "delete").    
            operationJson:Add("verb", "delete").
            operationJson:Add("params", paramArr).
            operationJson:Add("useBeforeImage", true).
            AddProperties(operationJson,pcClassName,"delete").
           
            operationArr:Add(operationJson).            
        return operationArr.
    end method. /* getOperations */
    
    method protected JsonObject getTable (input phBuffer  as handle):
        define variable tableJson    as JsonObject no-undo.
        define variable itemsJson    as JsonObject no-undo.
        define variable propsJson    as JsonObject no-undo.
        define variable fieldJson    as JsonObject no-undo.
        define variable extentJson   as JsonObject no-undo.
        define variable idJson       as JsonObject no-undo.
        define variable cObjectType  as character  no-undo.
        define variable cSchemaXML   as longchar   no-undo.
        define variable hField       as handle     no-undo.
        define variable i            as integer    no-undo.

        tableJson = new JsonObject().
        tableJson:Add("type", "array").
        itemsJson = new JsonObject().
        itemsJson:Add("additionalProperties", false).
        idJson = new JsonObject().
        idJson:Add("type", "string").
        propsJson = new JsonObject().
        propsJson:Add("_id", idJson).
        propsJson:Add("_errorString", idJson).
         /* Add fields to the table object  */
        do i = 1 to phBuffer:num-fields:
            hField = phBuffer:buffer-field(i).
            fieldJson = new JsonObject().
            if hField:extent gt 0 then do:
                fieldJson:Add("type", "array").
                fieldJson:Add("ablType", caps(hField:data-type)).
                fieldJson:Add("title", hField:label).
                fieldJson:Add("maxItems", hField:extent).
                extentJson = new JsonObject().
                extentJson:Add("type", getJsonType(hField:data-type)).
                fieldJson:Add("items", extentJson).
            end. /* Extent */
            else do:
                fieldJson:Add("type", getJsonType(hField:data-type)).
                fieldJson:Add("ablType", caps(hField:data-type)).
                fieldJson:Add("default", hField:default-value).
                fieldJson:Add("title", hField:label).
            end. /* Normal */

            if hField:data-type begins "date" then
                fieldJson:Add("format", lc(replace(hField:data-type, "-", ""))).

            propsJson:Add(hField:serialize-name, fieldJson).
             
        end.
        itemsJson:Add("properties", propsJson).
        tableJson:Add("items", itemsJson).
       
        return tableJson.
    end method. /* getTable */
    
    method protected character getJsonType ( input pcABLType as character ):
        case pcABLType:
            when "DECIMAL" then
                return "number".
            when "INTEGER" then
                return "integer".
            when "LOGICAL" then
                return "boolean".
            otherwise
                return "string". /* some bug with dates  */     
        end case.
    end method. /* getJsonType */
    
    method protected void AddProperties(poNode as JsonObject, className as char, pcOperationType as char) :
        case pcoperationType :
/*            when "" then                         */
/*              ponode:Add("somename","somevalue").*/
        end.    
        
    end.
    
end class.