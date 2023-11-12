 
 /*------------------------------------------------------------------------
    File        : JsonUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 24 11:10:59 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Json.ObjectModel.ObjectModelParser from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.Error.UnprocessableEntityError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Progress.Json.ObjectModel.JsonConstruct from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using OpenEdge.Core.DataTypeEnum from propath.
using OpenEdge.Core.DataTypeHelper from propath.

block-level on error undo, throw.

class Pmfo.Util.JsonUtil: 
    
    method static public JsonObject ReadJsonFile (pcFile as char):
        define variable parser as ObjectModelParser no-undo.
        define variable json as JsonObject no-undo.
        parser = new ObjectModelParser().
        return cast(parser:ParseFile(pcfile),JsonObject).
    end method.
    
/* need to understand codepage to make thi swork */   
/*    method static public JsonObject ReadJsonLongchar (pclong as longchar):*/
/*        define variable parser as ObjectModelParser no-undo.              */
/*        define variable json as JsonObject no-undo.                       */
/*        parser = new ObjectModelParser().                                 */
/*        return cast(parser:Parse(pclong),JsonObject).                     */
/*    end method.                                                           */
    
    method static public JsonObject ReadJson (pcchar as char):
        define variable parser as ObjectModelParser no-undo.
        define variable json as JsonObject no-undo.
        parser = new ObjectModelParser().
        return cast(parser:Parse(pcChar),JsonObject).
    end method.
    
    /* get an array in a json hierarchy of arrays but only if parent array nodes are unique 
       used to get the requested dataset when unique finds are used to find parent(s)
       from a keyed url 
    */ 
    method static public JsonArray GetArrayFromSingleParents(pjson as JsonObject,pcname as char):
        define variable cnames      as character extent no-undo.
        define variable i           as integer   no-undo.
        define variable oJsonArray  as JsonArray no-undo.
        define variable oEmptyArray as JsonArray no-undo.
        define variable oChildArray as JsonArray no-undo.
        
        cnames = pJson:GetNames().
        do i = 1 to extent(cnames):
            if pJson:GetType(cNames[i]) = JsonDataType:ARRAY then
            do:
                oJsonArray = pJson:GetJsonArray(cNames[i]).
                if cNames[i] = pcName then do:
                    return oJsonArray.
                end.    
            end.
        end. 
        do i = 1 to extent(cnames):
            if pJson:GetType(cNames[i]) = JsonDataType:ARRAY then
            do:
                oJsonArray = pJson:GetJsonArray(cNames[i]).    
                if oJsonArray:Length = 1 then   
                do: 
                    ochildarray = GetArrayFromSingleParents(oJsonArray:GetJsonObject(1),pcname).
                    if valid-object(oChildArray) and oChildArray:Length > 0 then
                        return oChildArray.
                end.
            end.
        end. 
        
        return oEmptyArray.
    end.
    
    // normalize an Object serialized from a temp-table  with a JsonObject as field
    method static public JsonObject NormalizeJsonFieldObject(pjson as JsonObject):
        define variable oObjParent as JsonObject no-undo.
        define variable oObjData   as JsonObject no-undo.
        
        oObjParent = pjson:GetJsonObject("Progress.Json.ObjectModel.JsonObject").
        oObjData   = oObjParent:GetJsonObject("ObjData").
        
        return oObjData.
    end method.      
    
    method static public character GetTextPlain(pjson as JsonObject, pcDelim as char):
        define variable cResponse as character no-undo.
        define variable cNames    as character extent no-undo.
        define variable i         as integer no-undo.
        define variable oObject   as JsonObject no-undo.
        define variable cValue    as character no-undo.
        cNames = pJson:GetNames(). 
        do i = 1 to extent(cNames):    
            if   pJson:GetType(cNames[i]) <> JsonDataType:ARRAY
            and  pJson:GetType(cNames[i]) <> JsonDataType:OBJECT then
            do:
                cValue = GetStringValue(pJson, cnames[i]).
                if cValue = ? then cValue = "?".
                cResponse = cResponse
                          + (if cResponse = "" then "" else pcDelim)
                          // the @ naming convention does not fit since we delim may be @
                          // we replace always evenif there is a different delimiter fro consistency
                          + subst("&1=&2",replace(cNames[i],"@","_"),cValue).
            end.      
             
        end. 
        return cResponse.
    end method.     
    
    /* get a single object in a json hierarchy of arrays but only if parent and requested array nodes are unique 
       used to get the requested dataset when unique finds are used to find parent(s)
       from a keyed url 
    */  
    method static public JsonObject GetObjectFromSingleParents(pjson as JsonObject,pcname as char):
        define variable oJsonArray  as JsonArray no-undo.
        define variable oJsonObject  as JsonObject no-undo.
        define variable cnames      as character extent no-undo.
        define variable i           as integer no-undo.
        cnames = pJson:GetNames().
        do i = 1 to extent(cnames):
            if pJson:GetType(cNames[i]) = JsonDataType:OBJECT then
            do:
                oJsonObject = pJson:GetJsonObject(cNames[i]).
                if cNames[i] = pcName then do:
                    return oJsonObject.
                end.    
            end.
            else if pJson:GetType(cNames[i]) = JsonDataType:ARRAY then
            do:
                oJsonArray = pJson:GetJsonArray(cNames[i]).
                if cNames[i] = pcName and oJsonArray:Length = 1 then
                    return oJsonArray:GetJsonObject(1).
            end.
        end. 
        do i = 1 to extent(cnames):
            if pJson:GetType(cNames[i]) = JsonDataType:ARRAY then
            do:
                oJsonArray = pJson:GetJsonArray(cNames[i]).    
                if oJsonArray:Length = 1 then   
                do: 
                    oJsonObject = GetObjectFromSingleParents(oJsonArray:GetJsonObject(1),pcname).
                    if valid-object(oJsonObject) then
                        return oJsonObject.
                end.
            end.
        end. 
        
        oJsonArray  = GetArrayFromSingleParents(pjson,pcname).
        if valid-object(oJsonArray) and oJsonArray:Length = 1 then
            return oJsonArray:GetJsonObject(1).
        return ?. 
    end. 
    
    method static public JsonObject MoveOnlyNodeUp(pJson as JsonObject, pcName as char):
        define variable oJsonArray   as JsonArray no-undo.
        define variable oJsonNode    as JsonObject no-undo.
        define variable laddEmpty    as logical no-undo.
        define variable cDummy       as character init ? no-undo.
        if pJson:Has(pcname) then 
        do:
             if pJson:GetType(pcName) = JsonDataType:ARRAY then
             do:
                 oJsonArray = pJson:GetJsonArray(pcName).
                 if oJsonArray:length > 1 then 
                     undo, throw new IllegalArgumentError(subst("More than one object found in JsonArray &1",pcName)).
                     
                 if oJsonArray:length = 0 then 
                     pjson:Set(pcName,cDummy).
                 else do:
                     if oJsonArray:GetType(1) = JsonDataType:ARRAY then
                         pjson:Set(pcName,oJsonArray:GetJsonArray(1)).
                     else if oJsonArray:GetType(1) = JsonDataType:OBJECT then
                         pjson:Set(pcName,oJsonArray:GetJsonObject(1)).
                     else 
                         undo, throw new NotImplementedError(subst("MoveOnlyNode of type &1",oJsonArray:GetType(1))).
                 end. 
             end.         
        end.
        else
            undo, throw new IllegalArgumentError(subst("Array &1 not found in Json",pcName)).
                   
    end method.
    
    
    /* create a JsonObject with all nodes of the passed json except the selected  */
    method static public JsonObject ExtractTopSiblings(pJson as JsonObject, pSelectedNode as character):
        define variable cNames        as character  no-undo extent.
        define variable iNode         as integer    no-undo.
        define variable oOtherWrapper as JsonObject no-undo.
        
        cNames = pJson:GetNames().
        if extent(cNames) > 1 then
        do:
            oOtherWrapper = new JsonObject().
            do iNode = 1 to extent(cnames):
                if cNames[iNode] <> pSelectedNode then
                do:
                    oOtherWrapper:Add(cNames[iNode],pJson:GetJsonArray(cNames[iNode])). 
                end.
            end.    
        end.
        return oOtherWrapper.
    end.    
    
/*    method static public void ReadJsonChild(phDataset as handle, phBuffer as handle, pJson as JsonConstruct).*/
/*        define variable cNames       as character extent no-undo.                                            */
/*        define variable hBuffer      as handle    no-undo.                                                   */
/*        define variable oJsonArray   as JsonArray no-undo.                                                   */
/*        define variable hDataset     as handle no-undo.                                                      */
/*        define variable oDatasetjson as JsonObject no-undo.                                                  */
/*        define variable oTopJson     as JsonObject no-undo.                                                  */
/*                                                                                                             */
/*        if type-of(pJson,JsonObject) then                                                                    */
/*        do:                                                                                                  */
/*            oJsonArray = new JsonArray().                                                                    */
/*            oJsonArray:Add(cast(pJson,JsonObject)).                                                          */
/*        end.                                                                                                 */
/*        else                                                                                                 */
/*            oJsonArray = cast(pjson,Jsonarray).                                                              */
/*                                                                                                             */
/*        oDatasetJson = new JsonObject().                                                                     */
/*        oTopJson = new JsonObject().                                                                         */
/*        oDatasetjson:Add(phBuffer:serialize-name,oTopJson).                                                  */
/*                                                                                                             */
/*        oTopJson:Add(phBuffer:serialize-name,oJsonArray).                                                    */
/*                                                                                                             */
/*        hDataset = DatasetUtil:CreateDataset(phBuffer).                                                      */
/*        hDataset:serialize-name = phBuffer:serialize-name.                                                   */
/*        //DatasetUtil:SetTrackingChanges(hDataset, true).                                                    */
/*        hDataset:read-json("JsonObject",oDatasetJson,"empty").                                               */
/*        //DatasetUtil:SetTrackingChanges(hDataset, false).                                                   */
/*                                                                                                             */
/*                                                                                                             */
/*    end method.                                                                                              */
    
    method static public void CopyNodes (pFromjson as JsonObject, pToJson as JsonObject):
        CopyNodes(pFromJson,pToJson,"","","").
    end method.
    
    method static public void CopyNodes (pFromjson as JsonObject, pToJson as JsonObject, pcExceptList as char):
        CopyNodes(pFromJson,pToJson,"","",pcExceptList).
    end method.
    
    method static public void CopyNodes (pFromjson as JsonObject, pToJson as JsonObject, pcOldString as char, pcReplaceString as char, pcExceptList as character):
        define variable cNames   as character extent no-undo.
        define variable cNewName as character no-undo.
        define variable i as integer no-undo.
        define variable idummy as integer no-undo init ?.
        define variable lDecimal as logical no-undo.           
        cNames = pFromJson:GetNames().
        do i = 1 to extent(cNames):
            
            if pcExceptList = "" or lookup(cNames[i],pcExceptList) = 0 then
            do: 
                if pcOldString > "" then 
                    cNewName = replace(cNames[i],pcOldString,pcReplaceString). 
                else 
                    cNewName = cNames[i].     
                if pFromJson:GetType(cNames[i]) = JsonDataType:NUMBER then
                do:
                    lDecimal = false.
                    do on error undo, throw:  
                        pToJson:Add(cNewName,pFromJson:GetInt64(cNames[i])).
                        catch e as Progress.Lang.Error :
                            lDecimal = true.
                        end catch.
                    end.
                    if lDecimal then
                        pToJson:Add(cNewName,pFromJson:GetDecimal(cNames[i])).
                end.
                else if pFromJson:GetType(cNames[i]) = JsonDataType:BOOLEAN then
                     pToJson:Add(cNewName,pFromJson:GetLogical(cNames[i])).
                else if pFromJson:GetType(cNames[i]) = JsonDataType:STRING then
                     pToJson:Add(cNewName,pFromJson:Getlongchar(cNames[i])). 
                else if pFromJson:GetType(cNames[i]) = JsonDataType:NULL then 
                do:  
                     pToJson:Add(cNewName,iDummy). 
                end.
                else if pFromJson:GetType(cNames[i]) = JsonDataType:ARRAY then 
                     pToJson:Add(cNewName,pFromJson:GetJsonArray(cNames[i])).
                else if pFromJson:GetType(cNames[i]) = JsonDataType:OBJECT then 
                     pToJson:Add(cNewName,pFromJson:GetJsonObject(cNames[i])).
            end.     
        end.
    end method.    
    
    method public static logical HasBeforeImage(pjson as jsonObject):
        if pjson:Has("prods:hasChanges") then
        do:
            return pjson:GetLogical("prods:hasChanges").
        end.
        else 
            return false.
    end method.
    
    method public static JsonConstruct GetRequestNode(pjson as JsonObject, phBuffer as handle, plThrowiferror as logical):
        define variable cRowName          as character  no-undo.
        
        if pjson:Has(phBuffer:serialize-name) then
        do: 
            if pJson:GetType(phBuffer:serialize-name) = JsonDataType:ARRAY then
                return pJson:GetJsonArray(phBuffer:serialize-name).
            else 
                return pJson:GetJsonObject(phBuffer:serialize-name).
        end.
        else do: // if nothing found on serialize-name use rowname if it is JsonObject 
            cRowName = BufferUtil:GetRowName(phbuffer).
            if pJson:Has(cRowName) and pJson:GetType(cRowName) = JsonDataType:object then
                return pJson:GetJsonObject(cRowName).
        end.
        if plThrowiferror then
            undo, throw new UnprocessableEntityError(subst("The entity body must have a top node named '&1'&2 in order to be a valid request for '&1'",
                                                            phBuffer:serialize-name,
                                                            if cRowName > "" then subst(" or '&1'",cRowName) else ""
                                                           )
                                                     ).
        return ?.                                                 
    end method.  
        
    // wrap a json request for read-json 
    method public static JsonObject WrapRequestForDataset(pjson as jsonObject, phDataset as handle, pRequestedbuffer as character):
        define variable oJsonDataset     as JsonObject    no-undo.
        define variable oJsonTables      as JsonObject    no-undo.
        define variable oJsonArray       as JsonArray     no-undo.
        define variable hBuffer          as handle        no-undo.
        define variable lHasBeforeImage  as logical       no-undo.
        define variable oJsonBefore      as JsonObject    no-undo.
        define variable oJsonNewBefore   as JsonObject    no-undo.
        define variable cNames           as character     no-undo extent.
        define variable iBefore          as integer       no-undo.
        define variable lFound           as logical       no-undo.
        define variable oJsonRequest     as JsonConstruct no-undo.
        define variable lAddedHasChanges as logical       no-undo.
        define variable cRowName         as character     no-undo.
        
        oJsonDataset = new jsonObject().
        
        lHasBeforeImage = HasBeforeImage(pjson).
        
        if pRequestedBuffer > "" then
        do:
            oJsonTables = new jsonObject().
             
            hBuffer = phDataset:get-buffer-handle(pRequestedBuffer).
            
            // if delete then the top node may not exist with before image so only throw error if no before image
            oJsonRequest = GetRequestNode(pJson,hBuffer,not lHasBeforeImage).
            if valid-object(oJsonRequest) then
            do:
                if lHasBeforeImage then
                do:
                    oJsonTables:Add("prods:hasChanges",pJson:GetLogical("prods:hasChanges")).
                    lAddedHasChanges = true.
                    lFound = true.
                end.   
                else 
                    lFound = true.
                
                if type-of(oJsonRequest,JsonArray) then
                    oJsonTables:Add(hBuffer:serialize-name,cast(oJsonRequest,JsonArray)).
                else do:
                    oJsonArray = new JsonArray().
                    oJsonArray:Add(cast(oJsonRequest,JsonObject)).
                    oJsonTables:Add(hBuffer:serialize-name,oJsonArray).
                end.
            end. 
            if lHasBeforeImage then
            do:
                if not lAddedHasChanges then
                do:
                    oJsonTables:Add("prods:hasChanges",pJson:GetLogical("prods:hasChanges")).
                end.   
                
                oJsonBefore = pJson:GetJsonObject("prods:before") no-error.
              //  if not valid-object(oJsonBefore) then
                //    undo, throw new UnprocessableEntityError("The entity body must have a 'prods:before' node with before-image data of the request").
               // to preserver order we copy all to the new 
               // oJsonBefore = cast(oJsonBefore:Clone(),JsonObject).
                
                //  if create there may not be a json before 
                if valid-object(oJsonBefore) then
                do:
                    cNames = oJsonBefore:GetNames().
                    
                    oJsonNewBefore = new JsonObject().
                    oJsonTables:Add("prods:before",oJsonNewBefore).
                    // check if the main requested node is present and convert it to array if it is an object 
                    do iBefore = 1 to extent(cNames):
                        if oJsonBefore:GetType(cNames[iBefore]) = JsonDataType:OBJECT then
                        do:
                            oJsonArray = new JsonArray().
                            if cNames[iBefore] = hBuffer:serialize-name or cNames[iBefore] = BufferUtil:GetRowName(hBuffer) then
                            do:
                                oJsonArray:Add(oJsonBefore:GetJsonObject(cNames[iBefore])).
                                oJsonNewBefore:Add(hBuffer:serialize-name,oJsonArray).
                                lFound = true.
                            end.   
                            else do:
                                oJsonArray:Add(cast(oJsonBefore:GetJsonObject(cNames[iBefore]):Clone(),JsonObject)).
                                oJsonNewBefore:Add(cNames[iBefore],oJsonArray).
                               //  oJsonBefore:remove(cNames[iBefore]).
                            end.    
                           
                        end.
                        else if oJsonBefore:GetType(cNames[iBefore]) = JsonDataType:ARRAY then
                        do:
                            if cnames[iBefore] = hBuffer:serialize-name then 
                                lFound = true.                                                                                    
                            oJsonNewBefore:Add(cNames[iBefore],oJsonBefore:GetJsonArray(cNames[iBefore])).
                        end.     
                    end.
                end.
                if not lFound then 
                    undo, throw new UnprocessableEntityError(subst("The entity body must have a top node and/or before-image node named '&1' or '&2' in order to be a valid update request for '&1'",
                                                                    hBuffer:serialize-name,
                                                                    BufferUtil:GetRowName(hBuffer) 
                                                                   )
                                                             ).
                                                             
            end. // if lhasbeforeimage
            else if not lFound then
            do:
                undo, throw new UnprocessableEntityError(subst("The entity body must have a top node named '&1' or '&2' in order to be a valid update request for '&1'",
                                                               hBuffer:serialize-name,
                                                               BufferUtil:GetRowName(hBuffer) 
                                                               )
                                                        ).
            end.
            oJsonDataset:Add(phDataset:serialize-name,oJsonTables).   
        end.
        else do:
            if lHasBeforeImage then
                undo, throw new NotImplementedError("Support for update request with multiple top level collections or objects").
            
            oJsonDataset:Add(phDataset:serialize-name,pJson).
        end.
        return ojsonDataset. 
    end method.
    
    method public static character GetStringValue (pJson as JsonObject,pName as char):
        define variable cNum as character no-undo.
        define variable cValue as character no-undo.
        if pJson:GetType(pName) = JsonDataType:NUMBER then
        do on error undo, throw:
            return string(pjson:GetInt64(pName)).
            catch e as Progress.Lang.Error :
                if e:GetMessageNum(1) = 16061 then
                    return string(pjson:GetDecimal(pName)).     
            end catch.
           // return string(pjson:GetDecimal(pName)).
        end.
        else if pJson:GetType(pName) = JsonDataType:BOOLEAN then
        do:
            return string(pjson:GetLogical(pName),"true/false").
        end.
        else if pJson:GetType(pName) = JsonDataType:STRING then
            return pjson:GetCharacter(pName). 
        else if pJson:GetType(pName) = JsonDataType:NULL then
            return ?.  
        else  
            undo, throw new IllegalArgumentError(subst("&1 is not a primitive node in the JSON",pname)).   
    end method.
    
     method public static character GetStringValue (pJson as JsonArray,piNum as integer):
         define variable ojson as JsonObject no-undo. 
         if pJson:GetType(pinum) = JsonDataType:NUMBER then
         do on error undo, throw:
             return string(pjson:GetInt64(piNum)).
             catch e as Progress.Lang.Error :
                if e:GetMessageNum(1) = 16061 then
                    return string(pjson:GetDecimal(piNum)).     
            end catch.
         end.
         else if pJson:GetType(pinum) = JsonDataType:BOOLEAN then
             return string(pjson:GetLogical(pinum)).
         else if pJson:GetType(pinum) = JsonDataType:STRING then
             return pjson:GetCharacter(pinum). 
         else if pJson:GetType(pinum) = JsonDataType:NULL then
             return ?.  
         else  
             undo, throw new IllegalArgumentError("The Array is not primitive").   
    end method.
    
    method public static character GetJsonType ( input pABLType as DataTypeEnum):
        define variable cType as character no-undo.
        define variable cMask as character no-undo.
        
        cMask = DataTypeHelper:GetMask(pABLType).
        if (entry(num-entries(string(cMask), ' ':u), cMask, ' ':u) eq 'extent':u) then
        // do not use - bug
        // if DataTypeHelper:IsArray(pABLType) then
        do: 
            cType = DataTypeHelper:GetMask(pAblType).
            cType = entry(1,cType," ").
            cType = GetJsonType(cType).
            return cType + "[]".
        end.
        else do:
           return GetJsonType(pAblType:ToString()).  
        end.    
    end method.    
    
        
    // thse are types used by client in catalog and report UI
    method public static character GetJsonType ( input pcABLType as character ):
        case pcABLType:
            when "DECIMAL" then
                return "number".
            when "INTEGER" or when "INT64" then
                return "integer".
            when "LOGICAL" then
                return "boolean".
            when "DATE" then
                return "date".
            when "DATETIME"  then
                return "date-time". // json schema name has dash
            when "DATETIME-TZ" then
                return "date-time-tz".
            when "ROWID" then
                return "rowid". 
            when "BLOB" then
                return "blob".    
            when "CLOB" then
                return "clob".    
            when "Progress.Lang.Object" then
                return "object".    
            otherwise
                return "string".  
        end case.
    end method. /* getJsonType */
    
    method public static integer GetJsonRowState(pJson as JsonObject):
        define variable cValue as character no-undo.
        if pJson:Has("prods:rowState") then 
            cValue = pJson:GetCharacter("prods:rowState").
        case cValue:
            when "modified" then return row-modified.
            when "created"  then return row-created.
            when "deleted"  then return row-deleted.
        end.
        return 0.      
    end method.    
    
    
/*    method static public character GetKeyValuesFromJsonObject(pJsonObject as JsonObject, pcKeys as char extent,pcDatatypes as char):                */
/*        define variable iField as integer no-undo.                                                                                                  */
/*                                                                                                                                                    */
/*        do iField = 1 to extent(pcKeys):                                                                                                            */
/*                                                                                                                                                    */
/*            if (pcDatatypes[iField] = "character" or pcDatatypes[iField] begins "date" or pcDatatypes[iField] = "longchar") then                    */
/*            do:                                                                                                                                     */
/*                               assign                                                                                                               */
/*                                   lHasSpaceOrPlus = index(cNextValue,StringConstant:SPACE) > 0 or index(cNextValue,"+") > 0                        */
/*                                   cNextValue      = quoter(cNextValue,"'")                                                                         */
/*                                   .                                                                                                                */
/*                           end.                                                                                                                     */
/*                           /* if hasspaceorplus use field name in expression so we can safely decode bad encoders like certain REST testing tools...*/
/*                              some REST tools will encode space as +, but not if there already is a +                                               */
/*                              this is indeterministic since a value with + and no space could origin from both space and +                          */
/*                           */                                                                                                                       */
/*                           if num-entries(cKeys) = 1 and not lHasSpaceOrPlus then                                                                   */
/*                           do:                                                                                                                      */
/*                               cNextRowKey = cNextValue.                                                                                            */
/*                           end.                                                                                                                     */
/*                           else do:                                                                                                                 */
/*                               cNextRowKey = cNextRowKey                                                                                            */
/*                                           + (if cNextRowKey = "" then "" else " and ")                                                             */
/*                                           + subst("&1 eq &2",                                                                                      */
/*                                                   if ExternalPaging                                                                                */
/*                                                   then DataBuffer:buffer-field(cTTField):serialize-name                                            */
/*                                                   else cTtField,                                                                                   */
/*                                                   cNextValue                                                                                       */
/*                                                  ).                                                                                                */
/*                           end.                                                                                                                     */
/*                       end.                                                                                                                         */
/*                   end.                                                                                                                             */
/*    end method.                                                                                                                                     */
/*                                                                                                                                                    */
     
      // A dataset that has grandchildren with same name for different parents will have duplicated 
     // nodes in the before-image section. These cannot be loaded to a prodataset as they all will end up
     // under he first parent.
     // This code does NOT resolve this, but eliminates the duplicates to make it easier to manage 
     // these on client for cases where response:writeBeforeImage
     // As of current the JsonReader assumes that this so done. (When this was written- the reader may possibly 
     // be changed to call this)          
     method static public void MergeDuplicateErrorNodes(pJson as JsonObject, phdataset as handle):
         MergeDuplicateProDatasetNodes(pJson, phdataset, "prods:errors").
     end method.
     
     method static public void MergeDuplicateBeforeImageNodes(pJson as JsonObject, phdataset as handle):
         MergeDuplicateProDatasetNodes(pJson, phdataset, "prods:before").
     end method.
     
     // A dataset that has grandchildren with same name for different parents will have duplicated 
     // nodes in the before-image section. These cannot be loaded to a prodataset as they all will end up
     // under he first parent.
     // This code does NOT resolve this, but eliminates the duplicates to make it easier to manage 
     // these on client for cases where response:writeBeforeImage
     // As of current the JsonReader assumes that this so done. (When this was written- the reader may possibly
     // be changed to call this)          
      
     method static private void MergeDuplicateProDatasetNodes(pJson as JsonObject, phdataset as handle, pcProgressType as character):
         define variable i                        as integer    no-undo.
         define variable cSerializeNames          as character  no-undo.
         define variable cBufferNames             as character  no-undo.
         define variable cDuplicateSerializeNames as character  no-undo.
         define variable hBuffer                  as handle     no-undo.
         define variable oBeforejson              as JsonObject no-undo.
         define variable cDupName                 as character  no-undo.
         define variable oNewBeforeBuffers        as JsonArray  extent no-undo. 
         define variable oFirstBeforeBuffers      as JsonArray  no-undo. 
         define variable iObject                  as integer    no-undo.
         
         do i = 1 to phDataset:num-buffers:
             if phDataset:get-buffer-handle(i):before-buffer:table-handle:has-records then
             do:
                 assign
                     cSerializeNames = cSerializenames 
                                     + (if cSerializeNames = "" then "" else ",")
                                     + phDataset:get-buffer-handle(i):serialize-name
                     cBufferNames    = cBufferNames 
                                     + (if cBufferNames = "" then "" else ",")
                                     + phDataset:get-buffer-handle(i):name
                     .
             end.
         end.
       
         do i = 1 to num-entries(cBufferNames):
             hbuffer = phDataset:get-buffer-handle(entry(i,cBufferNames)). 
             if lookup(hBuffer:serialize-name,cSerializeNames) <> i then 
             do:
                if lookup(hBuffer:serialize-name,cDuplicateSerializeNames) = 0 then 
                     cDuplicateSerializeNames = cDuplicateSerializeNames 
                                     + (if cDuplicateSerializeNames = "" then "" else ",")
                                     + hbuffer:serialize-name
                                     .
             end.
         end.
         if num-entries(cDuplicateSerializeNames) > 0 then
         do:
             oBeforejson = pjson:GetJsonObject(pcProgressType).
             extent(oNewBeforeBuffers) = num-entries(cDuplicateSerializeNames).
             do i = 1 to extent(oNewBeforeBuffers):
                 oNewBeforeBuffers[i] = new JsonArray().
                 cDupName = entry(i,cDuplicateSerializeNames).  
                 do while true:  
                     ofirstBeforeBuffers = ?. // important  not populated if no-error sdupresses the error
                     oFirstBeforeBuffers = oBeforejson:GetJsonArray(cDupName) no-error.
                     if valid-object(oFirstBeforeBuffers) then
                     do:
                         do iObject = 1 to oFirstBeforeBuffers:length:
                             oNewBeforeBuffers[i]:Add(oFirstBeforeBuffers:GetJsonObject(iObject)).
                         end.
                         oBeforejson:remove(cDupname).
                     end.
                     else 
                         leave. 
                 end.
             end.
             
             do i = 1 to extent(oNewBeforeBuffers):
                 oBeforejson:add(entry(i,cDuplicateSerializeNames),oNewBeforeBuffers[i]). 
             end. 
         end.
          
     end.    

    
end class.