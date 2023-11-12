 
 /*------------------------------------------------------------------------
    File        : JsonReader
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Aug 05 16:54:41 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.Map from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.DataAdmin.IField from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using Pmfo.Core.BusinessLogic.BeforeImageReadEnum from propath.
using Pmfo.Core.BusinessLogic.IJsonExtendedReadHandler from propath.
using Pmfo.Core.BusinessLogic.IJsonReadHandler from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.UnprocessableEntityError from propath.
using Pmfo.Core.Error.ValidationError from propath.
using Pmfo.Core.Manager.IIdentityManager from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.JsonUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Util.TableUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonConstruct from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Enum from propath.
using Progress.Lang.Error from propath.
using OpenEdge.Core.ByteBucket from propath.
using Pmfo.Core.Common.WarningMessages from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Util.UnconvertableAsciiCharacterUtil from propath.
 
class Pmfo.Util.JsonReader: 
    
    define public property WarningMessages as WarningMessages no-undo get. set .
     
    define protected property IdentityManager as IIdentityManager  no-undo 
        get(): 
            return cast(Application:StartupManager:getManager(get-class(IIdentityManager)),IIdentityManager).
        end.
    define public property BeforeImageJson as JsonObject no-undo get. set.   
    
    define temp-table ttBeforeSource  no-undo
        field name   as character
        field handle as handle
        index name as primary unique name.
    
    define temp-table ttBeforeJson no-undo
        field handle as handle
        field key    as character
        field json   as Progress.Lang.Object
        index name as primary unique key.
    
    // enforce unique prods.id
    define temp-table ttAfterJson no-undo 
        field key    as character
        index name as primary unique key.
   
    define protected property JsonReadHandler as IJsonReadHandler  no-undo get. set. 
    define protected property BeforeImageReadOption as BeforeImageReadEnum  no-undo get. set.
    
    define private variable HasNextId as logical no-undo.
    
    // @TODO Deprecate or improve - Recent BEs implement IJsonRadHandler or use a reader provided as IJsonReadHandlerProvider for this  
    define protected property NextIdMap as IMap no-undo 
        get().
           if not valid-object(NextIdMap) then
              NextIdMap = new Map().
           return NextIdMap.      
        end.
        set.
    
    constructor public JsonReader ():
        super ().
    end constructor.
        
    // the handler is optional so null is currently allowed    
    constructor public JsonReader (phandler as IJsonReadHandler  ):
        super ().
        JsonReadHandler = pHandler.
    end constructor.
    
    // the handler is optional so null is currently allowed    
    constructor public JsonReader (phandler as IJsonReadHandler,  pNextidMap as IMap ):
        this-object (pHandler).
        NextIdMap = pNextIdMap.
        HasNextId = true.
    end constructor.
    
    method protected void AssertRowState(pRowstate as RowStateEnum):
        Assert:NotNull(pRowState,"RowState").
        if RowStateEnum:None:Equals(pRowState) or RowStateEnum:CreatedOrModified:Equals(pRowState) then
            undo, throw new NotImplementedError(subst("Support for load of row state '&1'",pRowState )).   
        
    end method.    
    
    method protected void AddWarning(pcEntity as character, pcMessage as character):
        if not valid-object(WarningMessages) then  
            WarningMessages = new WarningMessages().  
       
        WarningMessages:Add(pcEntity, pcMessage).
        
    end method.    
    
    method public void ImportJson(pRequestedHandle as handle, plReadChildren as logical, pJson as JsonObject, pState as RowStateEnum, pBeforeImageReadOption as BeforeImageReadEnum):
        define variable lBeforeImage as logical no-undo.
        define variable hDataset as handle no-undo.
        if not valid-handle(pRequestedHandle) then
            undo, throw new IllegalArgumentError("Invalid request handle"). 
        
        BeforeImageReadOption = pBeforeImageReadOption.
        
        lBeforeImage = JsonUtil:HasBeforeImage(pjson). 
/*        if lBeforeImage and RowStateEnum:Modified:Equals(pState) = false then                                                        */
/*            undo, throw new UnprocessableEntityError(subst("Before image data is not aiiowed for row state '&1'",pState:toString())).*/
/*                                                                                                                                     */
        if lBeforeImage then 
        do:
            if not valid-object(pBeforeImageReadOption) then 
               undo, throw new IllegalArgumentError("BeforeImageReadOption cannot be null if the json has before image data"). 
            
            // create only may not have before imagw
            if pJson:Has("prods:before") then
                BeforeImageJson = pJson:GetJsonObject("prods:before").
            else //hack  - check for valid object isa used to identify has before image   
                BeforeImageJson = new JsonObject().   
        end.
        if pRequestedHandle:type = "dataset" then 
        do: 
            if plReadChildren then
                undo, throw new IllegalArgumentError("Read children true for dataset read"). 
                
            ImportDatasetFromJson(pRequestedHandle,pJson,pState).
        end.
        else if pRequestedHandle:type = "buffer" then
        do:
            ImportBufferFromJson(pRequestedHandle,plReadChildren, pJson,pState).
        end.
        else 
            undo, throw new IllegalArgumentError(subst("Invalid request type &1. Supported types are BUFFER or DATASET",pRequestedHandle:type)). 
    end method.
    
    method protected void ImportBufferFromJson(pRequestedBuffer as handle, plReadChildren as logical, pJson as JsonObject, pState as RowStateEnum):
        define variable oJsonRequest as JsonConstruct no-undo.
        define variable hRelation    as handle no-undo.
        define variable lReadOnly    as logical no-undo.
        if not valid-handle(pRequestedBuffer) then
            undo, throw new IllegalArgumentError("Invalid requested buffer handle"). 
            
        AssertRowState(pstate).   
      
        oJsonRequest = JsonUtil:GetRequestNode(pJson,pRequestedBuffer,true).
     
        if plReadChildren  then 
        do:
            if not pRequestedBuffer:avail then 
               undo, throw new IllegalStateError(subst("Buffer &1 is not available for JsonReader read Children option",pRequestedBuffer:name)). 
            if valid-object(JsonReadHandler) then
                lReadOnly = JsonReadHandler:CheckReadOnlyAndPrepareBuffer(pRequestedBuffer,pState).
            if lReadOnly then    
                undo, throw new IllegalStateError(subst("Buffer &1 cannot be set to read-only in JsonReader when it was created from URL key",pRequestedBuffer:name)). 
             
            ReadChildren(pRequestedBuffer, cast(oJsonRequest,JsonObject), pState, "").
        
        end.
        else do:   
            hRelation = pRequestedBuffer:parent-relation.
             
            if valid-handle(hRelation) then
                ReadChild(hRelation,GetJsonArray(oJsonRequest),pState,""). 
            else 
                ReadTable(pRequestedBuffer,GetJsonArray(oJsonRequest),pState).
            
        end. 
    end method.    
    
    method protected void ImportDatasetFromJson(phDataset as handle, pJson as JsonObject, pState as RowStateEnum):
        define variable hBuffer          as handle        no-undo.
        define variable oRequestNode     as JsonArray no-undo.
        define variable iNumTopBuffers   as integer no-undo.
        define variable i                as integer no-undo.
        define variable lAny             as logical no-undo.
        AssertRowState(pstate).  
        
        iNumTopBuffers = DatasetUtil:GetNumTopBuffersExcludeRepos(phDataset).
         
        do i = 1 to iNumTopBuffers:  
            hBuffer = DatasetUtil:GetTopBufferExcludeRepos(phDataset, i).
            if iNumTopBuffers = 1 or pJson:Has(hBuffer:serialize-name) then 
            do:
                oRequestNode = GetJsonArray(pJson,hBuffer:serialize-name). 
                ReadTable(hBuffer,oRequestNode,pState).
                lAny = true.
            end.    
        end.
        if lany = false then 
            undo, throw new UnprocessableEntityError("None of the expected entities are present in the submitted JSON object").    
             
        catch e as Progress.Lang.Error :
            if e:GetMessageNum(1) = 16058 then
                undo, throw new NotFoundError(hBuffer:serialize-name,phDataset:serialize-name,e).    
            undo, throw e.           
        end catch.
    end method.    
    
    method protected void LoadBeforeImageData(phhandle as handle):
        define variable oJson      as JsonObject no-undo.
        define variable oJsonArray as JsonArray no-undo.
        define variable i as integer no-undo.
        if BeforeImageJson:Has(phhandle:serialize-name) then
        do:
            oJsonArray = GetJsonArray(BeforeImageJson, phhandle:serialize-name).
            do i = 1 to oJsonArray:length:
                oJson = oJsonarray:GetJsonObject(i).
                if ojson:Has("prods:rowState") then
                do:
                    if JsonUtil:GetJsonRowState(ojson) = 0 then 
                       undo, throw new UnprocessableEntityError(subst('Before image row for &1 prods:rowState "&2" is not valid',
                                                                       phHandle:serialize-name,
                                                                       ojson:GetCharacter("prods:rowState")
                                                                       )).
        
                end.
                else    
                    undo, throw new UnprocessableEntityError(subst("Before image row for &1 is missing prods:rowState",phHandle:serialize-name)).
                
                if ojson:Has("prods:id") then
                do on error undo, throw:
                    create ttBeforeJson.
                    
                    assign
                        ttBeforeJson.handle = phhandle
                        ttBeforeJson.json   = oJson
                        ttBeforeJson.key    = oJson:GetCharacter("prods:id")
                        .
       
                   validate ttBeforeJson.    
                   catch e as Progress.Lang.Error :
                       delete ttBeforeJson.  
                       if e:GetMessageNum(1) = 132 then
                          undo, throw new UnprocessableEntityError(subst('Before image row for &1 prods:id value "&2" is a duplicate of the prods:id of another before image row',
                                                                     phHandle:serialize-name,
                                                                     oJson:GetCharacter("prods:id")
                                                                     )
                                                                ).
                       else 
                           undo, throw e.
                                                                
                   end catch.  
                    
                end.
                else 
                    undo, throw new UnprocessableEntityError(subst("Before image row for &1 is missing prods:id",phHandle:serialize-name)).
            end.
        end.    
    end method.    
    
    method protected void LoadBeforeImageDeletesFromJson(phbuffer as handle, phParentFields as handle extent, pcChildFields as character extent ): 
        define variable oJsonArray as JsonArray no-undo. 
        define variable iObject    as integer no-undo.
        define variable iFld       as integer no-undo.
        define variable hFld       as handle no-undo. 
        define variable oJson      as JsonObject no-undo.
        define variable iParentFld as integer no-undo.
        define variable lCompare   as logical no-undo.
         
        if BeforeImageJson:Has(phbuffer:serialize-name) then
        do:
            oJsonArray = BeforeImageJson:GetJsonArray(phbuffer:serialize-name).
            do iObject = 1 to oJsonArray:length:
                oJson = oJsonArray:GetJsonObject(iObject).
                if GetJsonRowState(oJson) = row-deleted then
                do:
                    lCompare = extent(phParentFields) <> ?.
                    do iParentFld = 1 to extent(phParentFields):
                        if not CompareField(oJson, phParentFields[iParentFld],phBuffer:buffer-field(pcChildFields[iParentFld]):serialize-name," before image") then
                        do:
                            lCompare = false.
                            leave. 
                        end.   
                    end.    
                    if lcompare then 
                    do:              
                        phbuffer:buffer-create().
                        do iFld = 1 to phBuffer:num-fields on error undo, throw :
                            hFld = phBuffer:buffer-field (iFld).
                            if ojson:Has(hFld:serialize-name) then  
                                AssignField(oJson, hfld).
                        end.
                        phbuffer:table-handle:tracking-changes = true.
                        phBuffer:buffer-delete().
                        phbuffer:table-handle:tracking-changes = false.
                    end.           
                end.    
            end.
            // this fixes an issue where deletes are not serialized 
            // in the encountered case there was only one delete created 
            // there before image apparently got available in the delete 
            // but there is no before-image avail when this method is called \
            if phBuffer:before-buffer:avail then 
                phBuffer:before-buffer:buffer-release.
         end.
                                     
    end method.
    
    // pistate = ? - row state is unchecked
    method protected void LoadBeforeImageFromJson(phbuffer as handle, pjson as JsonObject, pistate as integer): 
        define variable iField        as integer    no-undo.
        define variable cJsonKey      as character  no-undo.
        define variable hBeforeField  as handle     no-undo.
        define variable hAfterField   as handle     no-undo.
        define variable cKey          as character  no-undo.
        define variable oBeforeJson   as JsonObject no-undo.
        define variable hBeforeSource as handle     no-undo.
        define variable iJsonState    as integer    no-undo.
        define variable cNames        as character  no-undo extent.
        define variable iName         as integer    no-undo.
        define variable iType         as integer no-undo.
        define variable cBadBeforeJsonTmpl as character no-undo
            init 'JSON &1 "&2" under the "&3" array is not valid. The arrays in the "prods:before" section cannot have child &1s.'.
            
       
        find ttBeforeSource where ttBeforeSource.name = phbuffer:name no-error.
        if not avail ttBeforeSource then
        do:
            create ttBeforeSource.
            assign
                ttBeforeSource.name   = phBuffer:name
                ttBeforeSource.handle = TableUtil:CreateTableLike(phBuffer)
                .
            LoadBeforeImageData(ttBeforeSource.handle).
          
        end.   
        if pJson:Has("prods:id") then
        do:
            if pistate = ? then 
            do: 
                if pjson:Has("prods:rowState") then
                do:
                    iJsonState = JsonUtil:GetJsonRowState(pjson).
                    if iJsonState = 0 then 
                    do: 
                        undo, throw new UnprocessableEntityError(subst('Row for &1 prods:rowState "&2" is not supported valid',
                                                                       phbuffer:serialize-name,
                                                                       pjson:GetCharacter("prods:rowState")
                                                                       )).
        
                    end.
                    else if iJsonState <> row-modified then 
                        undo, throw new IllegalStateError(subst('LoadBeforeImageFromJson called fpr json node with prods:rowState "&1"',BufferUtil:GetRowStateStringPast(iJsonState))) .
                    
                end.      
                else    
                    undo, throw new UnprocessableEntityError(subst("Before image row for &1 is missing prods:rowState",phbuffer:serialize-name)).
         
            end. 
            cKey = pJson:GetCharacter("prods:id").
            do on error undo, throw:
                create ttAfterJson.
                ttAfterJson.key = cKey.
                validate ttAfterJson.
                catch e as Progress.Lang.Error :
                    delete ttAfterJson.  
                    if e:GetMessageNum(1) = 132 then
                        undo, throw new UnprocessableEntityError(subst('Row for &1 prods:id value "&2" is a duplicate of the prods:id of another row',
                                                                  phBuffer:serialize-name,
                                                                 cKey
                                                                 )
                                                            ).
                   else 
                        undo, throw e.
                                                            
               end catch.  
            end.
            
            hBeforeSource = ttBeforeSource.handle:default-buffer-handle.
            hBeforeSource:buffer-create.
            
            find ttBeforeJson where ttBeforeJson.handle = ttBeforeSource.handle
                                and ttBeforejson.key     = cKey no-error.
            if avail ttBeforejson then                     
                oBeforeJson = cast(ttBeforeJson.json,JsonObject).           
            else 
                undo, throw new UnprocessableEntityError(subst('Row with key &1 and prods:Id "&2" is missing from "&3" before image',
                                                                ServiceInfo:GetLabeledKey(phBuffer),
                                                                cKey,
                                                                phbuffer:serialize-name)
                                                                ).
            
            do ifield = 1 to hBeforeSource:num-fields:         
                hBeforeField = hBeforeSource:buffer-field(iField).
                if oBeforejson:has(hBeforeField:serialize-name) then
                do:
                    AssignField(oBeforejson,hBeforeField). 
                    if not pjson:has(hBeforeField:serialize-name) then 
                    do:
                        // The BeforeImageReadEnum:AssignAfterFromBeforeImage seems superfluous 
                        // as for now (11/24/2022) make it default for child data 
                        // we need some logic to enforce that all fields are present in unmodified parents
                        // before this can be removed completelt       
                        if valid-handle(phBuffer:parent-relation) 
                        or BeforeImageReadEnum:AssignAfterFromBeforeImage:Equals(BeforeImageReadOption) then 
                        do:
                            hAfterField = phBuffer:buffer-field(hBeforeField:name).
                            
                            // if after is default (can have been assigned by JsonReadHandler HandleFieldNotInJson  
                            if hAfterField:default-value = hAfterField:buffer-value 
                            and hBeforeField:buffer-value <> hAfterField:buffer-value then
                            do: 
                                hAfterField:buffer-value = hBeforeField:buffer-value.
                        
                            end.
                        end.    
                    end.         
                end.
                else do:
                    if valid-handle(phBuffer:parent-relation) then 
                    do:
                        AssignBeforeValueFromParent(phBuffer:parent-relation,hBeforeField).
                    end.
                end.     
            end.
            phBuffer:mark-row-state (row-modified,hBeforeSource).
             
            hBeforeSource:buffer-delete.
        end.
        catch e2 as UnprocessableEntityError :
            // throw a more explanatory error if the reason for the missing prods error is that the array in the before iamge has children
            if valid-object(oBeforeJson) then
            do:
                cNames = oBeforeJson:GetNames().
                do iname = 1 to extent(cNames):
                    iType =  oBeforeJson:GetType(cNames[iname]).
                    if iType = JsonDataType:ARRAY then 
                        undo, throw new UnprocessableEntityError(subst(cBadBeforeJsonTmpl,
                                                                       "array",
                                                                       cNames[iName],
                                                                       phBuffer:serialize-name
                                                                      ) 
                                                                ).
                    else if iType = JsonDataType:OBJECT then 
                        undo, throw new UnprocessableEntityError(subst(cBadBeforeJsonTmpl,
                                                                       "object",
                                                                       cNames[iName],
                                                                       phBuffer:serialize-name
                                                                      ) 
                                                                ).
                       
                end.     
            end.
            undo, throw e2.
          
        end catch.
        
    end method.
    
    method protected void AssignBeforeValueFromParent(phParentRelation as handle,phFld as handle) :
        define variable hParent as handle no-undo.
        define variable cParentField as character no-undo.
        define variable iChildFld as integer no-undo.
        define variable hParentFld as handle no-undo.
         
        do iChildFld = 1 to num-entries(phParentRelation:relation-fields) by 2:
            if entry(iChildFld + 1,phParentRelation:relation-fields) = phFld:name then 
            do:
                cParentField = entry(iChildFld, phParentRelation:relation-fields).
                leave.
            end.     
        end.    
        if cParentField > "" then
        do: 
            hParent = phParentRelation:parent-buffer.
            if valid-handle(hparent:before-buffer) and hparent:before-rowid <> ? then 
            do: 
                hparent:before-buffer:find-by-rowid(hparent:before-rowid).
                hParentFld = hparent:before-buffer:buffer-field(cParentField).   
            end.
            else
                hParentFld = hparent:buffer-field(cParentField).     
            phFld:buffer-value = hParentFld:buffer-value. 
        end.
    end method.
    
    // wrap JsonConstruct as child in array if not array
    method protected JsonArray GetJsonArray(pJson as JsonConstruct):
        define variable oJsonArray as JsonArray no-undo.
        if type-of(pJson,JsonArray) then
            return cast(pJson,JsonArray).
        else do:
            oJsonArray = new JsonArray().
            oJsonArray:Add(cast(pJson,JsonObject)).
            return ojsonArray.
        end.
     
    end method.
    
    method protected JsonArray GetJsonArray(pJson as JsonObject, pSerializeName as character):
        define variable oJsonArray as JsonArray no-undo.
        if pJson:GetType(pSerializeName) = JsonDataType:ARRAY then
            return pJson:GetJsonArray(pSerializeName).
        else do:
            oJsonArray = new JsonArray().
            oJsonArray:Add(pJson:GetJsonObject(pSerializeName)).
            return ojsonArray.
        end.
     
    end method.
    
    method protected void ReadChild(phRelation as handle, pJsonArray as JsonArray,pState as RowStateEnum,pcParentNewKey as character):
        define variable oJsonRow      as JsonObject no-undo.      
        define variable i             as integer    no-undo.
        define variable iFld          as integer    no-undo.
        define variable hParent       as handle     no-undo.
        define variable hChild        as handle     no-undo.
        define variable hParentFields as handle     no-undo extent.
        define variable cChildFields  as character  no-undo extent.
        define variable iCount        as integer no-undo.
        
        hParent = phRelation:parent-buffer.
        
        if not hParent:avail then      
            undo, throw new IllegalArgumentError(subst("Parent '&1' passed to ReadChild is not available",hParent:name)). 
        
        hChild = phRelation:child-buffer. 
        
        extent(hParentFields) = int(num-entries(phRelation:relation-fields) / 2).
        extent(cChildFields)  = extent(hParentFields).
        do iFld = 1 to num-entries(phRelation:relation-fields) by 2:
            iCount = iCount + 1.
            hParentFields[iCount] = hParent:buffer-field(entry(iFld,phRelation:relation-fields)). 
            cChildFields[iCount] = entry(iFld + 1,phRelation:relation-fields).
        end.    
        ReadTable(hChild, pJsonArray,pState, hParent, hParentFields, cChildFields, pcParentNewKey ). 
        
    end method.   
    
    method protected void ReadTable(phBuffer as handle, pJsonArray as JsonArray,pState as RowStateEnum):
        define variable hDummy as handle extent no-undo.
        define variable cDummy as character extent no-undo.
        
        ReadTable(phBuffer,pJsonArray,pState,?,hDummy,cDummy,""). 
    end method.
    
    method protected void ReadTable(phBuffer as handle, pJsonArray as JsonArray, pState as RowStateEnum, phParent as handle,  phParentFields as handle extent, pcChildFields as char extent, pcParentNewKey as char):
        define variable i        as integer no-undo.
        define variable oJsonRow as JsonObject no-undo.    
        define variable iFld     as integer no-undo.
        define variable cKey as character no-undo.
        define variable hChldFld as handle no-undo.
        define variable NextIdEnum as Enum no-undo.
        define variable iForeignKey as int64 no-undo.
        define variable cForeignKey as character no-undo.
        define variable lReadOnlyTable as logical no-undo.
        define variable cExceptFields as character no-undo.
        define variable rDelete as rowid no-undo.
        define variable iJsonRowState as integer init ? no-undo.
        define variable lSkipRow as logical no-undo.
        define variable lChildHasNonEmptyValue as logical.
        define variable cParentConflictMsg as character no-undo.
          
        if HasNextId then
        do:
            NextIdEnum = cast(NextIdMap:Get(new String(phBuffer:table-handle:name)),Enum).
            if valid-object(NextIdEnum) then
            do:
                cKey = ServiceInfo:GetKeys(phBuffer).
                if num-entries(cKey ) > 1 then
                    undo, throw new IllegalOperationError(subst("Cannot use NextIdEnum with entity '&1' that has multiple keys",phBuffer:serialize-name)).
            end.
        end.
        
        if valid-object(BeforeImageJson) and valid-object(phparent) then 
        do:
            LoadBeforeImageDeletesFromJson(phBuffer,phParentFields,pcChildFields).
        end.
        
        if pJsonArray:length > 0 then
        do:
            if valid-object(JsonReadHandler) then
            do:              
                lReadOnlyTable = CheckReadOnlyAndPrepareBuffer(phParent,phBuffer,pState).
            end. 
           
            
            /* Transaction scope to ensure row is complete */ 
            rowloop:
            do i = 1 to pJsonArray:length transaction:        
                 
                oJsonRow = pJsonArray:GetJsonObject(i).
                
                phBuffer:buffer-release().
                if valid-object(JsonReadHandler) and type-of(JsonReadHandler,IJsonExtendedReadHandler) then 
                do on error undo, throw:
                    lSkipRow = cast(JsonReadHandler,IJsonExtendedReadHandler):CheckOrSkipRow(oJsonRow, phbuffer, this-object ).
                    if lSkipRow then
                    do: 
                        // relase just to minimize side effects
                        phBuffer:buffer-release().
                        next rowloop.
                    end.   
                    catch e as Progress.Lang.Error :
                       // rethrow as unprocessable to avoid wrapping if nested     
                        undo,throw new UnprocessableEntityError(subst("Failed to read '&1' from json",phBuffer:serialize-name),e).
                    end catch.
                end.    
                // null may happen if posted entity is an object with no value 
                if valid-object(oJsonRow) then
                do on error undo, throw:    
                    phBuffer:buffer-create().
                        
                    cExceptFields = "".
                    rDelete = ?.
                    if valid-handle(phParent) then
                    do iFld = 1 to extent(phParentFields):
                        lChildHasNonEmptyValue = false.
                        hChldFld = phBuffer:buffer-field(pcChildFields[iFld]).
                        if oJsonRow:has(hChldFld:serialize-name) then
                        do: 
                            
                            ReadField(oJsonRow, hChldFld).
                            
                            if hChldFld:buffer-value <> ? then 
                            do:
                                case hChldFld:data-type:
                                    when "integer" or when "decimal" or when "int64"  then 
                                        lChildHasNonEmptyValue = hChldFld:buffer-value gt 0.
                                    when "character" or when "longchar" then 
                                        lChildHasNonEmptyValue = hChldFld:buffer-value gt "".
                                    otherwise    
                                        lChildHasNonEmptyValue = true.
                                end.
                            end.
                            // NOTE - the order of the 3 checks below is due to baclwards copmprabilit. 
                            //        the first and second was present before the third general check was added
                            //        There is lots of overlap between first and third, but they will likely always have sime differnces   
                            if pcParentNewKey > "" and phParentFields[iFld]:name = pcParentNewKey then
                            do: 
                                // we will likely always support empty child values for this create case   
                                if lChildHasNonEmptyValue then
                                    undo, throw new UnprocessableEntityError(subst("Foreign key '&1' in entity '&2' row &3 have a value different from &4 and null. The value should preferable be omitted, but null and empty is also allowed.",
                                                                                   hChldFld:serialize-name,
                                                                                   phBuffer:serialize-name,
                                                                                   i,
                                                                                   if hChldFld:data-type = "int64" or hChldFld:data-type = "integer" or hChldFld:data-type = "decimal" then "0" else "blank"                                                     
                                                                                   )).
                                // assign from parent  
                                hChldFld:buffer-value = phParentFields[iFld]:buffer-value.
                            end.   
                            // added for one to one where the key is on the child 
                            // in most cases these are not exposesd in json at all
                            // but before image with delete we may need the key 
                            else if phParentFields[iFld]:serialize-hidden = true then 
                            do:
                                // don't throw error for lChildHasNonEmptyValue=  assumed to be handled correctly by service later 
                                phParentFields[iFld]:buffer-value = hChldFld:buffer-value.
                            end.    
                            else if hChldFld:buffer-value <> phParentFields[iFld]:buffer-value then 
                            do:
                                cParentConflictMsg = subst("Foreign key '&1' value &2 in entity '&3' &4conflicts with field '&5' value &6 in parent entity '&7'.",                                                                                     
                                                            hChldFld:serialize-name,
                                                            BufferValueAsMessage(hChldFld),                                                     
                                                            phBuffer:serialize-name,
                                                            if pJsonArray:length > 1 then subst("row &1 ",i) else "",
                                                            phParentFields[iFld]:serialize-name,
                                                            BufferValueAsMessage(phParentFields[iFld]),
                                                            phParent:serialize-name                                                     
                                                          ).
                                undo, throw new UnprocessableEntityError(cParentConflictMsg).
                            end.       
                        end. // if oJsonRow:has(hChldFld:serialize-name)      
                        else  
                            hChldFld:buffer-value = phParentFields[iFld]:buffer-value.
                        
                        cExceptFields = cExceptFields 
                                      + (if cExceptFields = "" then "" else ",")
                                      +  hChldFld:name.
                                        
                    end.     
                    
                   // allow lReadonlyTable = ? if CheckReadOnlyAndPrepareBuffer implementaqtion does not return anything 
                    if lReadOnlyTable <> true then 
                    do:
                        if valid-object(BeforeImageJson) = false then
                        do: 
                            case pState: 
                                when RowStateEnum:Created or when RowStateEnum:Action then
                                    phBuffer:mark-row-state (row-created).
                                when RowStateEnum:Modified then
                                do:
                                    if valid-object(BeforeImageJson) then
                                        LoadBeforeImageFromJson(phbuffer, oJsonRow, ?).
                                    else // somewhat point less - no changes so nothing will be saved
                                         // but no harm -    
                                        phBuffer:mark-row-state (row-modified,phbuffer).
                                end.
                                when RowStateEnum:Deleted then
                                do: 
                                    rDelete  = phBuffer:rowid. // we must read the data before we delete it
                                    // the readrow was moved below here so that parents are finalized before loading children
                                end.      
                            end case.
                        end.
                        else do:
                            iJsonRowState = GetJsonRowState(oJsonRow).
                           
                            case iJsonRowState:
                                when row-created then 
                                    phBuffer:mark-row-state (row-created).
                                when row-modified then 
                                    LoadBeforeImageFromJson(phbuffer, oJsonRow, iJsonRowState).
                                 
                            end.     
                        end.
                    end.
                         
                    ReadRow(phBuffer,oJsonRow,pState,NextIdEnum,cKey,cExceptFields).
                    
                    if rDelete <> ? then
                    do:  
                        phBuffer:find-by-rowid (rDelete).  
                        // must use tracking -
                        // mark-row-state does not remove after-buffer.
                        // delete of after-buffer removes everything 
                        phBuffer:table-handle:tracking-changes = true.
                        phBuffer:buffer-delete().
                        phBuffer:table-handle:tracking-changes = false.
                    end.
                    // allow json readers to throw validation errors - the BusinessEntityOperationHandler will call ThrowDatasetError in the BE
                    catch e1 as ValidationError:
                        undo, throw e1.
                    end.    
                    // don't wrap unprocessable - or rethrow as unprocessable to avoid wrapping higher up 
                    catch e2 as ApplicationError :
                        if phBuffer:avail then
                            phBuffer:buffer-delete(). 
                        // don't wrap the error if it has a http status different than 500    
                        if e2:StatusCode <> StatusCodeEnum:InternalServerError then
                            undo, throw e2.
                        else // rethrow as unprocessable to avoid wrapping if nested     
                            undo,throw new UnprocessableEntityError(subst("Failed to read '&1' from json",phBuffer:serialize-name),e2).
                    end catch.
                   
                    catch e3 as Progress.Lang.Error :
                        if phBuffer:avail then
                            phBuffer:buffer-delete(). 
                        if e3:GetMessageNum(1) = 132 then
                        do:
                            // TODO  improve message to show key values ...
                            undo, throw new UnprocessableEntityError(subst("&1 object found with duplicate key values in node &2",phBuffer:serialize-name,i)).     
                        end.
                         
                        undo,throw new ApplicationError(subst("Failed to read '&1' from json",phBuffer:serialize-name),e3).
                    end catch.
                end.
            end. /* do i = 1 to array:length */
         
        end.
        
    end method.
    
    method protected character BufferValueAsMessage(phFld as handle):
        
        if phFld:buffer-value = ? then 
            return "null".
        
        case phFld:data-type:
           when "integer" or when "int64" or when "decimal" then
               return phFld:buffer-value.
           when "character" then 
           do:
              if phFld:buffer-value = "" then 
                  return "blank".
              else    
                  return subst('&1',phFld:buffer-value).
           end.
           otherwise do:
               return subst('&1',phFld:buffer-value).
           end.   
        end case.
    end.      
    
    method protected void ReadRow(phBuffer as handle,pJson as JSONObject, pRowState as RowStateEnum,pNextIdEnum as Enum, pKey as char).
        ReadRow(phBuffer, pJson,pRowState,pNextIdEnum,pkey,"").
    end method.
    
    method protected void ReadRow(phBuffer as handle, pjson as JSONObject, pRowState as RowStateEnum,  pNextIdEnum as Enum, pKey as char, pcKeyColumns as char).
         define variable i as integer no-undo.
         define variable hFld as handle no-undo.
         define variable iKey as int64 no-undo.
         define variable lUseKey as logical no-undo.
        
         do i = 1 to phBuffer:num-fields on error undo, throw :
             hFld = phBuffer:buffer-field (i).
             
             if  hFld:serialize-hidden = false  
             and lookup(hFld:name,pcKeyColumns) = 0 then
             do:
               
                 if pjson:Has(hFld:serialize-name) then 
                 do:
                     if pRowState = RowStateEnum:Created and pKey > "" and hFld:name = pKey and valid-object(pNextIdEnum) then
                     do:
                         if hFld:data-type = "integer" or hFld:data-type = "int64" then
                         do:
                             case hFld:data-type:
                                 when "int64" then
                                    iKey =  pjson:GetInt64(hFld:serialize-name).
                                 when "integer" then
                                    iKey = pjson:GetInteger(hFld:serialize-name).
                             end.            
                             if iKey = 0 or iKey = ? then
                             do:
                                 hFld:buffer-value = IdentityManager:NextId(pNextIdEnum).
                                 lUseKey = true. 
                             end.
                             else do:  
                                 hFld:buffer-value = iKey.
                             end.    
                         end.     
                         else 
                             undo, throw new NotImplementedError(subst("Use of NextIdEnum on Json read for data type '&1' field '&2'",hFld:data-type,hFld:name)).      
                     end.
                     else do:
                         ReadField(pjson,hfld).
                     end.    
                 end.
                 else if valid-object(JsonReadHandler) then
                     JsonReadHandler:HandleFieldNotInJson(pjson,hfld).
             end.
         end.
         phBuffer:buffer-validate(). // ensure calls to JsonReadHandler:CheckReadOnlyAndPrepareBuffer for children see this record in a static by-reference call 
         if not valid-object(BeforeImageJson) or GetJsonRowState(pJson) <> row-deleted then
             ReadChildren(phBuffer,pjson,pRowState,if luseKey then pKey else "").
    end method.    
    
    method protected integer GetJsonRowState(pJson as JsonObject):
        if valid-object(BeforeImageJson) = false then 
            undo, throw new IllegalOperationError("GetJsonRowState called in JSON with no before-image section").
        return JsonUtil:GetJsonRowState(pJson).   
    end method.    
    
    method protected void ReadChildren(phParent as handle,pJson as JSONObject, pRowState as RowStateEnum, pcParentNewKey as char).
        define variable i           as integer    no-undo. 
        define variable hRelation   as handle     no-undo.
        define variable cKeys       as character  no-undo.
        define variable lReadOnly   as logical    no-undo.
        define variable oChildArray as JsonArray  no-undo.
        
        /* loop through children and call ReadChild */
       
        do i = 1 to phParent:num-child-relations:
            hRelation = phParent:get-child-relation[i].
            if hRelation:reposition = false then 
            do: 
              
                if pjson:has(hRelation:child-buffer:serialize-name) then
                do:  
                    // convert to array for one-to-one 
                    // expect null (since one-to-one child may have been sent to client as null it may come back this way)    
                    oChildArray = GetJsonArray(pJson,hRelation:child-buffer:serialize-name).
                    if valid-object(oChildArray) then 
                        ReadChild(hRelation,oChildArray,pRowState,pcParentNewKey).
                end.
                else do:
                    // if the child buffer does not exist, but has the same name read the fields into
                    // since this is (likely??) a flattened many-to-many relation  
                    if RowStateEnum:Created:Equals(pRowState) 
                    and hrelation:parent-buffer:serialize-name = hRelation:child-buffer:serialize-name then
                    do: 
                        if valid-object(JsonReadHandler) then
                            lReadOnly = CheckReadOnlyAndPrepareBuffer(hrelation:parent-buffer,hRelation:child-buffer,pRowState).
                        else
                            lReadOnly = false.
                        hRelation:child-buffer:buffer-create().
                        ReadRow(hRelation:child-buffer, pjson, pRowState, ?, "").
                        if lReadOnly <> true then
                            hRelation:child-buffer:mark-row-state (row-created).
                    end. 
                end.    
            end.   
        end.
    end method.
    
    method protected char GetJsonValueWhenError (pJson as JsonObject,pName as char):
        define variable cValue as character no-undo.
        cValue = JsonUtil:GetStringValue(pJson, pName).
        if cValue = ? then 
            cValue = "null".
        return cValue.    
        catch e as IllegalArgumentError :
            return "".    
        end catch.    
    end method.
    
    method protected char GetJsonArrayValueWhenError (pJsonArray as JsonArray,pinum as integer):
        define variable cValue as character no-undo.
        cValue = JsonUtil:GetStringValue(pJsonArray, pinum).
        if cValue = ? then 
            cValue = "null".
        return cValue.    
        catch e as IllegalArgumentError :
            return "".    
        end catch.    
    end method.
    
    method public logical CompareField(pjson as JSONObject,phfld as handle,pcJsonName as character, pcErrorText as character):
        if phFld:extent > 0 then
            undo, throw new NotImplementedError("Compare of JSON extent with field extent").
        else do on error undo, throw: 
            case phFld:data-type:
                when "character" then
                    return phFld:buffer-value = pjson:GetCharacter(pcJsonName).
                when "longchar" or when "clob" then
                    return phFld:buffer-value = pjson:GetLongchar(pcJsonName).
                when "blob" then
                    return phFld:buffer-value = base64-decode(pjson:GetLongchar(pcJsonName)).
                when "int64" then
                    phFld:buffer-value = pjson:GetInt64(pcJsonName).
                when "integer" then
                    return phFld:buffer-value = pjson:GetInteger(pcJsonName).
                when "decimal" then
                    return phFld:buffer-value = pjson:GetDecimal(pcJsonName).
                when "logical" then
                    return phFld:buffer-value = pjson:GetLogical(pcJsonName).    
                when "date" then
                    return phFld:buffer-value = pjson:GetDate(pcJsonName).    
                when "datetime" then
                    return phFld:buffer-value = pjson:GetDatetime(pcJsonName).    
                when "datetime-tz" then
                    return phFld:buffer-value = pjson:GetDatetimeTZ(pcJsonName).    
            end case.
            catch e as Progress.Lang.Error :
                ThrowComparePropertyError(e, pcErrorText, phFld:data-type, phFld:buffer-name, pcJsonName, GetJsonValueWhenError(pJson,pcJsonName) ).
            end catch. 
        end. 
         
    end method.     
   
    method public void AssignField(pjson as JSONObject,phfld as handle):
        define variable cMsg       as character no-undo.
        define variable oJsonArray as JsonArray no-undo.
        define variable cWarning   as character no-undo.
                
        if phFld:extent > 0 then
        do:
            oJsonArray = pJson:GetJsonArray(phFld:serialize-name).
            AssignArrayField(ojsonArray, phfld).
        end.   
        else do on error undo, throw: 
            case phFld:data-type:
                when "character" then
                    phFld:buffer-value = pjson:GetCharacter(phFld:serialize-name).
                when "longchar" or when "clob" then
                    phFld:buffer-value = pjson:GetLongchar(phFld:serialize-name).
                when "blob" then
                    phFld:buffer-value = base64-decode(pjson:GetLongchar(phFld:serialize-name)).
                when "int64" then
                    phFld:buffer-value = pjson:GetInt64(phFld:serialize-name).
                when "integer" then
                    phFld:buffer-value = pjson:GetInteger(phFld:serialize-name).
                when "decimal" then
                    phFld:buffer-value = pjson:GetDecimal(phFld:serialize-name).
                when "logical" then
                    phFld:buffer-value = pjson:GetLogical(phFld:serialize-name).    
                when "date" then
                    phFld:buffer-value = pjson:GetDate(phFld:serialize-name).    
                when "datetime" then
                    phFld:buffer-value = pjson:GetDatetime(phFld:serialize-name).    
                when "datetime-tz" then
                    phFld:buffer-value = pjson:GetDatetimeTZ(phFld:serialize-name).    
            end case.
            catch e as Progress.Lang.Error :
                if e:GetMessageNum(1) = 12009
           //     and session:cpinternal = "ISO8859-1"
            then
                do:
                     new UnconvertableAsciiCharacterUtil():HandleJsonString(pJson, phFld, output cWarning).  .
                     if cWarning > "" then 
                          AddWarning(phfld:buffer-handle:serialize-name, cWarning).
                end.     
                else
                    ThrowPropertyError(e, phFld, GetJsonValueWhenError(pJson,phFld:serialize-name)).
            end catch. 
        end. 
         
    end method.    
    
    // wrapper in case the [arent buffer changed position
    // TODO  - create a better isolated API
    method protected logical CheckReadOnlyAndPrepareBuffer(phParent as handle,phBuffer as handle, pState as RowStateEnum):
        define variable lReadOnlyTable as logical no-undo.
        define variable roParent       as rowid no-undo.
        if valid-object(JsonReadHandler) then
        do:
            if valid-handle(phParent) and phParent:avail then
            do:
                roParent = phParent:rowid.
            end.
            lReadOnlyTable = JsonReadHandler:CheckReadOnlyAndPrepareBuffer(phBuffer,pState).
            if valid-handle(phParent) and roParent <> ? and (phParent:avail = false or roParent <> phParent:rowid) then
            do:
                phParent:find-by-rowid(roParent).
            end.    
        end.   
        return lReadOnlyTable.        
    end method.    
    
    method protected void AssignArrayField(pjsonArray as JSONArray,phfld as handle):
        define variable cMsg as character no-undo.
        define variable oJsonArray as JsonArray no-undo.
        define variable i as integer no-undo.
        define variable iStart as integer no-undo.
        define variable iEnd as integer no-undo. 
                
        do i = 1 to pjsonArray:length
        on error undo, throw:           
            case phFld:data-type:
                when "character" then
                    phFld:buffer-value(i) = pjsonArray:GetCharacter(i).
                when "longchar" or when "clob" then
                    phFld:buffer-value(i) = pJsonArray:GetLongchar(i).
                when "blob" then
                    phFld:buffer-value(i) = base64-decode(pJsonArray:GetLongchar(i)).
                when "int64" then
                    phFld:buffer-value(i) = pJsonArray:GetInt64(i).
                when "integer" then
                    phFld:buffer-value(i) = pJsonArray:GetInteger(i).
                when "decimal" then
                    phFld:buffer-value(i) = pJsonArray:GetDecimal(i).
                when "logical" then
                    phFld:buffer-value(i) = pJsonArray:GetLogical(i).    
                when "date" then
                    phFld:buffer-value(i) = pJsonArray:GetDate(i).    
                when "datetime" then
                    phFld:buffer-value(i) = pJsonArray:GetDatetime(i).    
                when "datetime-tz" then
                    phFld:buffer-value(i) = pJsonArray:GetDatetimeTZ(i).    
            end case.
            catch e as Progress.Lang.Error :
                ThrowPropertyError(e, phFld, GetJsonArrayValueWhenError(pJsonArray,i)).
            end catch. 
        end.
    end method.    
    
    method protected void ThrowPropertyError(pError as Error, phfld as handle, pcValue as character):
        define variable cMsg as character no-undo. 
        cMsg = subst("Cannot read &1 property '&2.&3' from JSON",phFld:data-type,phfld:buffer-handle:serialize-name,phfld:serialize-name).
        if pError:GetMessageNum(1) = 16060  or pError:GetMessageNum(1) = 16061 then 
             cMsg = cMsg + "." + subst(" JSON value &1 is not the expected data type.",pcValue).
        undo, throw new UnprocessableEntityError(cMsg,pError).
    end method.    
    
    method protected void ThrowComparePropertyError(pError as Error, pcErrorText as character, pcdatatype as character, pcBuffername as character, pcFieldname as character, pcValue as character):
        define variable cMsg as character no-undo. 
        cMsg = subst("Cannot read &1 property '&2.&3' from JSON &4 section",pcdatatype,pcBuffername,pcFieldname,pcErrorText).
        if pError:GetMessageNum(1) = 16060  or pError:GetMessageNum(1) = 16061 then 
             cMsg = cMsg + "." + subst(" JSON value &1 is not the expected data type.",pcValue).
        undo, throw new UnprocessableEntityError(cMsg,pError).
    end method. 
    
    /** read a buffer field from json  */
    method protected void ReadField(pjson as JSONObject,phfld as handle).
        define variable cMsg as character no-undo.
        define variable lLoaded as logical no-undo.
        
        if valid-object(JsonReadHandler) then 
            lLoaded = JsonReadHandler:ReadField(pjson, phfld).
        
        // allow ? if ReadField implementation does not return anything     
        if lLoaded <> true then
        do:                
            AssignField(pjson,phfld).
        end.
 
    end.
    
    method protected logical IsModeAppend(pcmode as char):
        if pcMode = "Append" then
            return true. 
        else if pcMode <> "Replace" then
            undo, throw new IllegalArgumentError(subst("Read mode '&1' passed to ReadTable. Valid values are 'Replace' or 'Append'",pcMode)). 
        return false.
    end method. 

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	destructor public JsonReader ( ):
	    for each ttBeforeSource:
	        if valid-handle(ttBeforeSource.handle) then
	            delete object ttBeforeSource.handle. 
	    end.    

	end destructor.
                                                                                                                                                                            
end class.