 
 /*------------------------------------------------------------------------
    File        : JsonWriter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 30 20:45:59 EDT 2019
    Notes       :  
----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Util.BufferUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.BusinessLogic.IJsonWriter from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
 
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.

class Pmfo.Core.BusinessLogic.JsonWriter implements IJsonWriter: 
    
    define public property DynamicExtentFields as character no-undo 
        get. 
        set(pcFieldlist as character):
            define variable i as integer no-undo.
            do i = 1 to num-entries(pcFieldList):
                if lookup(".",entry(i,pcFieldList)) < 2 then
                   undo, throw new IllegalArgumentError(subst("Entry '&1' in DynamicExtentFields '&2' has invalid format. The field name must be qualified with table wuth a period.",entry(i,pcFieldList),pcFieldList)). 
            end.    
        end. 
    constructor public JsonWriter ():
        super ().
    end constructor.
    
    method public JsonObject WriteDataset(phDataset as handle):
        define variable iBuffer as integer no-undo.
        define variable iNumTopBuffers as integer no-undo.
        define variable hBuffer as handle no-undo.
        define variable oJson   as JsonObject no-undo. 
        define variable oJsonArray as JsonArray no-undo.
        define variable cDeactivated as character no-undo.
        define variable iRelation as integer no-undo.
        define variable hRelation as handle no-undo.
        
        do iRelation = 1 to phDataset:num-relations:
            hRelation = phDataset:get-relation (iRelation).
            if hRelation:active = false then
                cDeactivated = cDeactivated
                             + (if cDeactivated = "" then "" else ",")
                             +  hRelation:child-buffer:name. 
        end.    
        // AssertDataset(phDataset).
        oJson = new JsonObject().       
        do iBuffer = 1 to phDataset:num-top-buffers:
            hBuffer = phDataset:get-top-buffer(iBuffer).
            if lookup(hBuffer:name,cDeactivated) = 0 then
                AddTable(oJson,hBuffer).
        end.
        
        return oJson.
        
    end method.     
    
    // final - not always called  - override the one with hide fields param
    method protected final logical AddTable(pJson as jsonObject,phBuffer as handle):
        return AddTable(pJson,phbuffer,"").
    end method.
    
    method protected logical AddTable(pJson as jsonObject,phHandle as handle, pcHideFields as character):
        define variable oJsonArray as JsonArray no-undo.
        define variable hBuffer as handle no-undo.
        
        oJsonArray = WriteRows(phHandle,pcHideFields).
        
        if oJsonArray:length > 0 then
        do:
            hBuffer = if phHandle:type = "buffer" then phHandle 
                      else phHandle:child-buffer.
            pJson:Add(hBuffer:serialize-name,oJsonArray).
            return true.
        end.      
        return false.
    end method.
        
    // final - override AddTable   
    method protected final JsonArray WriteRows(phHandle as handle, pcHideFields as character):
        define variable hQuery  as handle no-undo.
        define variable cJoin as character no-undo.
        define variable hBuffer as handle no-undo.
        define variable oJsonArray as JsonArray no-undo.
        define variable oJson as JsonObject no-undo.
        
        oJsonArray = new JsonArray().
        if phHandle:type = "data-relation" then
        do: 
            cJoin = " where " + JoinExpression(phHandle).
            hBuffer = phHandle:child-buffer.
        end.
        else 
            hBuffer = phHandle.
            
        create query hQuery.
        hQuery:add-buffer(hBuffer).
          
        hQuery:query-prepare(subst("for each &1 &2",hbuffer:name,cJoin)).
     
        hQuery:query-open().
        hQuery:get-first.
        do while hBuffer:avail:
            AddBuffer(ojsonArray,hBuffer,pcHideFields).
            hQuery:get-next. 
        end.    
        return oJsonArray.
        finally:
            if valid-handle(hQuery) then
                delete object hQuery.
        end. 
    end method.
    
    // final - use DynamicExtentFields 
    method protected final JsonArray WriteExtent(phFld as handle):
        define variable oJsonArray as JsonArray no-undo.
        define variable i as integer no-undo.
        define variable lDynamic as logical no-undo.
        lDynamic = DynamicExtentFields > "" 
                   and lookup(subst("&1.&2",phFld:table-handle:name,phFld:name),DynamicExtentFields) > 0.
        oJsonArray = new JsonArray().
        do i = 1 to phFld:extent:
            if lDynamic = false or phFld:buffer-value(i) > "" then
                oJsonArray:Add(phFld:buffer-value(i)).
        end.
        return oJsonArray.
    end method.
    
    method protected logical AddBuffer(pJson as jsonObject,phHandle as handle, pcHideFields as character):
        define variable oJson as JsonObject no-undo.
        oJson = WriteBuffer(phHandle,pcHideFields).
        if valid-object(oJson) then
        do:
            pjson:Add(phHandle:serialize-name,oJson).
            return true.
        end.    
        return false.
    end method.
    
    
    method protected logical AddBuffer(pJsonArray as jsonArray,phHandle as handle, pcHideFields as character):
        define variable oJson as JsonObject no-undo.
        oJson = WriteBuffer(phHandle,pcHideFields).
        if valid-object(oJson) then
        do:
            pJsonArray:Add(oJson).
            return true.
        end.    
        return false.
    end method.
    
    // final - override AddBuffer  
    method protected final JsonObject WriteBuffer(phBuffer as handle, pcHideFields as character):
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        if AddFields(oJson,phBuffer,pcHideFields) then
            return oJson.
        return ?.         
    end method.
    
    method protected logical AddFields(pJson as jsonObject, phHandle as handle, pcHideFields as character):
        define variable iFld as integer no-undo.
        define variable hFld as handle no-undo.
        define variable lFieldOk as logical no-undo.
        define variable lChildren as logical no-undo.
        define variable cLong as longchar no-undo.
        define variable mMem as memptr no-undo.
        define variable hBuffer as handle no-undo.
        
        // not needed? 
        hBuffer = if phHandle:type = "buffer" then phHandle 
                  else phHandle:child-buffer.  
            
        do iFld = 1 to hBuffer:num-fields on error undo, throw:
            hFld = hbuffer:buffer-field(iFld).
            if hFld:serialize-hidden = false 
            and lookup(hFld:name,pcHideFields) = 0 then
            do: 
                if hfld:extent > 0 then 
                    pJson:Add(hfld:serialize-name,WriteExtent(hFld)).
                else do:
                    if hfld:data-type <> "clob" and hFld:data-type <> "blob" then
                    do:
                        if hfld:buffer-value <> ? then
                        do:
                            pJson:Add(hfld:serialize-name,hFld:buffer-value).
                        end.
                        else
                            pJson:AddNull(hfld:serialize-name).
                    end.
                    else do:
                        copy-lob hfld:buffer-value to mMem.
                        if get-size(mMem) > 0 then
                            pJson:Add(hfld:serialize-name,base64-encode(mMem)).
                        else
                            pJson:AddNull(hfld:serialize-name).
                        set-size(mMem) = 0.    
                    end.
                end.    
                lFieldOk = true.
            end.  
            catch e as Progress.Lang.Error :
                undo, throw new ApplicationError(subst("Cannot write &1 with datatype &2 value &3",
                                                        hfld:serialize-name,
                                                        hFld:data-type,
                                                        hFld:buffer-value
                                                       )
                                                  ,e).      
            end catch.   
        end.
        
        lChildren = AddChildren(pJson,hBuffer).
        
        return lChildren or lFieldOk.
         
    end method.
    
/*    method protected logical AddLob(pJson as jsonObject,pFld as handle):*/
/*                                                                        */
/*    end method.                                                         */
    
    method protected character GetForeignKeys(phRelation as handle): 
        define variable i as integer no-undo.
        define variable cFld as character no-undo.
        define variable cList as character no-undo.
        do i = 2 to num-entries(phRelation:relation-fields) by 2:
            cFld  = entry(i,phRelation:relation-fields).
            cList = cList 
                  + (if cList = "" then "" else ",")
                   + cFld.
        end.
        return cList. 
    end method.
    
    method protected character JoinExpression(phRelation as handle ):
        return BufferUtil:GetJoinExpression(phRelation,false).
    end method.    
    
    method protected logical AddChildren(pJson as JsonObject,phBuffer as handle):
        define variable hRelation as handle no-undo.
        define variable i as integer no-undo.
        define variable lOk as logical no-undo.
        define variable cHideFields as character no-undo.
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phbuffer:get-child-relation(i).
            if hRelation:active then
            do:
                if hRelation:foreign-key-hidden then
                    cHideFields = GetForeignKeys(hRelation).
                if AddTable(pJson,hRelation,cHideFields) then
                    lok = true.
            end.    
        end.     
        return lok.
    end method.
      
end class.