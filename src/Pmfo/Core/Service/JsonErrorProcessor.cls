 
 /*------------------------------------------------------------------------
    File        : JsonErrorProcessor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Jun 07 08:23:49 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonConstruct from propath.
using Pmfo.Util.StringUtil from propath.

block-level on error undo, throw.

class Pmfo.Core.Service.JsonErrorProcessor: 
    
    define property JsonWrapper as JsonConstruct no-undo get. set.
    
   
    method public void GetErrors  (phDataset as handle, pRequestedBuffer as character, pRequestType as RequestTypeEnum):
        define variable i        as integer no-undo.
        define variable hBuffers as handle  extent no-undo.
        define variable JsonWrapper as jsonObject no-undo.
        
        JsonWrapper = new JsonObject().
        
        if pRequestedBuffer > "" then do:
            extent(hBuffers) = 1.
            hBuffers[1] = phDataset:get-buffer-handle(pRequestedbuffer).
            if not valid-handle(hBuffers[i]) then
                undo, throw new IllegalArgumentError(subst("Requested buffer '&1' not found in returned dataset",pRequestedBuffer)).
        end.
        else do:
            extent(hBuffers) = phDataset:num-top-buffers.
            do i = 1 to phDataset:num-top-buffers:
                hBuffers[i] = phdataset:get-buffer-handle(i).
            end.
        end.
        do i = 1 to extent(hBuffers):
            if RequestTypeEnum:SingleRow:Equals(pRequestType) then 
            do:
                if i > 1 then 
                    undo, throw new IllegalOperationError("More than one row found for single row request").
    
            end.
        end.
        
    end method.
    
    method public void AddError (pJson as JsonObject):
         define variable oJsonArray as JsonArray no-undo.
            
         case true:
            // single error? add an simple JSON object
            when not valid-object(JsonWrapper) then
                assign JsonWrapper = pJson.
            
            // if there's already an array, add to it                
            when type-of(JsonWrapper, JsonArray) then
                cast(JsonWrapper, JsonArray):Add(pJson).
            
            // if there's a single json object, switch to an array
            when type-of(JsonWrapper, JsonObject) then
            do:
                assign oJsonArray = new JsonArray().
                oJsonArray:Add(cast(pJson, JsonObject)).
                oJsonArray:Add(pJson).               
                assign JsonWrapper = oJsonArray.
            end.
        end case.
    end method.    
    
    method public JsonArray GetBufferErrors (phBuffer as handle):
        define variable hQuery as handle no-undo.
      
        define variable hBeforebuffer as handle no-undo.
        define variable jsonConstruct as JsonConstruct no-undo.
        define variable oJsonChild    as JsonArray no-undo.
        define variable oJsonArray as JsonArray  no-undo.
        define variable iCount as integer no-undo.
        
        hBeforebuffer = phBuffer:before-buffer.
        create query hQuery.
        
        hQuery:add-buffer(hBeforeBuffer).
        hQuery:query-prepare(subst("for each &1",hBeforeBuffer:name)).
        hQuery:query-open.
        hQuery:get-first.
        oJsonArray = new JsonArray().
        do while hBeforebuffer:avail:
            iCount = iCount + 1.
            if hBeforebuffer:error then 
                oJsonArray:Add(GetError(iCount, hBeforeBuffer)).
            
            hQuery:get-next.
        end.
        return oJsonArray. 
            
        finally:
           if valid-handle(hQuery) then
                delete object hQuery.
        end.
    end method.
    
    method public void AddChildren(pJson as JsonObject,phBuffer as handle):
        define variable ichild as integer no-undo.
        define variable hRelation as handle no-undo.
        define variable hChildbuffer as handle no-undo.
        define variable oJsonArray    as JsonArray no-undo.
        define variable oJsonChild    as JsonArray no-undo.
        do iChild = 1 to phBuffer:num-child-relations:
            hRelation = phBuffer:get-child-relation (iChild).
                // there will be no child node in json for repos
            if hRelation:reposition = false then
            do:
                hChildBuffer = phBuffer:get-child-relation (iChild):child-buffer.
                oJsonChild = GetBufferErrors(hChildBuffer).
                if valid-object(oJsonChild) then
                do:
                    pJson:add(hChildbuffer:serialize-name,oJsonChild).
                end.
            end.        
        end. // ichild loop
    end. 
    
    method public JsonObject GetError(pindex as integer,phBeforeBuffer as handle):
        define variable oJson as JsonObject no-undo.
        define variable cMsg as character no-undo.
        define variable iMsg as integer no-undo.
        define variable ipos as integer no-undo.
        if phBeforeBuffer:error then
        do:
            cMsg = phBeforebuffer:error-string.
            ipos = r-index(cMsg,"(").
            if ipos > 0 then
            do on error undo, throw:
                StringUtil:ExtractParenthesis(substr(cMsg,iMsg - 1),true).
                iMsg = integer(StringUtil:ExtractParenthesis(substr(cMsg,iMsg - 1),true)).
                catch e as Error :
                   // nothing    
                end catch.
            end.    
            oJson = new jsonObject().
            oJson:Add("index",pindex).
            oJson:Add("errorMsg",cMsg).
            oJson:Add("errorNum",Imsg).
            
        end.
        return oJson.
    end. 
    
    
    method public JsonConstruct GetJson():
        
    end. 
    
    
end class.