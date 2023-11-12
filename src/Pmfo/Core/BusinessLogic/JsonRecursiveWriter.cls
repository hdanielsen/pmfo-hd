 
 /*------------------------------------------------------------------------
    File        : JsonRecursiveWriter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 30 20:45:59 EDT 2019
    Notes       :  
----------------------------------------------------------------------*/


block-level on error undo, throw.
 
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IJsonRecursiveWriter from propath.
using Pmfo.Core.BusinessLogic.JsonWriter from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.DatasetUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Core.BusinessLogic.JsonRecursiveWriter inherits JsonWriter implements IJsonRecursiveWriter: 
    
    define protected property RequestedLevels   as integer no-undo get. set. 
    define protected property CurrentLevel      as integer no-undo get. set. 
    
    define protected property SkipTop           as logical no-undo get. set. 
    
    define protected property SkipRelation      as handle  no-undo get. set. 
    define protected property RecursiveRelation as handle  no-undo get. set. 
    
    constructor public JsonRecursiveWriter ():
        super ().
    end constructor.
    
    // support to skip the top level allows request for a single top table using filter
    // where the filter identifies ONE record that then is used as filter for the 
    // children - the single top will be in the data, but not reruned in json   
    constructor public JsonRecursiveWriter (plSkiptop as logical):
        super ().
        SkipTop = plSkiptop.
    end constructor.
    
    method public JsonObject WriteDataset(phDataset as handle, pTableRequest as IGetDataTableRequest, piLevels as integer):
        define variable hBuffer            as handle no-undo.
        define variable oJson              as JsonObject no-undo. 
        define variable oJsonArray         as JsonArray no-undo.
         
        define variable iRelation          as integer no-undo.
        define variable hRelation          as handle no-undo.
      
        define variable hQuery             as handle no-undo.   
         
        RequestedLevels = piLevels.
        if not valid-object(pTableRequest) then
        do: 
            if piLevels > 0 then 
                undo, throw new IllegalArgumentError("Level cannot be provided without TableRequest"). 
           // return super:WriteDataset(phDataset).
        end.
      
        do iRelation = 1 to phDataset:num-relations:
            hRelation = phDataset:get-relation (iRelation).
            if hRelation:recursive then 
                RecursiveRelation = hRelation.       
            else
                SkipRelation = hRelation.               
        end.    
      
        if not valid-handle(RecursiveRelation) then
        do:
            // we should support this
            undo, throw new NotImplementedError("WriteDataset for TableRequest for non-recursive dataset"). 
        end.
        
        oJson = new JsonObject().       
        
        if valid-object(pTableRequest) then 
            hbuffer = DatasetUtil:GetBufferByTableName(phDataset, pTableRequest:TableName).
        else
            hBuffer = phDataset:get-buffer-handle(1).
            
        CurrentLevel = 1.
        
        if valid-object(pTableRequest) and pTableRequest:FindUnique then 
        do:
            do on error undo, throw: 
                hbuffer:find-unique (pTableRequest:QueryDefinition:ToString()).
                catch e as Progress.Lang.Error :
                    // 3166 - more than one
                    // 138 - not on file 
                    if e:GetMessageNum(1) = 138 or e:GetMessageNum(1) = 3166 then
                    do: 
                         undo, throw new TableNotFoundError(hBuffer, pTableRequest, e).
                    end.
                    else 
                        undo, throw new NotFoundError(subst("'&1'",hBuffer:serialize-name),e). 
                end catch.
            end.
            AddBuffer(oJson,hBuffer,"").
        end.
        // support a request for a top level as a grouping without returning the top
        else if SkipTop then 
        do: 
            if not valid-handle(SkipRelation) then
                 undo, throw new UnsupportedOperationError("SkipTop is only supported for recursive relations defined with two tables"). 
            do on error undo, throw:
                hbuffer:find-unique (pTableRequest:QueryDefinition:ToString()).
                catch e as Progress.Lang.Error :
                    // 3166 - more than one
                    // 138 - not on file - add an empty array 
                    if e:GetMessageNum(1) = 138 then
                    do: 
                        oJson:Add(hbuffer:serialize-name,new JsonArray()).
                    end.    
                    // 3166 - more than one - illegal state
                    if e:GetMessageNum(1) = 3166 then
                         undo, throw new IllegalStateError("More than one record found using query for SkipTop").
                    else 
                        undo, throw e. 
                end catch.
            end.
            
            // add children using the relation r
            if hBuffer:avail then
                AddTable(oJson,SkipRelation,"").
        end.    
        else do:
            oJsonArray = new JsonArray().
            oJson:Add(hBuffer:serialize-name,oJsonarray).
            
            create query hQuery.
            hQuery:add-buffer(hBuffer).
            if valid-object(pTableRequest) then
                hQuery:query-prepare(pTableRequest:QueryString).
            else 
                hQuery:query-prepare(subst("for each &1",hBuffer:name)).
            hQuery:query-open().
            hQuery:get-first().
            
            do while hbuffer:avail:
                // only top level - necessary if no query 
                 if not HasParent(RecursiveRelation) then
                    AddBuffer(oJsonArray,hbuffer,"").
                hQuery:get-next. 
            end.    
        end.
         
        return oJson.
 
        finally:
            if valid-handle(hQuery) then  
                delete object hQuery.       
        end finally. 
    end method.     
    
    method protected override logical AddChildren(pJson as JsonObject,phBuffer as handle):
        define variable lOk as logical no-undo.
        if RequestedLevels = ? or CurrentLevel < RequestedLevels then 
        do:
            CurrentLevel = CurrentLevel + 1. 
            lOk = super:AddChildren(pJson,phbuffer).
            CurrentLevel = CurrentLevel - 1. 
        end. 
        return lOk.
    end method.
    
    method protected logical HasParent(phRelation as handle):
        define variable hBuffer as handle no-undo.
        define variable cJoin   as character no-undo.
        cJoin = "where " + FindParentExpression(phRelation).
        // use buffer to keep current record avail  
        if phRelation:parent-buffer = phRelation:child-buffer then
            create buffer hBuffer for table phRelation:parent-buffer buffer-name phRelation:parent-buffer:name.
        else hBuffer = phRelation:parent-buffer.    
        hBuffer:find-first(cJoin) no-error.
        if hbuffer:avail then
            return true.
        return false.
    end method.
    
    // returns a where expression to find children from parent
    method protected character FindParentExpression(phRelation as handle):
        define variable hChildBuffer  as handle    no-undo.
        define variable hParentBuffer as handle    no-undo.
        define variable cValue        as character no-undo.
        define variable i             as integer   no-undo.
        define variable cExpression   as character no-undo.
      
        hChildBuffer = phRelation:child-buffer.
        hParentBuffer = phRelation:parent-buffer.
         
        if not hChildBuffer:avail then
        do:
            undo, throw new IllegalStateError(subst("Child buffer '&1' not available in GetFindParentExpression",hChildBuffer:name)).
        end.
               
        do i = 1 to num-entries(phRelation:relation-fields) by 2:
            cValue = hChildBuffer:buffer-field(entry(i + 1,phRelation:relation-fields)):buffer-value.
            cExpression = cExpression 
                   + (if cExpression = "" then "" else " and ")
                   + subst("&1.&2 = '&3'",hParentBuffer:name,entry(i,phRelation:relation-fields),cValue). 
                   .
        end. 
        return cExpression. 
    
    end method.
    
    // read the many to many table and get data from the actual table so the json is serialized
    // as a tree without expsing the many to meny 
    method protected logical AddRecursiveTable(pJson as JsonObject,phHandle as handle):
        define variable hQuery     as handle no-undo.
        define variable cJoin      as character no-undo.
        define variable cChildJoin as character no-undo.
        define variable hBuffer    as handle no-undo.
        define variable oJsonArray as JsonArray no-undo.
        define variable oJson      as JsonObject no-undo.
        define variable cName      as character no-undo.
        define variable hRecurseBuffer as handle no-undo.
        define variable cHideFields as character no-undo.
      
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
        hRecursebuffer = RecursiveRelation:child-buffer.
        do while hBuffer:avail:
            cChildJoin = " where " + JoinExpression(RecursiveRelation).
            if RecursiveRelation:foreign-key-hidden then
                cHideFields = GetForeignKeys(RecursiveRelation).
            hRecursebuffer:find-unique(cChildJoin) no-error.    
            if hREcursebuffer:avail then
            do:        
               AddBuffer(oJsonArray,hRecursebuffer,"").
            end.    
            hQuery:get-next. 
        end.    
        if oJsonArray:length > 0 then
        do:
            pJson:Add(hRecursebuffer:serialize-name,oJsonArray).
            return true.
        end.
        return false.
        finally:
            if valid-handle(hQuery) then
                delete object hQuery.
        end. 
    end method.
    
    method protected override logical AddTable(pJson as JsonObject,phHandle as handle,pcHide as character):
        define variable oJsonArray as JsonArray no-undo.
        define variable hBuffer    as handle no-undo.
        define variable cWhere     as character no-undo.
        
        if phHandle:type = "data-relation" then
        do: 
            if SkipRelation = phHandle then
                return AddRecursiveTable(pJson, phHandle). 
        end.
        
        return super:AddTable(pJson,phHandle,pchide).
        
    end method.
    
     
      
end class.