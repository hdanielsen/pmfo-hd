 
 /*------------------------------------------------------------------------
    File        : DataError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Jun 03 08:28:44 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.DataUpdateError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonConstruct from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Core.Error.DataUpdateError inherits ApplicationError:        
    
    define protected property JsonWrapper as JsonObject no-undo get. set.
                  
    constructor public DataUpdateError ( input pmsg as character , phDataset as handle, pRequestedBuffer as character, pRequestType as RequestTypeEnum):
        super (input subst("Update of '&1' failed",phDataset:serialize-name)).
    end constructor.

    constructor public DataUpdateError ( input pmsg as character,phDataset as handle, pRequestedBuffer as character, pRequestType as RequestTypeEnum, input innErr as Progress.Lang.Error ):
        super (input subst("Update of '&1' failed",phDataset:serialize-name), input innErr).
    end constructor.

    method public void CreateJsonError  (phDataset as handle, pRequestedBuffer as character, pRequestType as RequestTypeEnum):
        define variable i        as integer no-undo.
        define variable hBuffers as handle  extent no-undo.
        
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
    
    method public JsonObject GetBufferError (phBuffer as handle):
    
    end method.    
    
    method public JsonArray  GetBufferErrors (phBuffer as handle):
        define variable hQuery as handle no-undo.
        define variable ichild as integer no-undo.
        define variable hRelation as handle no-undo.
        define variable hChildbuffer as handle no-undo.
        define variable hBeforebuffer as handle no-undo.
        define variable jsonConstruct as JsonConstruct no-undo.
        define variable json      as JsonObject no-undo.
        define variable jsonArray as JsonArray  no-undo.
        define variable iCount as integer no-undo.
        
        create query hQuery.
        
        hQuery:add-buffer(hBeforeBuffer).
        hQuery:Query-prepare(subst("for each &1",hBeforeBuffer:name)).
        hQuery:query-open.
        hQuery:get-first.
        
        do while hBeforebuffer:avail:
            iCount = iCount + 1.
            if iCount > 1  then 
                 undo, throw new IllegalOperationError("More than one row found for single row request").
            
          //  if plAddSingle then
/*            do:                             */
/*                json = new JsonObject().    */
/*            end.                            */
/*            else do:                        */
/*                jsonArray = new JsonArray().*/
/*            end.                            */
            if hBeforebuffer:error then 
                 pJson:
            
            do iChild = 1 to phBuffer:num-child-relations:
                
                hRelation = phBuffer:get-child-relation (iChild).
                    // there will be no child node in json for repos
                if hRelation:reposition = false then
                do:
                    hChildBuffer = phBuffer:get-child-relation (iChild):child-buffer.
                //    AddBufferErrors(jsonConstruct,hChildBuffer,false).
                    
                     // add empty array if no data - this is typicaly expected by clients - null errors is common if not 
                   // ?? oJson:Add(hChildBuffer:serialize-name,new JsonArray()).
                end.        
            end. // ichild loop
            hQuery:get-next.
        end.
        finally:
            if valid-handle(hQuery) then
                delete object hQuery.
        end.    
    end method.
    
    method public JsonObject GetRow(phBeforeBuffer as handle):
        
        
    end. 
    
    
    method public JsonConstruct GetJson():
        
    end. 

end class.