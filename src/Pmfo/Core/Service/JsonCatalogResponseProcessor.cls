 
 /*------------------------------------------------------------------------
    File        : JsonCatalogResponseProcessor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Core.Service.JsonCatalogResponseProcessor: 
         
            
    constructor public JsonCatalogResponseProcessor (  ):
        super ().
    end constructor.
            
    method public void processGetResponse(pJson as JsonObject, pcSelectedFields as character, pcTopNode as character):
         ProcessEntity(pJson:GetJsonObject(pcTopNode), pcSelectedFields). 
    end.    
    
    method protected void ProcessEntities(pJsonArray  as JsonArray, pcSelectFields as character):
        define variable i as integer no-undo.
        do i = 1 to pJsonArray:length:
            ProcessEntity(pJsonArray:GetJsonObject(i),pcSelectFields).
        end.                                    
    end method.
    
    method protected void ProcessEntity(pJson  as JsonObject, pcSelectFields as character):
        define variable cNames as character extent no-undo.
        define variable i as integer no-undo.
        cNames = pJson:GetNames().
        do i = 1 to extent(cNames):
            if cNames[i] = "entities" then
                ProcessEntities(pJson:getJsonArray("entities"),pcSelectFields). 
            else if lookup(cNames[i],pcSelectFields) = 0 then
            do:
                pJson:Remove(cNames[i]).  
            end.
        end.    
                                        
    end method.
    
      
end class.