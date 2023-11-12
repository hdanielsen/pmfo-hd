 
 /*------------------------------------------------------------------------
    File        : JsonWalker
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Jan 17 06:13:05 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/
 

block-level on error undo, throw.

using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.

class Pmfo.Util.JsonParser:
     method public void Parse(pjsonObject as JsonObject):
         Parse(pJsonObject,"").     
     end method.     
    
     method public void Parse(pjsonObject as JsonObject, pcName as character):
        define variable cNames as character extent no-undo.
        define variable i as integer no-undo.
        define variable iType as integer no-undo.
        define variable cCurrentArray as character no-undo.
        define variable cCurrentObject as character no-undo.
        cNames = pjsonObject:GetNames().
        do i = 1 to extent(cNames):
            iType = pJsonObject:GetType(cNames[i]).
            if iType = JsonDataType:ARRAY then 
                Parse(pJsonObject:GetJsonArray(cNames[i]),cNames[i]).  
          
            else if iType = JsonDataType:Object then 
                Parse(pJsonObject:GetJsonObject(cNames[i]),cNames[i]).          
        end.    
    end method.     
    
    
    method public void Parse(pjsonArray as JsonArray,pcName as character):
        define variable i as integer no-undo. 
        define variable iType as integer no-undo.
        
        do i = 1 to pJsonArray:length: 
            iType = pJsonArray:GetType(i).
            if iType = JsonDataType:Object then 
                Parse(pJsonArray:GetJsonObject(i),pcName).      
        end.    
    end method.     
    
    
end class.