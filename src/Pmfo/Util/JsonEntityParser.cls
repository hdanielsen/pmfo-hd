 
 /*------------------------------------------------------------------------
    File        : JsonKeyValidator
    Purpose     : Was intended for validation of foreign keys before using data:read-json. 
                  If a child has a foreing key value that does not match the json parent it just disappears
                  (or ends up as child elsewhere maybe? )   
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 07 07:26:20 EDT 2023
    Notes       : Not in use. Keeping in case we decide to complete and as reminder/documentation 
                  Instead of implementing this for dataset load we may as well consider always using JsonReader
                  Track of parent key value is complex and alreayd managed in JsonReader
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.JsonParser from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Util.JsonEntityParser inherits JsonParser: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable Dataset as handle no-undo.
    define variable Path as character no-undo.
    
    constructor public JsonEntityParser ():
        super ().
    end constructor.
        
    method public override void Parse(pjsonObject as JsonObject, pcName as character):
        define variable cCurrentPath as character no-undo.
        define variable cNames       as character extent no-undo.
        define variable hbuffer      as handle no-undo.
        cCurrentPath = Path.
        Path = subst("&1/&2",Path,pcname).
        cNames = pJsonObject:GetNames().
        if extent(cNames) > 1 
        or pJsonObject:GetType(cNames[1]) = JsonDataType:ARRAY
        or pJsonObject:GetType(cNames[1]) = JsonDataType:OBJECT then
        do:
            hBuffer = DatasetUtil:GetBufferByPathName(this-object:Dataset, Path).
        end.  
        super:Parse(pjsonObject,pcname).
        Path = cCurrentPath.
         
    end method.
          
    method public override void Parse(pjsonArray as JsonArray, pcName as character):
        define variable cCurrentPath as character no-undo.
        define variable hbuffer      as handle no-undo.
        
        cCurrentPath = Path.
        Path = subst("&1/&2",Path,pcname).
        
        hBuffer = DatasetUtil:GetBufferByPathName(this-object:Dataset, Path).
        
        
        super:Parse(pjsonArray,pcname).
        Path = cCurrentPath.
        
    end method.      
    
end class.