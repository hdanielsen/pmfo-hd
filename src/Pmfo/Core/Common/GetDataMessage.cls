
/*------------------------------------------------------------------------
    File        : GetDataMessage   
    Purpose     : 
    Syntax      : 
    Description : Support exposing server side overrides in a GetData Request in business logic 
    Author(s)   : hdaniels
    Created     : Wed Jan 31 12:00:54 EDT 2019
    Notes       :  
  ----------------------------------------------------------------------*/

using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.Common.IKey from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.Common.IGetDataMessage from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Pmfo.Core.Common.Key from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.

class Pmfo.Core.Common.GetDataMessage implements IGetDataMessage:  
    define public property Id              as integer  no-undo get. protected set.
    define public property Entity          as character no-undo get. protected set.  
    define public property Message         as character no-undo get. protected set.
    define public property Key             as IKey      no-undo get. protected set.
    define public property Keys            as IKey  extent no-undo get. protected set.
   
    constructor public GetDataMessage (input pId as integer, 
                                       input pMessage as character,
                                       input pEntity as character,
                                       input pKey as IKey
                                      ):
       // Assert:y(pId,"Id").
        Assert:NotEmpty(pMessage,"Message").
        
        this-object:Id = pId .
        this-object:Message = pMessage.
        this-object:Entity = pEntity.
        this-object:Key = pKey.
        
       // Entity = entry(1,this-object:Key,"-").  
    end constructor.
    
    constructor public GetDataMessage (input pId as integer, 
                                      input pMessage as character,
                                      input pEntity as character,
                                      input pKeys as IKey extent
                                      ):
       // Assert:y(pId,"Id").
        Assert:NotEmpty(pMessage,"Message").
        
        this-object:Id = pId .
        this-object:Message = pMessage.
        this-object:Entity = pEntity.
        this-object:Keys = pKeys.
        
       // Entity = entry(1,this-object:Key,"-").  
    end constructor.
     
    // equals if key is same so contains returns true if already in collection   */
    method override public logical Equals( input objectRef as Progress.Lang.Object ):
        if valid-object(objectRef) then
        do:
            if super:Equals(input objectRef) = false then
            do: 
                if type-of(objectRef,IGetDataMessage) then
                do:
                   return cast(objectRef,IGetDataMessage):Id = this-object:Id.
                   // to add keys
                end.   
            end.
            else 
                return true.       
        end.
        return false.

    end method.
    
    method public JsonObject ToJson().
       define variable oJson as JsonObject no-undo.
       define variable oJsonArray as JsonArray no-undo.
        
       define variable i as integer no-undo.
       oJson = new JsonObject().
       oJsonArray = new JsonArray().
       oJson:Add("entity",this-object:Entity).
       oJson:Add("id",this-object:Id).
       oJson:Add("cause",this-object:Message).
       
       if extent(this-object:Keys) > 0 then 
       do:
           oJsonArray = new JsonArray().
            
           do i = 1 to extent(this-object:Keys):
               oJsonArray:Add(this-object:Keys[i]:ToJson()) .   
           end.
           oJson:Add("key",oJsonArray).     
       end.    
       else do: 
           oJson:add("key",this-object:Key:ToJson()).
       end.
       
       return oJson.
    end method.
    
end class.