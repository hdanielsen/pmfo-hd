 
 /*------------------------------------------------------------------------
    File        : GetDataQuestion
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Jul 3 2023
    Notes       :             
----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Assert from propath.
using Pmfo.Core.Common.IGetDataQuestion from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Core.Common.GetDataQuestion implements IGetDataQuestion: 
    define public    property Key     as character no-undo get. protected set.
    define public    property Message as character no-undo get. public set.
    define public    property Entity  as character no-undo get. protected set. 
    
    constructor public GetDataQuestion (pKey as character, pMessage as character ):
        Assert:NotEmpty(pKey,"Key").
        Assert:NotEmpty(pMessage,"Message").
        
        this-object:Key = pKey. 
        this-object:Message = pMessage.
        Entity = entry(1,this-object:Key,"-").  
    end constructor.
     
    // equals if key is same so contains returns true if already in collection   */
    method override public logical Equals( input objectRef as Progress.Lang.Object ):
        if valid-object(objectRef) then
        do:
            if super:Equals(input objectRef) = false then
            do: 
                if type-of(objectRef,IGetDataQuestion) then
                   return cast(objectRef,IGetDataQuestion):Key = this-object:Key.
            end.
            else 
                return true.       
        end.
        return false.

    end method.
    
    method public JsonObject ToJson().
       define variable oJson as JsonObject no-undo.
       oJson = new JsonObject().
       oJson:Add("entity",this-object:Entity).
       oJson:Add("key",this-object:Key).
       oJson:Add("message",this-object:Message).
       return oJson.
    end method.
     
end class.