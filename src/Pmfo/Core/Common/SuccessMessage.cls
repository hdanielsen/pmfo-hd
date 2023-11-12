 
 /*------------------------------------------------------------------------
    File        : SuccessMessage
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 13 15:43:33 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.Common.ISuccessMessage.
using Pmfo.Core.Common.IKey from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

block-level on error undo, throw.

class Pmfo.Core.Common.SuccessMessage implements ISuccessMessage: 

    define public property Entity as character no-undo 
    get.
    protected set.
    
    define public property Key as IKey no-undo 
    get.
    protected set. 
    
    define protected property RowState as RowStateEnum no-undo 
    get.
    set.
    
    define protected property ActionName as character no-undo 
    get.
    set.
    
    define public property Message as character init ? no-undo 
    get():
        if this-object:Message = ? then
        do: 
            if ActionName > "" then  
                return subst("&1 of &2 with &3 was successful",
                              StringUtil:CamelToWords(Actionname),
                              BufferUtil:SingularizeSerializeName(Entity,true),
                              this-object:Key:ToString("text") 
                              
                             ).
             else if valid-object(RowState) then
                 return subst("&1 with &2 was &3",
                               BufferUtil:SingularizeSerializeName(Entity,true),
                               this-object:Key:ToString("text"), 
                               lc(Rowstate:ToString())
                              ).
             else 
                 return subst("&1 with &2 was updated", //???
                               BufferUtil:SingularizeSerializeName(Entity,true),
                               this-object:Key:ToString("text")
                              ).                 
        end.
        else 
            return this-object:Message.                                       
    end.    
    set.
    
    define property UrlId      as character no-undo 
         get():
             return subst("/&1(&2)",Entity,Key:ToString()).
         end get. 
     
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    constructor public SuccessMessage (pcEntity as character, poKey as IKey ):
        super ().
        Entity = pcEntity.
        this-object:Key = poKey. 
    end constructor.
    
    constructor public SuccessMessage (pcEntity as character, poKey as IKey, pcmessage as character ):
        this-object(pcEntity,poKey).
        this-object:Message = pcMessage. 
    end constructor.
    
    constructor public SuccessMessage (pcEntity as character, poKey as IKey, pRowState as RowStateEnum ):
        this-object(pcEntity,poKey).
        RowState = pRowState. 
    end constructor.
    
    constructor public SuccessMessage (pcEntity as character, poKey as IKey, pRowState as RowStateEnum, pcActionName as character ):
        this-object(pcEntity,poKey,pRowState).
        Actionname = pcActionName.
    end constructor.
    
    method public JsonObject ToJson():
        define variable oJson as JsonObject no-undo.
        oJson = new JsonObject().
        oJson:add("entity",Entity).
        oJson:add("message",this-object:Message).
        oJson:add("urlId",UrlId).
        oJson:add("key",Key:ToJson()).
        oJson:add("stringKey",Key:ToString()).
        return oJson.       
    end method.
     
end class.