 
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
 
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Progress.Json.ObjectModel.JsonObject from propath.

class Pmfo.Core.BusinessLogic.JsonManyToManyWriter : 
    
    constructor public JsonManyToManyWriter ():
        super ().
    end constructor.
    
    method public void FlattenXRefLinks(pJson as JsonObject, pParent as character, pLinks as ICollection):
        define variable cNames as character extent no-undo.
        define variable i as integer no-undo.
        define variable j as integer no-undo.
        define variable oIterator as IIterator no-undo.
        define variable oRelDef   as IRelationDefinition no-undo.
        define variable cFlattenList as character no-undo.
        
        if valid-object(pLinks) and pLinks:size > 0 then
        do:
            oIterator = pLinks:Iterator().
            if pParent > "" then
            do:
                do while oIterator:HasNext().
                    oRelDef = cast(oIterator:Next(),IRelationDefinition).
                   
                    if oReldef:ParentName = pParent 
                    and oReldef:Xref 
                    // todo no need to check for this since it is inherited, but need a trest 
                    or (type-of(oRelDef,IRelation) and cast(oReldef,IRelation):Xref) then
                    do: 
                        cFlattenList = cFlattenList 
                                     + (if cFlattenList = "" then "" else ",")
                                     + oReldef:PublicName.   
                    end. 
                end.
            
                     cNames = pjson:GetNames().
                    do i = 1 to extent(cNames):
                         
                         if pjson:GetType(cNames[i]) = JsonDataType:Array 
                         or pjson:GetType(cNames[i]) = JsonDataType:Object then
                         do:
                             if cFlattenList > "" and lookup(cNames[i],cFlattenList) > 0 then
                             do:
                                 ReplaceXrefWithEntity(pjson, cNames[i]).
                             end.
                             else if pjson:GetType(cNames[i]) = JsonDataType:Array then
                             do:
                                
                                 do j = 1 to pjson:GetJsonArray(cNames[i]):Length:
                                     if pjson:GetJsonArray(cNames[i]):GetType(j) = JsonDataType:OBJECT then 
                                         FlattenXRefLinks(pjson:GetJsonArray(cNames[i]):GetJsonObject(j),cNames[i],pLinks).
                                 end.                        
                             end.
                             else  
                             do:
                                 FlattenXRefLinks(pJson:GetJsonObject(cNames[i]),cNames[i],pLinks).   
                             end.
                         end. 
                    end.
                
            end.
/*            else do:                                                                                                   */
/*                cNames = pJson:GetNames().                                                                             */
/*                do i = 1 to extent(cNames):                                                                            */
/*                    if pjson:GetType(cNames[i]) = JsonDataType:Array                                                   */
/*                    or pjson:GetType(cNames[i]) = JsonDataType:Object then                                             */
/*                    do:                                                                                                */
/*                        if pjson:GetType(cNames[i]) = JsonDataType:Array then                                          */
/*                        do:                                                                                            */
/*                             do j = 1 to pjson:GetJsonArray(cNames[i]):Length:                                         */
/*                                 if pjson:GetJsonArray(cNames[i]):GetType(j) = JsonDataType:OBJECT then                */
/*                                     FlattenXRefLinks(pjson:GetJsonArray(cNames[i]):GetJsonObject(j),cNames[i],pLinks).*/
/*                             end.                                                                                      */
/*                         end.                                                                                          */
/*                         else                                                                                          */
/*                         do:                                                                                           */
/*                             FlattenXRefLinks(pJson:GetJsonObject(cNames[i]),cNames[i],pLinks).                        */
/*                         end.                                                                                          */
/*                     end.                                                                                              */
/*                end.                                                                                                   */
/*            end.                                                                                                       */
        end.
    end method.     
   
    method protected void ReplaceXrefWithEntity(pParentJson as JsonObject, pcSerializename as character):
        define variable oEntityJson    as JsonObject no-undo. 
        define variable cNames         as character  extent no-undo.
        define variable oNewJson       as JsonObject no-undo.
        define variable oNewJsonArray  as JsonArray  no-undo.
        define variable oOldJson       as JsonObject no-undo.
        define variable oOldJsonArray  as JsonArray  no-undo.
        define variable iExtent        as integer no-undo.
        define variable iName          as integer no-undo.
        if pParentJson:GetType(pcSerializename) = JsonDataType:ARRAY then
        do:
            oNewJsonArray = new JsonArray().
            oOldJsonArray = pParentJson:GetJsonArray(pcSerializename).
            do iExtent = 1 to oOldJsonArray:Length:     
                oOldJson = oOldJsonArray:GetJsonObject(iExtent).
                if oOldjson:Has(pcSerializename) then 
                do:
                    if oOldJson:GetType(pcSerializename) = JsonDataType:ARRAY then
                    do:
                       if oOldJson:GetJsonArray(pcSerializename):length = 1 then 
                           oNewJson = oOldJson:GetJsonArray(pcSerializename):GetJsonObject(1). 
                       else if oOldJson:GetJsonArray(pcSerializename):length = 0 then 
                           oNewJson = ?.
                       else         
                           undo, throw new IllegalStateError(subst("The '&1' entity at the the end of the XRef relation cannot have more than one row",pcSerializename)).
                    end.
                    else 
                       oNewJson = oOldJson:GetJsonObject(pcSerializename).
                       
                    if oNewJson <> ? then 
                        oNewJsonArray:Add(oNewjson).
                end.
            end.     
            pParentJson:set(pcSerializename,oNewJsonArray).     
        end.    
        else do:
            oOldJson = pParentJson:GetJsonObject(pcSerializename).
            if oOldJson:Has(pcSerializename) then
            do:
                if oOldJson:GetType(pcSerializename) = JsonDataType:ARRAY then
                do:
                   if oOldJson:GetJsonArray(pcSerializename):length = 1 then 
                       oNewJson = oOldJson:GetJsonArray(pcSerializename):GetJsonObject(1). 
                   else if oOldJson:GetJsonArray(pcSerializename):length = 0 then 
                       oNewJson = ?.    
                   else      
                       undo, throw new IllegalStateError(subst("The '&1' entity at the the end of the XRef relation cannot have more than one row",pcSerializename)).
                end.
                else 
                   oNewJson = oOldJson:GetJsonObject(pcSerializename).
                
                if oNewJson = ? then 
                    pParentJson:setNull(pcSerializename).
                else
                    pParentJson:set(pcSerializename,oNewJson).    
            end.
            else
               pParentJson:SetNull(pcSerializename).
        end.
        
    end method.      
      
end class.