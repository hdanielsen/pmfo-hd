 
 /*------------------------------------------------------------------------
    File        : JsonTrackSequenceHandler
    Purpose     : Use to track order of load to json  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Jul 16 10:06:55 EDT 2021
    Notes       : Adds rows for the enity to the TrackLoadOrderAction:LoadSequences List
                  to allow the BE to read the order           
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.IJsonReadHandler from propath.
using Pmfo.Core.BusinessLogic.ITrackLoadOrderAction from propath.
using Pmfo.Core.Common.FieldValue from propath.
using Pmfo.Core.Common.IFieldValue from propath.
using Pmfo.Core.Common.Key from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
 

class Pmfo.Core.Common.JsonTrackSequenceHandler implements IJsonReadHandler: 
    define protected property TrackAction as ITrackLoadOrderAction no-undo get. set.    
    define protected property Entity      as character             no-undo get. set.    
    define protected property KeyList     as character             no-undo get. set.    
    define protected property FieldValues as IFieldValue extent    no-undo get. set.
    define variable JoinedKeys as character no-undo.
    
    define private variable KeyCount as integer no-undo.
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    constructor public JsonTrackSequenceHandler (pTrackAction as ITrackLoadOrderAction, pcEntity as character, pcKeys as character):
        super ().
        TrackAction = pTrackAction.
        Entity = pcEntity.
        KeyList = pcKeys.
    end constructor.
        
    method public logical CheckReadOnlyAndPrepareBuffer( input phBuffer as handle, input pState as Pmfo.Core.BusinessLogic.RowStateEnum ):
        define variable cParentJoins as character no-undo.
        define variable cChildJoins as character no-undo.
        define variable i as integer no-undo.
        define variable IKey as integer no-undo.
        define variable hParentfield as handle no-undo.
        
        if phbuffer:serialize-name = Entity then
        do:
            if valid-handle(phbuffer:parent-relation) then 
            do:
                cParentJoins = BufferUtil:GetParentJoinFields(phbuffer:parent-relation).
                cChildJoins  = BufferUtil:GetChildJoinFields(phbuffer:parent-relation).
                extent(FieldValues) = ?. 
                extent(FieldValues) = num-entries(KeyList).
                KeyCount = 0.
                 
                do i = 1 to num-entries(cChildJoins).
                
                    iKey = lookup(entry(i,cChildJoins),KeyList).
                     
                    if iKey > 0 then
                    do: 
                        hParentField = phbuffer:parent-relation:parent-buffer:buffer-field(entry(iKey,cParentJoins)). 
                        FieldValues[IKey] = new FieldValue(hParentField:name,PrimitiveHolderUtil:GetHolder(hParentField)).  
                        KeyCount = KeyCount + 1.
                        JoinedKeys = JoinedKeys 
                                   + (if JoinedKeys = "" then "" else ",")
                                   + entry(i,cChildJoins).
                    end.              
                end. 
                // rather unlikely, so throw not implmented    
                if KeyCount = num-entries(KeyList) then 
                     undo, throw new NotImplementedError("JsonTrackSequenceHandler load of child with all key values present in parent").
                 
            end.   
            
        end.    
        return false.
    end method.

    method public void HandleFieldNotInJson( input pjson as Progress.Json.ObjectModel.JsonObject, input phfld as handle ):
    end method.
    
    // Read field ti use the order of the data in Json to deifne the new PsckSeq
    method public logical ReadField( input pjson as JsonObject, input phfld as handle ):
        define variable iKey as integer no-undo.
        
        if phFld:buffer-handle:serialize-name = Entity then
        do:
            // don't read if the field was loaded from parent join  
            if JoinedKeys = "" or lookup(phFld:name,JoinedKeys) = 0 then
                iKey =  lookup(phFld:name,KeyList). 
            
            if iKey > 0 then 
            do: 
                
                if extent(FieldValues) = ? then
                do: 
                    extent(FieldValues) = num-entries(KeyList). 
                    KeyCount = 0.
                end.
                   
                case phFld:data-type:
                    when "character" then
                        phFld:buffer-value = pjson:GetCharacter(phFld:serialize-name).
                    when "longchar" or when "clob" then
                        phFld:buffer-value = pjson:GetLongchar(phFld:serialize-name).
                    when "blob" then
                        phFld:buffer-value = base64-decode(pjson:GetLongchar(phFld:serialize-name)).
                    when "int64" then
                        phFld:buffer-value = pjson:GetInt64(phFld:serialize-name).
                    when "integer" then
                        phFld:buffer-value = pjson:GetInteger(phFld:serialize-name).
                    when "decimal" then
                        phFld:buffer-value = pjson:GetDecimal(phFld:serialize-name).
                    when "logical" then
                        phFld:buffer-value = pjson:GetLogical(phFld:serialize-name).    
                    when "date" then
                        phFld:buffer-value = pjson:GetDate(phFld:serialize-name).    
                    when "datetime" then
                        phFld:buffer-value = pjson:GetDatetime(phFld:serialize-name).    
                    when "datetime-tz" then
                        phFld:buffer-value = pjson:GetDatetimeTZ(phFld:serialize-name).    
                        
                end case.
                
                FieldValues[iKey] = new FieldValue(phFld:name,PrimitiveHolderUtil:GetHolder(phFld)).
                KeyCount = KeyCount + 1. 
             
                if KeyCount = extent(FieldValues) then 
                do: 
                    TrackAction:LoadSequences:Add(new Key(FieldValues)). 
                    if JoinedKeys = "" then 
                        extent(FieldValues) = ?.
                    else // if values from join then reset KeyCount to include the joined keys 
                        KeyCount = num-entries(JoinedKeys).    
                end.
                return true.        
            end.
        end.
        return false.
    end method.
    
    
end class.