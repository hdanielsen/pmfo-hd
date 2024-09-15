 
 /*------------------------------------------------------------------------
    File        : JsonLookupTableReadHandler
    Purpose     : Manage json posted with lookup data 
                  This is typically data fetched with expand and then posted as-is for validation and operations
                  that typcially are not saved to db. 
                  This handler preserves the state of posted lookup data for reserialization/response by: 
                  - creating records only once  
                  - define relations as many to one 
                  - define fields not in json as serialize-hidden   
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 21 02:26:15 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.BusinessLogic.IJsonExtendedReadHandler from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.
using Pmfo.Core.BusinessLogic.RelationDefinition from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.JsonReader from propath.
using Pmfo.Util.JsonUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using OpenEdge.Core.Assert from propath.

class Pmfo.Core.Common.JsonLookupTableReadHandler implements IJsonExtendedReadHandler: 
    
    // uses only one of these
    define protected property MainEntities as character no-undo get. set.
    define protected property LookupEntities as character no-undo get. set.
    define protected property NullList as character no-undo get. set.
    
    // the BE action method should push this to ClientUpdateRequest:Relations (may be improved and/or moved)  
    define public property Links as  ICollection no-undo 
        get():
            if not valid-object(Links) then 
                Links = new Collection().
            return Links.    
        end.    
        protected set.
        
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public JsonLookupTableReadHandler (pcEntities as character  ):
        this-object(pcEntities,false).
    end constructor.

    constructor public JsonLookupTableReadHandler (pcEntities as character, plAreLookups as logical ):
        define variable i as integer no-undo.
        super ().
        Assert:NotEmpty(pcEntities,"Entities").
      //  Assert:NotNull(plAreLookups,"AreLookups").
        
        if plAreLookups = false then      
            MainEntities = pcEntities.
        else 
            LookupEntities = pcEntities.
    end constructor.
    
    method public logical CheckReadOnlyAndPrepareBuffer( input phBuffer as handle, input pState as RowStateEnum ):
        return IsLookupEntity(phBuffer).
    end method.
    
    method protected logical IsLookupEntity(phBuffer as handle):
        if MainEntities > "" then
            return lookup(phBuffer:serialize-name,MainEntities) = 0.  
        else
            return lookup(phBuffer:serialize-name,LookupEntities) > 0.  
        
    end method.    
    // called with buffer released and not avail 
    //  
    // return true to signal that the JsonReader should skip this row 
    // the pjson may be ? if this is a lookup woht no data
    method public logical CheckOrSkipRow(pJson as JsonObject, phBuffer as handle, pReader as JsonReader).
        define variable cParentfields as character no-undo.
        define variable cChildfields as character   no-undo.
        define variable i as integer no-undo.
        define variable oRel as IRelationDefinition  no-undo.
        define variable cPropNames  as character   no-undo.
        define variable hFld as handle no-undo.     
        define variable cQualName  as character no-undo.  
        
        if IsLookupEntity(phBuffer) then
        do:
            
            if valid-handle(phBuffer:parent-relation) then 
            do: 
                // no json means null object for no record
                if valid-object(pJson) then 
                do:
                    phbuffer:find-unique (subst("where &1",BufferUtil:GetJoinExpression(phBuffer:parent-relation))) no-error.
                    cChildFields  = BufferUtil:GetChildJoinFields(phBuffer:parent-relation).
                    if not phBuffer:avail then
                    do:
                        phbuffer:buffer-create.
                        cParentFields = BufferUtil:GetParentJoinFields(phBuffer:parent-relation).

                        do i = 1 to num-entries(cParentFields):
                            phBuffer:buffer-field(entry(i,cChildfields)):buffer-value
                                  = phBuffer:parent-relation:parent-buffer:buffer-field(entry(i,cParentFields)):buffer-value.
                        end.
                    end.
                    
                end.
                oRel = new RelationDefinition(phBuffer:parent-relation:parent-buffer:serialize-name,phbuffer:serialize-name,RelationTypeEnum:ManyToOne). 
                // for nullist
                cQualname = subst("&1.&2",phBuffer:parent-relation:parent-buffer:name,phbuffer:name).
                
                if not Links:Contains(oRel) then
                do:
                    Links:Add(oRel).
                    // if object hide fields not in data
                    if valid-object(pJson) then 
                        HideFields(pJson,phbuffer).
                    else // else keep track to check if any other of the relation has data
                        NullList = StringUtil:AppendEntry(Nulllist,cQualname).     
                end.
                // if we got an object that previously was added to nullist hide fields not in data
                else if valid-object(pJson) and lookup(cQualname,Nulllist) > 0 then 
                do:
                    HideFields(pJson,phbuffer).
                    NullList = StringUtil:RemoveEntryFromList(cQualname,Nulllist).
                end.
                if valid-object(pJson) then
                do i = 1 to phbuffer:num-fields: 
                    hFld = phBuffer:buffer-field(i).
                    if lookup(Hfld:serialize-name,cChildfields) = 0 
                    and pJson:has(Hfld:serialize-name) then
                        pReader:AssignField(pjson, hfld).
                end.        
                    
            end.       
            return true.
        end.    
        return false.   
    end method.
    
    method protected void HideFields(pJson as JsonObject,phBuffer as handle): 
        define variable cPropnames as character no-undo.
        define variable i as integer no-undo.
         // does not matter if this is exact as long as it becomes an object in json
        cpropNames = Array:ToString(pJson:GetNames()).
        do i = 1 to phBuffer:num-fields: 
            if lookup(phBuffer:buffer-field(i):serialize-name,cPropNames) = 0 then
                phBuffer:buffer-field(i):serialize-hidden = true. 
                
        end.
    end method.
    
    method public void HandleFieldNotInJson( input pjson as JsonObject, input phfld as handle ):
        
    end method.

    method public logical ReadField( input pjson as JsonObject, input phfld as handle ):
        return false.
    end method.

end class.