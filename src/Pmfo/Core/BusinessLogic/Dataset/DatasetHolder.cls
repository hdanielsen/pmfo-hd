 
 /*------------------------------------------------------------------------
    File        : DatasetHolder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 14 15:29:55 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

 

block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.Collection from propath.
using Pmfo.Core.BusinessLogic.Dataset.IDatasetHolder from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.StringUtil from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.BusinessLogic.RelationDefinition from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.

class Pmfo.Core.BusinessLogic.Dataset.DatasetHolder implements IDatasetHolder: 
    
    define public    property Dataset         as handle no-undo get. protected set.
    define protected property Links           as ICollection no-undo get. set.
    
    define protected property ServiceManager as IServiceManager no-undo
        get():
            return cast(Application:ServiceManager,IServiceManager).
        end get.
        
    constructor public DatasetHolder (phClientDataset as handle):
        super ().
        this-object:Dataset = phClientDataset.
    end constructor.
    
    
    // TODO.. ideally the relation added for missing expadns could be done later when the BEs have 
    // been started are started in DatasetGetManager, but the LinRouter that adds these for Expand 
    // suffers from the same problem -
    method public void AddLinksAndSelects(pBe as IBusinessEntity,pClientRequest as IClientGetRequest ):
        define variable cExpand       as character no-undo.
        define variable iExpand       as integer no-undo. 
        define variable cExpandEntry  as character no-undo.
        define variable cExpandSelect as character no-undo.
        define variable hBuffer       as handle no-undo.
        define variable cProcessed    as character no-undo.
        define variable i             as integer no-undo.
        define variable iChild        as integer no-undo.
        define variable oBe           as IBusinessEntity  no-undo.
        define variable oRelation     as IRelationDefinition  no-undo. 
        define variable hRelation     as handle           no-undo.
        cExpand = pClientRequest:Expand.  
      
        if cExpand > "" then
        do:
            // grab select from expands and apply to the client dataset (LinkRouter has added them ot the server dataset)
            do iExpand = 1 to num-entries(cExpand): 
                cExpandEntry = entry(iExpand,cExpand).
                hBuffer =  DatasetUtil:GetBufferByPathName(this-object:Dataset,subst("&1/&2",pClientRequest:Collections[1],cExpandEntry)).
                if not valid-handle(hBuffer) then 
                    // todo  - this is not always a function...?? although the  holder is typically used to share dataset
                    // across BEs and functions () 
                    undo, throw new UnsupportedOperationError(subst("Expand '&1' for &2 Function",
                                                                    cExpandEntry,
                                                                    pClientRequest:Collections[extent(pClientRequest:Collections)])). 
                cExpandSelect = cast(pClientRequest,IClientGetRequest):GetExpandSelect(cExpandEntry).
                if cExpandSelect > "" then
                    BufferUtil:SetVisibleFields(hBuffer, cExpandSelect).
                cProcessed = StringUtil:AppendEntry(cProcessed,hBuffer:table-handle:name). 
            end.     
        end.
        do i = 1 to this-object:Dataset:num-buffers: 
            hBuffer = this-object:Dataset:get-buffer-handle(i).
            if hBuffer:table-handle:serialize-name = pBe:Resource then
                oBe = pBE.
            else   
                oBe = ?.
            // add missing relation for the client dataset to the clientrequest    
            do iChild = 1 to hBuffer:num-child-relations: 
                hRelation = hBuffer:get-child-relation(iChild).
                if lookup(hRelation:child-buffer:table-handle:name,cProcessed) = 0 then 
                do:
                    if not valid-object(oBe) then
                        oBe = this-object:ServiceManager:CreateBusinessEntity(hBuffer:table-handle:serialize-name).
                    
                    oRelation = GetClientLink(hBuffer:table-handle:serialize-name,hRelation:child-buffer:serialize-name).
                    if not valid-object(oRelation) then
                        oRelation = oBe:GetLink(hRelation:child-buffer:serialize-name).
                    
                    if valid-object(oRelation) then 
                    do: 
                        if not valid-object(pClientRequest:ExpandedRelations) then 
                            pClientRequest:ExpandedRelations = new Collection(). 
                        pClientRequest:ExpandedRelations:Add(oRelation).   
                    end.    
                end.
            end. 
        end.
    end method.    
    
    method protected void AddClientLink(pcParent as character, pcPublicChild as character,pRelationType as RelationTypeEnum):
        if valid-object(Links) = false then 
            Links = new Collection().
        else if valid-object(GetClientLink(pcParent,pcPublicChild)) then 
            undo, throw new IllegalArgumentError(subst("Link from &1 to &2 is already defined",pcParent,pcPublicChild)).
                
        Links:Add(new RelationDefinition(pcParent,pcPublicChild,pRelationType)).      
    end method.    
    
    method public IRelationDefinition GetClientLink(pcParentName as character, pcPublicName as character):
        define variable oIterator as IIterator no-undo.
        define variable oRel      as IRelationDefinition no-undo.
        if valid-object(Links) then
        do: 
            oIterator = Links:Iterator().
            do while oIterator:HasNext():
               oRel = cast(oIterator:Next(),IRelationDefinition).
               if  oRel:ParentName = pcParentName 
               and oRel:PublicName = pcPublicName then
                   return oRel. 
            end.    
        end.
        return ?.       
    end method.    
    
    // allow deep copy (only for resue of main defs - if you need both keep the class alivee)
    method public void GetDataset(output dataset-handle phDataset).
        phDataset = this-object:Dataset.
        finally:
            delete object phDataset.        
        end finally.
    end.
    
end class.