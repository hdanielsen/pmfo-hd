 
 /*------------------------------------------------------------------------
    File        : DatasetRouter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Sep 01 23:59:03 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IMap from propath.
using Pmfo.Core.BusinessLogic.IDatasetRelation from propath.
using Pmfo.Core.BusinessLogic.IDynamicRelation from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.Router.RequestRouter from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Util.TableUtil from propath.
 

class Pmfo.Core.BusinessLogic.Router.LinkRouter inherits RequestRouter: 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
   
    constructor public LinkRouter (pDataset as handle, pLinks as ICollection, pActions as IMap, pFunctions as IMap, pFixedChildren as character):
        super (pDataset, pLinks, pActions , pFunctions, pFixedChildren).
    end constructor.
    
/*    method protected override logical AddTopBuffers(phHandle as handle):                                                                                   */
/*        // the request builder would need to be improved to handle dataset ot resource segments in the URI                                                 */
/*        undo, throw new UnsupportedOperationError(subst("Link to Entity '&1' that does not have a single top table with same serialize-name as dataset.",).*/
/*    end method.                                                                                                                                            */
    
    method public handle ResolveRequest (pNewDataset as handle, pRelation as IRelation, piPosition as integer, pClientRequest as IClientRequest,  pRowState as RowStateEnum, pLinks as ICollection, output pMethod as IMethod ):
        
        define variable hBuffer            as handle no-undo.
        define variable hSourcebuffer      as handle no-undo.
        define variable hTable             as handle no-undo.
        define variable hSourceRelation    as handle no-undo.
        define variable hChildbuffer       as handle no-undo.
        
        define variable cSourceName        as character no-undo.
        define variable cNewSerializeName  as character no-undo.
        define variable cNewBufferName     as character no-undo.
        define variable hLinkRelation      as handle no-undo.
        assign
            cSourcename       = pRelation:name
            cNewSerializeName = pRelation:PublicName.
        
        // try to keep table name matching dataset relation if it exists    
        if type-of(pRelation,IDatasetRelation) then
        do: 
            // make sure it is valid - old code did copy these into scope where the dataset no longer exists
            hLinkRelation = cast(pRelation,IDatasetRelation):RelationHandle.
            if valid-handle(hLinkRelation) then 
                cNewBufferName = hLinkRelation:child-buffer:name.
        end.         
        
        hSourcebuffer = DataSetUtil:GetTopBufferBySerializeName(this-object:Dataset,cSourcename).
        // if not valid dataset relation use source buffer name - code exists that uses dataset:get-buffer-handle (tablename)
        if cNewBufferName = "" then 
            cNewBufferName = hSourceBuffer:name.
        
        if type-of(pRelation,IDynamicRelation) then 
        do:
            hTable = CreateTableAndForeignFields(hSourcebuffer, cast(pRelation,IDynamicRelation), pNewDataset).
        end.
        else         
            hTable = TableUtil:CreateTableLike(hSourcebuffer).
        
        create buffer hBuffer for table hTable buffer-name cNewBufferName.
        hBuffer:serialize-name = cNewSerializeName.
        pNewDataset:add-buffer(hBuffer).
        
        if extent(pClientRequest:Collections) > piPosition then
        do:
            CreateMembers(pRowState,pClientRequest:Collections[piPosition + 1]).
            find ttMember where ttMember.key = pClientRequest:Collections[piPosition + 1] no-error.
            
            if not avail ttMember then
                undo, throw new NotFoundError(subst("Buffer, &1 or Link '&2'",if pRowState = RowStateEnum:None then "Function" else "Action",pClientRequest:Collections[piPosition + 1]),subst("Resource or dataset '&1'",pClientRequest:Collections[piPosition])).
            
            if valid-handle(ttMember.memberHandle) then
            do:
                 
                hSourceRelation = ttMember.memberHandle:parent-relation.
                hChildbuffer = CreateChildTable (pNewdataset,hBuffer, hSourceRelation).
                if extent(pClientRequest:Collections) = piPosition + 1 then
                do:
                    // @TODO deactivate links before createChilren ? seems likely  
                    // note added when adding Createchildren for method
                    CreateChildren (pNewdataset,hBuffer,ttMember.memberHandle).
                end.    
                else  
                    ResolveSubRequest (pNewDataset, hChildbuffer,hSourceRelation:child-buffer, piPosition + 1, pClientRequest, pRowState, pLinks, output pMethod ).
            end.
            else if valid-object(ttMember.memberObject) then
            do:
                if type-of(ttMember.memberObject,IMethod) then
                do:
                    pMethod = cast(ttMember.memberObject,IMethod).
                    // links in BE is defined wioth resource as parent so use table serialize-name
                    DeactivateDatasetLinks(hBuffer:table-handle:serialize-name, pRowState,pMethod).
                    CreateChildren(pNewdataset,hBuffer, hSourcebuffer).
                end.
                else do:
                    ResolveLinkAndCreateTables(pNewDataset, hBuffer, piPosition + 1, pClientRequest, cast(ttMember.memberObject,IRelation), pRowState, pLinks, output pMethod). 
                end.       
            end. 
        end.
        else do:
            // links in BE is defined wioth resource as parent so use table serialize-name
            DeactivateDatasetLinks(hBuffer:table-handle:serialize-name, pRowState,?).
            CreateChildren (pNewdataset,hBuffer,hSourcebuffer).
        end.
        
        return hBuffer.
/*        catch e as Error :                                                                                                                                           */
/*            // buffers added to the dataset is deleted with the dataset, but                                                                                         */
/*            // delete in case the error happened while current buffer was not yet added                                                                              */
/*                                                                                                                                                                     */
/*                                                                                                                                                                     */
/*            undo, throw new ApplicationError(subst("Failed to create dataset for link from '&1' to '&2'",pcCollections[piPosition - 1],pcCollections[piPosition]),e).*/
/*        end catch.                                                                                                                                                   */
/*                                                                                                                                                                     */
    end method.    
    
    method protected handle CreateTableAndForeignFields(phSourcebuffer as handle, pRelation as IDynamicRelation, phDataset as handle):
         define variable hParent as handle no-undo.
         define variable cKeys as character no-undo.
         define variable cKeyArray as character extent no-undo.
         define variable hCopyFields as handle extent no-undo.
         define variable i as integer no-undo.
         define variable cfieldPairs as character no-undo.
         define variable cParentFields as character extent no-undo.
         define variable cChildFields as character extent no-undo.
         define variable hTable as handle no-undo.
         hParent = DatasetUtil:GetBufferBySerializeName(phDataset, pRelation:parentname).
         
         cKeys = ServiceInfo:GetKeys(hParent).
         cKeyArray = Array:Split(cKeys).
         extent(hCopyFields) = extent(cKeyArray).
         do i = 1 to extent(hCopyFields): 
             hCopyFields[i] = hParent:buffer-field(cKeyArray[i]).
         end.      
         hTable = TableUtil:CreateTableLike(phSourceBuffer, phSourceBuffer:name, hCopyFields, true, output cfieldPairs).
         if pRelation:PairsList() <> cFieldPairs then
         do: 
              extent(cParentFields) = integer(num-entries(cFieldPairs) / 2).
              extent(cChildFields) = extent(cParentFields).
              do i = 1 to extent(cParentFields) by 2:
                  cParentFields[i] = entry(i,cFieldPairs).
                  cChildFields[i] = entry(i + 1,cFieldPairs).
              end.
              pRelation:CreateJoinPredicates(cParentFields, cChildFields).
             
         end.     
         return hTable. 
    end method.    
    
    method protected void ResolveSubRequest (pNewDataset as handle, phParent as handle, phSourceParent as handle, piStartPosition as integer, pClientRequest as IClientRequest,  pRowState as RowStateEnum, pLinks as ICollection, output pMethod as IMethod ):
        define variable iCollection   as integer   no-undo.
        define variable hSourceParent as handle    no-undo.
        define variable hSourceChild  as handle    no-undo.
        define variable hBuffer       as handle    no-undo.
        define variable hParentBuffer as handle    no-undo.
        define variable hPrevParentBuffer as handle no-undo.
        define variable oRelation     as IRelation no-undo.
         
        hParentBuffer = phParent.
        hSourceParent = phSourceParent.
        do iCollection = piStartPosition to extent(pClientRequest:Collections):
            hSourceChild = GetChildBufferBySerializeName(phSourceParent, pClientRequest:Collections[iCollection]).
            
            if valid-handle(hSourceChild) then
            do:
                hBuffer = CreateChildTable (pNewdataset,hBuffer, hSourceChild:parent-relation).
            end.     
            else 
            do:
                oRelation = GetLink(pClientRequest:Collections[iCollection - 1], pClientRequest:Collections[iCollection]).
                if valid-object(oRelation) then
                do:
                    ResolveLinkAndCreateTables(pNewDataset, hParentBuffer, iCollection + 1, pClientRequest, oRelation, pRowState, pLinks, output pMethod). 
                    return.
                end.
                
            end.
            if not valid-handle(hBuffer) and iCollection > piStartPosition then 
            do: 
                 /* Try to passthru to the parent buffer using previous parent */
                 pLinks = new Collection().
                 oRelation = CreateRelation(hSourceParent:parent-relation,pRowState).
                 ResolveLinkAndCreateTables(pNewDataset, hParentBuffer:parent-relation:parent-buffer, iCollection, pClientRequest, oRelation, pRowState, pLinks, output pMethod). 
                 
            end.
            
            // we are not looking fo rmethod here, but this is where 
            if not valid-handle(hSourceChild) then
                undo, throw new NotFoundError(
                                              // parameter 1
                                              subst("Buffer, &1 or Link '&2'",if pRowState = RowStateEnum:None then "Function" else "Action",pClientRequest:Collections[iCollection]),
                                              // parameter 2
                                              subst("Buffer '&1'",pClientRequest:Collections[iCollection - 1])
                                              ).
            hParentBuffer = hBuffer.
            hSourceParent = hSourceChild.
        end.
        
        CreateChildren (pNewdataset,hParentBuffer,hSourceParent).
    end method.    
    
    method protected void CreateChildren (pNewdataset as handle,phNewParent as handle, phOriginalParent as handle):
        define variable i as integer no-undo.
        define variable hSourceRelation       as handle no-undo.
        define variable hbuffer as handle no-undo.
        do i = 1 to phOriginalParent:num-child-relations:
            
            hSourceRelation       = phOriginalParent:get-child-relation (i).
            if hSOurceRelation:recursive = false then
            do:
                hBuffer = CreateChildTable(pNewDataset,phNewParent,hSourceRelation).
                CreateChildren(pNewDataset,hBuffer,hSourceRelation:child-buffer). 
            end.
        end.
    end method. 
    
    method protected handle CreateChildTable (pNewdataset as handle,phNewParent as handle, phOriginalRelation as handle):
        define variable hOriginalBuffer as handle no-undo.
        define variable hParentBuffer   as handle no-undo.
        define variable hBuffer         as handle no-undo.
        
        define variable hNewTable       as handle no-undo.
        define variable hNewRelation as handle no-undo.
        
        hOriginalBuffer = phOriginalRelation:child-buffer.
        hNewTable = TableUtil:CreateTableLike(hOriginalBuffer). 
        
        create buffer hBuffer for table hNewTable buffer-name hOriginalBuffer:table-handle:name.   
        hBuffer:serialize-name =  hOriginalBuffer:serialize-name.
        
        pNewdataset:add-buffer(hBuffer).
        hNewRelation = pNewDataset:add-relation(phNewParent,hBuffer,phOriginalRelation:relation-fields,phOriginalRelation:reposition,phOriginalRelation:nested).
        hNewRelation:foreign-key-hidden = phOriginalRelation:foreign-key-hidden.
        return hBuffer.
        catch e as Progress.Lang.Error :
            if valid-handle(hNewTable) then 
                delete object hNewTable.     
            undo, throw e.     
        end catch.
    end method.
    
       
    
          
end class.