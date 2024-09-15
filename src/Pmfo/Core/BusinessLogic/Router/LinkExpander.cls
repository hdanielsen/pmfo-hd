 
 /*------------------------------------------------------------------------
    File        : LinkExpander
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Oct 30 01:58:20 EDT 2020
    Notes       : AssertExpand throws BadRequestError
  ----------------------------------------------------------------------*/
 

block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.
using Pmfo.Core.BusinessLogic.RelationDefinition from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.TableUtil from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Util.BufferUtil from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Util.StringUtil from propath.
 
class Pmfo.Core.BusinessLogic.Router.LinkExpander:
     
    define protected property ServiceManager as IServiceManager no-undo 
         get():
             return  cast(Application:ServiceManager,IServiceManager).
         end.  
    
    define protected property Expand as character no-undo get. set. 
    
    // as of current we throw bad request from assert as datasetRouter will throw them as-is 
    method public void AssertExpand(pcCollections as character extent,pcExpand as character):
        define variable i as integer no-undo.
        define variable j as integer no-undo.
        define variable iEntity      as integer no-undo. 
        define variable iOtherEntity as integer no-undo. 
        define variable cThisentry as character no-undo.
        define variable cOtherentry as character no-undo.
        define variable cThisentity as character no-undo.
        define variable cColectionList as character no-undo.
        
        Expand = pcExpand.
        
        cColectionList = Array:ToString(pcCollections).
        do i = 1 to num-entries(pcExpand):
            cThisentry = entry(i,pcExpand).
            if cthisentry = "" then 
            do:
                if i = num-entries(pcExpand) then
                    undo, throw new BadRequestError(subst("Invalid comma found at end of 'expand=&1'",pcExpand)).
                if i = 1 then
                    undo, throw new BadRequestError(subst("Invalid comma found at beginning of 'expand=&1'",pcExpand)).
                
                undo, throw new BadRequestError(subst("Entry &1 in 'expand=&2' is blank",i,pcExpand)).
                
            end.     
            do j = 1 to num-entries(pcExpand):
                if j <> i then
                do:
                    cOtherentry = entry(j,pcExpand).
                  
                    if cOtherEntry = cthisentry then
                         undo, throw new BadRequestError(subst("Duplicate expand entries for &1",cOtherEntry)).
                    // if this is shorter and we are at the last entry and it begins with the same as another
                    // then tit is redundat  
                    // we need thios for select
/*                    if num-entries(cThisEntry,"/") lt num-entries(cOtherEntry,"/")                                                                       */
/*                    and cOtherEntry begins cThisEntry + "/" then                                                                                         */
/*                        undo, throw new BadRequestError(subst("Expand entry '&1' is redundant when expand of '&2' is specified",cThisEntry,cOtherEntry)).*/
                    
                    if num-entries(cThisEntry,"/") > 1 then 
                    do iEntity = 2 to num-entries(cThisEntry,"/"):
                        cthisentity = entry(iEntity,cThisEntry,"/").
                        if lookup(cthisentity,cColectionList) > 0 then 
                            undo, throw new BadRequestError(subst("Cannot expand entity '&1' when the same entity is in the URL",cthisentity)).
                        do iOtherEntity = 1 to num-entries(cOtherEntry,"/"):
                            // if the entity is in another 
/*                            if cThisentity = entry(iOtherEntity,cOtherEntry,"/") then                                                                                                                        */
/*                            do:                                                                                                                                                                              */
/*                                // if different number or different parent then this is not valid                                                                                                            */
/*                                // since we start with iEntity 2 the (entry( - 1 should be safe for both (if iotherentry is 1 then the numbers wont match                                                    */
/*                                if iEntity <> iOtherEntity                                                                                                                                                   */
/*                                or (entry(iEntity - 1,cThisEntry,"/") <> entry(iOtherEntity - 1,cOtherEntry,"/")) then                                                                                       */
/*                                undo, throw new BadRequestError(subst("Entity '&1' is referenced in expand entries '&2' and '&3'. An entity can only be expanded once.",cThisEntity,cThisEntry,cOtherEntry)).*/
/*                            end.                                                                                                                                                                             */
                        end.    
                    end.         
                end.
            end.
        end.
        
    end method.    
    
    method protected ICollection SeparateClientRequests(pClientRequest as IClientGetRequest, input-output pcExpand as character):
        define variable oRequest as IGetDataRequest no-undo.
        define variable i as integer no-undo. 
        define variable cExpandEntry as character no-undo.
        define variable cParams      as character no-undo.
        define variable iLeft        as integer no-undo.
        define variable iRight       as integer no-undo.
        
        do i = 1 to num-entries(pcExpand):
            cExpandEntry = entry(i,pcExpand).
            iLeft  = index(cExpandEntry,"(").
            iRight = index(cExpandEntry,")").
            if iLeft > 0 and iRight > 0 and iRight = length(cExpandEntry) then 
            do:
                entry(i,pcExpand) = substring(cExpandEntry,1,iLeft - 1). 
                cParams = substring(cExpandEntry,iLeft + 1,iRight - 2).
                
            end.    
            else if iRight > 0 then
                undo, throw new BadRequestError(subst("Expand entry '&1' has an invalid rigth parenthesis",cExpandEntry)) .
            else if iLeft > 0 then
                undo, throw new BadRequestError(subst("Expand entry '&1' is missing a right parenthesis",cExpandEntry)) .   
/*            if iLeftParenthesis > 0  then                                                                                         */
/*            do:                                                                                                                   */
/*               if index(cUriSegments[i],")",iLeftParenthesis) = 0 then                                                            */
/*                   undo, throw new BadRequestError(subst("The URI segment '&1' is missing a rigth parenthesis",cUriSegments[i])) .*/
/*               KeyWheres[i]   = StringUtil:ExtractParenthesis(cUriSegments[i], true).                                             */
/*               Collections[i] = substr(cUriSegments[i],1,iLeftParenthesis - 1).                                                   */
/*            end.                                                                                                                  */
        end.          
              
    end method.
             
    method public handle Expand(phDataset as handle, pcRequestedEntity as character, pcExpand as character, pClientRequest as IClientGetRequest, pLinks as ICollection  ):
        define variable oExpandLinks    as ICollection no-undo. 
        define variable hDataset        as handle no-undo.
        define variable hBuffer         as handle no-undo.
        define variable oBe             as IBusinessEntity no-undo.
        define variable hBeDataset      as handle no-undo.
        define variable lAllowManytoOne as logical no-undo.
        define variable cExpand         as character no-undo.
        define variable i               as integer no-undo.
        define variable hDeleteDataset  as handle no-undo.
        define variable iMaxStaticEntries  as integer no-undo. 
       //  SeparateClientRequests(pClientRequest,input-output pcExpand).
        
        AssertExpand(pClientRequest:Collections, pcExpand).
       
        // don't count action/function    
        if valid-object(pClientRequest:ServiceMethod) and pClientRequest:Collections[2] = pClientRequest:ServiceMethod:name then
            IMaxStaticentries = 2.
        else 
            IMaxStaticentries = 1.
        
        // first expand the dataset if the requested table is the top in the dataset
        // This will activate deactivated links also for cases that are not fullly resolved
        // fully expanded entries will be removed from the returned expand 
        // 
        if extent(pClientRequest:Collections) = IMaxStaticentries 
        and pClientRequest:Collections[1] = phDataset:serialize-name
        // not preapred to handle this in here- fully possible, but the only gaol here is to use the existing dataset
        // so it would need to be done effciently and links
        // TODO? get rid of this entirely since it adds complication also for select 
        //       or rewrite to a better way to check the relation versus the expand entries
        //       Everything works if this is not here! the differenc is that this may use the exiting statc dataset
        //       This will have many more relations and buffers so may have cost elsewhere      
        and HasXrefRelation(pLinks) = false
        then
        do:
            cExpand = ExpandDatasetLinks(pcRequestedEntity,phDataset,pClientRequest, pcExpand).
        end.
        else
            cExpand = pcExpand. 
         
        // if there are any entries left we need a new dataset and find the links in other BEs
        if cExpand > "" then
        do on error undo, throw:
            do i = 1 to extent(pClientRequest:Collections):
                if pClientRequest:Collections[i] = pcRequestedEntity then
                do:
                    lAllowManytoOne = pClientRequest:KeyWheres[i] <> "".
                end.    
            end.    
            
            oExpandLinks = CreateRelations(pcRequestedEntity,cExpand).
            pClientRequest:ExpandedRelations = oExpandLinks. // used to flatten many to one and one to one in JsonGetResponseProcessor
            
            hDataset = CopyDataset(phDataset,pcRequestedEntity, output hBuffer).
            if phDataset:dynamic then 
                hDeleteDataset = phDataset.
            
            // if there is parent we need to find links and dataset for the buffer 
            // this is currently called from DatasetRouter with Links for top BE
            // todo? move this to LinkRouter to be called when the linked table is resolved resolved there?? 
            if valid-handle(hBuffer:parent-relation) then
            do:
                oBe = cast(ServiceManager:CreateBusinessEntity(hBuffer:table-handle:serialize-name),IBusinessEntity) no-error.
                if not valid-object(oBe) then 
                     undo, throw new IllegalOperationError(subst("Child entity not found for relation from '&1' to '&2'",
                                                                 hBuffer:parent-relation:parent-buffer:serialize-name,
                                                                 pcRequestedEntity)). 
                pLinks = if oBe:HAsLink then oBe:Links else ?.  
                hBeDataset = oBe:LinkDataset.
                
            end.
            else 
                hBeDataset = phDataset.
             
            ProcessLinks(pcRequestedEntity, hDataset, hBuffer, oExpandLinks, hBeDataset, pLinks, lAllowManytoOne, pClientRequest).
            catch e as Progress.Lang.Error:
                if valid-object(hDataset) and hDataset:dynamic then 
                    delete object hDataset.
                undo, throw e.        
            end catch.
        end.
        else 
            hDataset = phDataset.
        
/*        define variable j as integer no-undo.                                     */
/*        define variable cc as character no-undo.                                  */
/*        do j = 1 to hdataset:num-buffers:                                         */
/*           cc = cc + subst("buffer &1",hdataset:get-buffer-handle(j):name) + "~n".*/
/*                                                                                  */
/*        end.                                                                      */
/*        message cc                                                                */
/*        view-as alert-box.                                                        */
        return hDataset.  
        catch e3 as IllegalOperationError :
            undo, throw e3.     
        end catch. 
        // errors from Assert can be used as-is
        catch e1 as BadRequestError :
            undo, throw e1.        
        end catch. 
        // show the full pcExpand param and stack for other errors         
        catch e2 as Progress.Lang.Error :
            undo, throw new BadRequestError(subst("&1 requested expand &2 or filter &3 '&4' &5 invalid",
                         if num-entries(cExpand) > 1 then "One or more of the" else "The",                                              
                         if num-entries(cExpand) > 1 then "entries" else "entry",
                         if num-entries(cExpand) > 1 then "qualifiers" else "qualifier",
                         cExpand,
                         if num-entries(cExpand) > 1 then "are" else "is"
                         ),
                         e2).    
        end catch.
        finally:
            if valid-handle(hDeleteDataset) then
                delete object hDeleteDataset.
        end finally.
    end method.
    
    method protected character ExpandDatasetLinks(pcRequestedentity as character,phDataset as handle, poCLientRequest as IClientGetRequest,  pcExpand as character):
        define variable hRelation as handle no-undo.
        define variable i as integer no-undo.
        do i = 1 to phDataset:num-relations:
            hRelation = phDataset:get-relation(i).
            if hRelation:parent-buffer:serialize-name = poCLientRequest:Collections[1] then 
                // returns links that were not expanded    
                pcExpand = ExpandLinks(pcRequestedentity,hRelation,pcExpand,poCLientRequest).
       
        end.
        return pcExpand.
    end method.
    
    method protected character ExpandLinks(pcRequestedentity as character,phRelation as handle,pcExpand as character, poCLientRequest as IClientGetRequest):
        define variable i          as integer   no-undo.
        define variable cChild     as character no-undo.
        define variable cExpand    as character no-undo.
        define variable lFound     as logical   no-undo.
        define variable cResolved  as character no-undo.
            
        do i = 1 to num-entries(pcExpand):
            cExpand = entry(i,pcExpand).
            
            cChild = entry(1,cExpand,"/").
            
            if cChild = phRelation:child-buffer:serialize-name then
            do:
                
                if ResolveExpandedChildLinks(phRelation:child-buffer,cExpand, 2) then
                do:
                    cResolved = cResolved
                              + (if cResolved = "" then "" else ",") 
                              + string(i).
                    
                    // if resolved and already active then the expand is not needed - throw error (Can be reconsidered as this may be too restrictive ) 
                    if phRelation:active then
                        undo, throw new IllegalOperationError(subst("Expand '&1' is invalid. The requested entit&2 included in the default response.",
                                                                     cExpand,
                                                                     if num-entries(cExpand,"/") > 1 then "ies are" else "y is")).
                
                end.
                lFound = true.     
            end. 
        end.
        // activate the relation if found 
        if lFound then
        do:
            message "activarted link" phRelation:parent-buffer:name phRelation:parent-buffer:serialize-name skip
                                      phRelation:child-buffer:name                                    phRelation:child-buffer:name
   
            view-as alert-box.
            phRelation:active = true.
            ProcessSelect(pcRequestedentity, phRelation:child-buffer, poCLientRequest). 
            
        end.
        
        // if fully resolved remove the entry             
        do i = 1 to num-entries(cResolved):
            entry(int(entry(i,cResolved)),pcExpand) = "".
        end.
        pcExpand = trim(replace(pcExpand,",,",","),",").
        
        return pcExpand.
    end method.    
    
    // called recursivelky for all children of an expanded link 
    // if there are child relations that matches the "/" delimited entry then the link is activated
    // if all "/" entries are found then the link is resolved     
    method protected logical ResolveExpandedChildLinks(phBuffer as handle,pcExpand as character,piLevel as integer):
        define variable i         as integer no-undo.
        define variable hRelation as handle no-undo.
        define variable lResolved as logical no-undo.   
        
        if phBuffer:num-child-relations = 0 then
        do:
            // if there are no entries at this level and no relation then this is resolved
            if num-entries(pcExpand,"/") < piLevel then
                lResolved = true.
        end.    
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phBuffer:get-child-relation (i).
            if num-entries(pcExpand,"/") >= piLevel then
            do: 
                // this only happens oncein the loop 
                if hRelation:child-buffer:serialize-name = entry(piLevel,pcExpand,"/") then
                do:
                    if ResolveExpandedChildLinks(hRelation:child-buffer,pcExpand,piLevel + 1) then
                        lResolved = true.
                    
                    hRelation:active = true.
                end.           
                else 
                    hRelation:active = false.
            end.
            else do:
                lResolved = true.
                hRelation:active = false.
            end. 
        end.    
        return lResolved. 
    end method.
    
    method protected handle CopyDataset(phDataset as handle, pcRequestedEntity as character, output phbuffer as handle):
        define variable hOriginalBuffer as handle no-undo.
       
        define variable hNewDataset     as handle no-undo.
        create dataset hNewDataset.
        hNewDataset:serialize-name = phDataset:serialize-name.
        hOriginalBuffer = DatasetUtil:GetBufferBySerializeName(phDataset,pcRequestedEntity).  
       
         /* top buffer from this dataset */
        //create buffer phbuffer for table hOriginalBuffer buffer-name hOriginalBuffer:table-handle:name.
        phBuffer = TableUtil:CreateTableLike(hOriginalBuffer):default-buffer-handle.   
        phbuffer:serialize-name =  hOriginalBuffer:serialize-name.
        hNewDataset:add-buffer(phbuffer).
        DatasetUtil:CloneParentRelation(hNewDataset, phbuffer, hOriginalBuffer).
        
        DatasetUtil:CloneChildRelations(hNewdataset, phbuffer, hOriginalBuffer).
        
        return hNewDataset. 
        catch e as Progress.Lang.Error :
           if valid-object(hNewDataset) then 
               delete object hNewDataset.
           undo, throw e.      
        end catch.
        
    end method.
    
    method protected ICollection CreateRelations(pcParent as character, pcExpand as character):
         define variable oLinks      as ICollection no-undo.
         define variable cExpand as character no-undo.
         define variable cEntities as character extent no-undo.
         define variable i as integer no-undo.
         define variable oRel as IRelationDefinition no-undo.
         oLinks = new Collection().
       
         do i = 1 to num-entries(pcExpand):
             cExpand = entry(i,pcExpand).
             extent(cEntities) = ?.
             cEntities = Array:Split(cExpand, "/").
             oRel = new RelationDefinition(pcParent,cEntities[1]).  
             if not oLinks:Contains(oRel) then
                 oLinks:Add(oRel).
             CreateRelations(1,cEntities,oLinks).      
         end.
         return oLinks.
     end method.
      
     method protected void CreateRelations(piParent as integer, pcEntities as character extent, pLinks as ICollection):
         define variable oRel as IRelationDefinition no-undo.
         if extent(pcEntities) >= piParent + 1 then
         do:
             oRel = new RelationDefinition(pcEntities[piParent],pcEntities[piParent + 1]).  
             if not pLinks:Contains(oRel) then
                 pLinks:Add(oRel).
             if extent(pcEntities) > piParent then     
                 CreateRelations(piParent + 1,pcEntities,pLinks).    
         end.
     end method.  
     
     method protected character GetQualName(pcRequestedentity as char, phBuffer as handle, output pcExpandQualname as character  ): 
         define variable cQualname       as character no-undo.
         define variable iQual           as integer no-undo.
         
         cQualname = BufferUtil:GetQualifiedBufferName(phBuffer).
         // expand is relative to pcRequestedentity. This may not be the top table in the dataset
         // so to find the relative name to pass to GetExpandSelect remove all qualifiers before including pcRequestedentity
         if num-entries(cQualName,"/") > 1 then
         do:
             pcExpandQualname = cQualname .
             do iQual = 1 to num-entries(cQualname,"/"):
                 entry(1,pcExpandQualname,"/") = "".
                 pcExpandQualname = left-trim(pcExpandQualname,"/").
                 // leave when we reached the requestedentity 
                 if entry(iQual,cQualName,"/") = pcRequestedentity then 
                 do: 
                     leave.
                 end.     
                  
             end.
         end.
         return cQualname. 
     end method.    
     
     method protected void ProcessSelect(pcRequestedentity as char, phBuffer as handle,poCLientRequest as IClientGetRequest ): 
         define variable cDummy as character no-undo.
         ProcessSelect(pcRequestedentity, phBuffer, false, poCLientRequest). 
     end method.    
     
     method protected void ProcessSelect(pcRequestedEntity as char, phBuffer as handle, plXrefTarget as logical, poCLientRequest as IClientGetRequest ): 
         define variable cQualname       as character no-undo.
         define variable iQual           as integer no-undo.
         define variable cExpandQualName as character no-undo.
         cQualname = GetQualName(pcRequestedEntity,phBuffer,output cExpandQualName).
         // expand is relative to pcRequestedentity. This may not be the top table in the dataset
         // so to find the relative name to pass to GetExpandSelect remove all qualifiers before including pcRequestedentity
         if num-entries(cQualName,"/") > 1 then
         do:
             // if xreftarget then the expand slect is defined on the parent 
             if plXrefTarget then 
                 cExpandQualname = StringUtil:RemoveEntryFromList(num-entries(cExpandQualname,"/"),cExpandQualname,"/"). 
                                   
             HideFields(phBuffer, poCLientRequest:GetExpandSelect(cExpandQualname)).
         end.    
     end method.    
     
     
     method protected void ProcessLinks(pcRequestedentity as character, phDataset as handle, phBuffer as handle,pExpandLinks as ICollection, pBEDataset as handle, pBeLinks as ICollection, pAllow as logical, poCLientRequest as IClientGetRequest):
         define variable oIterator      as IIterator no-undo.
         define variable oRelDef        as IRelationDefinition no-undo.
         define variable oBe            as IBusinessEntity no-undo.
         define variable oRelation      as IRelation no-undo.
         define variable lResolved      as logical no-undo.
         define variable cServiceEntity as character no-undo.
         define variable hRelation      as handle no-undo.
         define variable cPairsList     as character no-undo.
         define variable hBuffer        as handle no-undo.
         define variable hBeBuffer      as handle no-undo.
         define variable hNewTable      as handle no-undo.
         define variable cPublicName     as character no-undo.
         define variable cRelationParent as character no-undo.
         define variable cQualname       as character no-undo.
         define variable cExpandQualname  as character no-undo.
         define variable cUniquename      as character no-undo.
         define variable iSuffix         as integer no-undo.
         define variable iExpand as integer no-undo.
         define variable lcorrectparent as logical no-undo.
         define variable cChildpath as character no-undo.
         define variable lDynEntity as logical no-undo.           
       //  define variable iQual           as integer no-undo.
         
         oIterator = pExpandLinks:Iterator().
         cQualName = GetQualName(pcRequestedentity,phBuffer, output cExpandQualName). 
         linkloop:         
         do while oIterator:HasNext():
             oRelDef = cast(oIterator:Next(),IRelationDefinition).
             if oRelDef:ParentName = phBuffer:serialize-name then
             do:
                 lCorrectparent = true. // only used for measurements as of current
         
                 // this isassumed to be correct for every case, but currently implemented for measurements
                 // needs serious testing befor emaking general 
                 if oReldef:PublicName = "measurements" then
                 do:
                     cChildpath = cQualname.
                     entry(1,cChildpath,"/") = "".
                     cChildPAth = left-trim(cChildPAth,"/").
                     cCHildpath = left-trim(subst("&1/&2",cChildPAth,oRelDef:PublicName),"/").
                     lCorrectparent = false.
                     do iExpand = 1 to num-entries(Expand):
                         if entry(iExpand,Expand) begins cChildpath then
                         do:
                            lcorrectparent = true.
                            leave.
                         end.                              
                     end.
                     if not lcorrectparent then 
                         next linkloop.
                 
                 end.    
                 cRelationParent = oRelDef:ParentName.
                 
                 if phBuffer:table-handle:serialize-name <> phBuffer:serialize-name then
                 do:  
                     // this check is likely redundant as the check below also will catch this  
                     if pBEDataset:get-buffer-handle(1):name = phBuffer:name then
                         cRelationParent =  pBEDataset:get-buffer-handle(1):serialize-name.
                     else if pBEDataset:get-buffer-handle(1):serialize-name = phBuffer:table-handle:serialize-name then
                         cRelationParent =  phBuffer:table-handle:serialize-name.    
                     
                 end.     
                 lResolved = false.
                 oRelation = ?.
                 hRelation = ?.
                 hBuffer = ?.
                 hBeBuffer = ?.
                 
                 if valid-object(pBeLinks) then
                 do:
                     oRelation = GetRelation(pBeLinks, cRelationParent, oRelDef:PublicName).
/*                     message "got relation from belinks buffer" phBuffer:name*/
/*                     "relation"  oRelation                                   */
/*                                                                             */
/*                     view-as alert-box.                                      */
                     if valid-object(oRelation) then 
                     do:
                       // unecessary  - works with current datasource? fillmode?  
/*                         if pAllow <> true and oRelation:Cardinality = RelationTypeEnum:ManyToOne then                                                                            */
/*                         do:                                                                                                                                                      */
/*                             if pAllow = false then                                                                                                                               */
/*                                undo, throw new IllegalOperationError(subst("Can only expand &1 relation from '&2' to '&3' when request for '&2' has a unique key in parenthesis",*/
/*                                                                            oRelation:Cardinality:ToString(),                                                                     */
/*                                                                            oRelDef:ParentName,                                                                                   */
/*                                                                            oRelDef:PublicName)).                                                                                 */
/*                           else                                                                                                                                                   */
/*                              undo, throw new IllegalOperationError(subst("Cannot expand '&2/&3' since it is a '&1' relation",                                                    */
/*                                                                            oRelation:Cardinality:ToString(),                                                                     */
/*                                                                            oRelDef:ParentName,                                                                                   */
/*                                                                            oRelDef:PublicName)).                                                                                 */
/*                                                                                                                                                                                  */
/*                         end.                                                                                                                                                     */
                         cServiceEntity = oRelation:name.
                         lDynEntity = oRelation:IsDynamic.
                         oRelDef:Cardinality = oRelation:Cardinality.
                         cPairsList = oRelation:PairsList().
                         lResolved = true.
                         cPublicName = oRelation:PublicName. // use the relation anme since the expand is not case sensitive 
                         if type-of(oRelDef,RelationDefinition) then // the RelationDefinition IS NEWed from expand - make sure casing is correct 
                              cast(oRelDef,RelationDefinition):PublicName = cPublicName.
                          
                         //  add a relation for the xref so that the table in the other end is added to the dataset
                         if oRelation:XRef then
                         do:
                             oReldef:Xref = true. 
                             pExpandLinks:Add(new RelationDefinition(cPublicName,cPublicName)).
                         end.
                     end.
                 end.
                 if lResolved = false and valid-handle(pBeDataset) then
                 do:
                     hRelation = GetDatasetRelation(pBEDataset, cRelationParent, oRelDef:PublicName).
                     if valid-handle(hRelation) then
                     do:
                         cServiceEntity = hRelation:child-buffer:table-handle:serialize-name.
                         cPairsList = hRelation:relation-fields.
                         hBeBuffer = hRelation:child-buffer.
                         oRelDef:Cardinality = RelationTypeEnum:OneToMany.
                         cPublicName = hBeBuffer:serialize-name. // use the relation anme since the expand is not case sensitive 
                         lResolved = true.
                     end.    
                 end. 
                 if lResolved then
                 do:
                     do on error undo, throw:
                         if lDynEntity then 
                         do:
                             oBe = ServiceManager:CreateDynamicBusinessEntity(phBuffer, oRelation).
                         end.
                         else
                             oBe = ServiceManager:CreateBusinessEntity(cServiceEntity).
                         catch e as Progress.Lang.Error :
                             undo, throw new IllegalOperationError(subst("Child entity not found for relation from '&1' to '&2'",oRelDef:ParentName, oRelDef:PublicName),e).
                         end catch.
                     end.
                      
                     // The buffer may already be in the dataset of the caller 
                     // in which case it was copied in copychildren
                     // skip for the special Xref case where parent and child is the same 
                     // it would find the parent  
                     if oRelDef:ParentName <> oRelDef:PublicName then
                     do:  
                         hBuffer = DatasetUtil:GetBufferByPathName(phDataset, cQualName + "/" + cPublicName). 
                     end. 
                   
                     // TODO if valid check that it mathches the relation - and that relation is acive
                     // NOTE a copyChildren depends on the relation being acitvated and  
                     // the DatasetRouter will activate the link if it has be deactivated , but 
                     // expands are found that needs it - but it would be better to move that logiv here
                     // ir make sure it is done  
                     if not valid-handle(hBuffer) then
                     do:
                         if not valid-handle(hBeBuffer) then
                             HBeBuffer = oBe:LinkDataset:get-buffer-handle(1).
                   
                         cUniquename = hBeBuffer:table-handle:name.
                         // need unique name for DataAccess - (really only when the table has children - since the name is used in the join for children) 
                         // magic : (static name in DS definition will still work when TT is passed with BIND or BY-REFERENCE)
                         do while valid-handle(DatasetUtil:GetBufferByTableName(phDataset,cUniquename)):
                             iSuffix = iSuffix + 1.
                             if length(cUniquename) > 32 and iSuffix < 10 then
                             do:
                                 cUniqueName = substr(hBeBuffer:table-handle:name,1,31) + string(iSuffix).
                             end. 
                             if length(cUniquename) > 30  then
                             do:
                                 cUniqueName = substr(hBeBuffer:table-handle:name,1,30) + string(iSuffix).
                             end. 
                             else 
                                 cUniquename = subst("&1-&2",
                                                     hBeBuffer:table-handle:name,
                                                    iSuffix
                                                   ).
                         end.  
                         
                         hNewTable = TableUtil:CreateTableLike(hBeBuffer,cUniquename).
                        
                         // DO NOT CREATE BUFFER !! use default-buffer-handle
                         // using defualt befuffer ensures that we can pass input table to programs
                         // and have acces to the data set and the relations 
                        
                         // Exception - for alias we need to use table since we cannot 
                         // have different serialize-name for default-buffer   
                         if cPublicName <> hnewTable:serialize-name then
                         do:
                             create buffer hbuffer for table hNewTable buffer-name hNewTable:name.
                             hbuffer:serialize-name = cPublicName.
                         end.
                         else 
                             hBuffer = hNewTable:default-buffer-handle. 
                              
                         phDataset:add-buffer(hbuffer).
                         // something is wrong when using all parameters to set foreign-key-hidden - possibly not-active is oppsiote  
                         hRelation = phDataset:add-relation(phBuffer,hBuffer,cPairsList,false,true).
                         
                         // don't hide foreign key for XREF - we need it since the parent will be removedt
                         // also don't hide for other relations han one to many as the caller may want 
                         // to PUT or POST to this and need the key as it will not be a child in that case 
                         if oRelDef:Cardinality = RelationTypeEnum:OneToMany 
                         and oRelDef:ParentName <> oRelDef:PublicName then
                         do:
                             SetOneToManyForeignKeyVisibility(hRelation,cExpandQualname, poCLientRequest:GetExpandSelect(cExpandQualname)).
                         end.
                     end.      
                     // get rid of the XRef relation now that it is resolved             
                     if oRelDef:ParentName = oRelDef:PublicName then 
                     do:
                         pExpandLinks:Remove(oRelDef).
                     end.
                     
                     // if this is an xref realtion then we wait with select until next round where oRelDef:ParentName = oRelDef:PublicName
                     // since the slect is for the xref target
                     if valid-object(oRelation) = false or oRelation:XRef = false then 
                          ProcessSelect(pcRequestedentity,
                                        hBuffer,
                                        oRelDef:ParentName = oRelDef:PublicName,
                                        poCLientRequest).
                                        
                     ProcessLinks(pcRequestedentity,phDataset,hBuffer,pExpandLinks,oBe:LinkDataset,if oBe:HasLink then oBe:Links else ?,?,poCLientRequest).
                 end.
                 else  
                     undo, throw new IllegalOperationError(subst("Relation from '&1' to '&2' is not valid",oRelDef:ParentName, oRelDef:PublicName)). 
             end.
            
                        
         end.
         
     end method.   
    
     // default behavior is to hide one to many foreignkeys. 
     // They cannnot be edited and PUT an POST should preferably use parent(keys)/child to ensure correct foreignkey reference  
     method protected void SetOneToManyForeignKeyVisibility(phRelation as handle, input pcParentQualifier as character, pClientSelect as character):
         define variable i           as integer no-undo.
         define variable lAnyVisible as logical no-undo.
         define variable hfld        as handle  no-undo.
         
         do i = 1 to num-entries(phRelation:relation-fields) by 2:
             hFld = phRelation:child-buffer:buffer-field(entry(i + 1,phRelation:relation-fields)).
             if lookup(hFld:serialize-name,pClientSelect) > 0 then 
             do:
                 lAnyVisible = true.
                  hFld:serialize-hidden = true.
             end.    
         end.
         
         if lAnyVisible = false then 
             phRelation:foreign-key-hidden = true.
     end.   
      
     method protected void HideFields(phBuffer as handle, pClientSelect as character):
         define variable i              as integer no-undo.
         define variable hFld           as handle no-undo.
         
         if pClientSelect > "" then 
         do i = 1 to phBuffer:num-fields:
             hFld = phBuffer:buffer-field(i).  
             if lookup(hFLd:serialize-name,pClientSelect) = 0 then
             do:
                 hFLd:serialize-hidden = true.         
             end.    
         end.
     end.   
     
     method private logical HasXrefRelation(pLinks as ICollection):
        define variable oIterator as IIterator no-undo.  
        define variable oRelation       as IRelation no-undo.
       
        if valid-object(pLinks) then
        do:
            oIterator = pLinks:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                if oRelation:XRef then 
                    return true.
            end.       
        end.
     end method.
     
     method protected IRelation GetRelation(pLinks as ICollection, pcParent as character, pcPubName as character):
        define variable oIterator as IIterator no-undo.  
        define variable oRelation       as IRelation no-undo.
       
        if valid-object(pLinks) then
        do:
            oIterator = pLinks:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
           
                if oRelation:ParentName  = pcParent
                and oRelation:PublicName = pcPubName then
                   return oRelation.
            end.       
        end.
        return ?.
    end method.  
    
    method protected handle GetDatasetRelation(phDataset as handle, pcParent as character, pcPubName as character):
        define variable i as integer no-undo.
        define variable hRelation as handle no-undo.
        do i = 1 to phDataset:num-relations :
            hRelation = phdataset:get-relation(i).
            if  hRelation:parent-buffer:serialize-name = pcParent
            and hRelation:child-buffer:serialize-name = pcPubName then
                return hRelation.
        end.    
        return ?.
    end method. 
     
     
    
end class.