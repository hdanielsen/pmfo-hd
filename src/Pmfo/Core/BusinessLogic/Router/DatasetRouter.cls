 
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
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IMap from propath.
using Pmfo.Core.BusinessLogic.IDatasetRelation from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.BusinessLogic.Router.LinkExpander from propath.
using Pmfo.Core.BusinessLogic.Router.RequestRouter from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.MethodNotAllowedError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.Error.NotImplementedError from propath.

class Pmfo.Core.BusinessLogic.Router.DatasetRouter inherits RequestRouter: 
    
    define protected property Datasets  as handle extent no-undo get. set.
    define protected property Resource  as character no-undo get. set.
    constructor public DatasetRouter (pResource as character, pDatasets as handle extent, pLinks as ICollection, pActions as IMap, pFunctions as IMap, pFixedChildren as character):
        super ( pDatasets[1], pLinks, pActions , pFunctions, pFixedChildren ).
        Resource = pResource.
        Datasets = pDatasets.
    end constructor.
    
    method protected override void CreateMembers(pRowState as RowStateEnum, pcRequestedMember as character) :
        define variable lHasMainDataset as logical no-undo.
        if Datasets[1]:serialize-name = Resource then
        do:
            super:CreateMembers(pRowState,pcrequestedMember).
            lHasMainDataset = true.
        end.
        AddDatasets(lHasMainDataset,pcRequestedMember).
    end method.
    
    method protected logical AddDatasets(plSkipFirst as logical,pcRequestedMember as character):
        define variable i as integer no-undo.
        define variable lAdded as logical no-undo.
       
        do i = if plSkipFirst then 2 else 1 to extent(Datasets):
            if pcRequestedMember = "" or pcRequestedMember = Datasets[i]:serialize-name then
            do on error undo, throw:
                create ttMember.
                assign
                    ttMember.key = Datasets[i]:serialize-name
                    ttMember.memberHandle = Datasets[i]
                    lAdded = true.
                   .
                if pcRequestedMember > "" then
                    return true.   
                catch e as Progress.Lang.Error :
                    delete ttMember no-error.
                    if e:GetMessageNum(1) = 132 then 
                    do:
                        ThrowAlreadyExistsError("Dataset", Datasets[i]:serialize-name).    
                    end.
                    else 
                       undo, throw e.
                end catch.
            end.
        end.
        return lAdded.
        
    end method. 
    
    method protected handle GetMemberDataset(buffer ttMember for ttMember):
        if valid-handle(ttMember.memberHandle) 
        and ttMember.memberHandle:type = "dataset" then
            return ttMember.memberHandle.
        else 
            return Datasets[1].     
    end method.
   
    
/*    method protected IMethod GetMethod(pcName as character):     */
/*        define variable oIterator  as IIterator no-undo.         */
/*        define variable oMapEntry  as IMapEntry no-undo.         */
/*        if valid-object(Methods) then                            */
/*        do:                                                      */
/*            oIterator = Methods:EntrySet:Iterator().             */
/*            do while oIterator:HasNext():                        */
/*                oMapEntry = cast(oIterator:Next(),IMapEntry).    */
/*                if cast(oMapEntry:Key,String):Value = pcName then*/
/*                do:                                              */
/*                    return cast(oMapentry:Value,IMethod).        */
/*                end.                                             */
/*            end.                                                 */
/*        end.                                                     */
/*        return ?.                                                */
/*    end method.                                                  */
    
    
    method public handle ResolveRequest (pClientRequest as IClientRequest):
        define variable hDataset       as handle        no-undo.
        define variable oRowState      as RowStateEnum  no-undo.
        define variable cExpand        as character     no-undo.
        define variable oMethod        as IMethod       no-undo.
        define variable oLinks         as ICollection   no-undo.
        define variable cRequestEntity as character     no-undo.
        define variable oLinkExpander  as LinkExpander  no-undo.
        define variable hNewDataset    as handle no-undo.
        // keep alive for Links
        
        oRowState = GetMethodRowStateEnum(pClientRequest:Method).
         // throw error if the first collection does not reference the resource 
        if Resource <> pClientRequest:Collections[1] then
            undo, throw new IllegalArgumentError(subst("Resource '&1' in top of search path for resource '&2'",pClientRequest:Collections[1],Resource)). 
         
        if extent(pClientRequest:Collections) = 1 then 
        do:
            if pClientRequest:Collections[1] = Datasets[1]:serialize-name then
            do: 
                DeactivateDatasetLinks(pClientRequest:Collections[1],oRowState,?).
                hDataset = Datasets[1].
            end.
            else        
                undo, throw new NotFoundError(subst("Dataset '&1'",pClientRequest:Collections[1]),subst("Resource '&1'",pClientRequest:Collections[1])).
        end.    
       
        if not valid-handle(hDataset) then
        do:
            CreateMembers(oRowState,pClientRequest:Collections[2]).  
            find ttMember where ttMember.key = pClientRequest:Collections[2] no-error.
           
            if not avail ttMember then
            do:
                undo, throw new NotFoundError(subst("Dataset, Buffer, &1 or Link '&2'",if oRowState = RowStateEnum:None then "Function" else "Action",pClientRequest:Collections[2]),subst("Resource or dataset '&1'",pClientRequest:Collections[1])).
            end.
            if valid-object(ttMember.memberObject) and type-of(ttMember.memberObject,IRelation) then
            do:
                if RowStateEnum:None:Equals(oRowState) = false then
                do: 
                    if RelationTypeEnum:ManyToOne:Equals(cast(ttMember.memberObject,IRelation):Cardinality) then 
                        undo, throw new MethodNotAllowedError(pClientRequest:Method,pClientRequest:Collections[1],pClientRequest:Collections[2]). 
                    
                    if cast(ttMember.memberObject,IRelation):CanCreate = false and RowStateEnum:Created:Equals(oRowState) then 
                        undo, throw new MethodNotAllowedError(pClientRequest:Method,pClientRequest:Collections[1],pClientRequest:Collections[2]). 
                    if cast(ttMember.memberObject,IRelation):CanUpdate = false and RowStateEnum:Modified:Equals(oRowState) then 
                        undo, throw new MethodNotAllowedError(pClientRequest:Method,pClientRequest:Collections[1],pClientRequest:Collections[2]). 
                    if cast(ttMember.memberObject,IRelation):CanDelete = false and RowStateEnum:Deleted:Equals(oRowState) then 
                        undo, throw new MethodNotAllowedError(pClientRequest:Method,pClientRequest:Collections[1],pClientRequest:Collections[2]). 
                end.
            end.
            hDataset = getMemberDataset(buffer ttMember).
                
            if extent(pClientRequest:Collections) > 2 and valid-handle(ttMember.memberHandle) then
            do:
                
                hDataset = ResolveSubRequest(
                             ttMember.memberHandle,
                             pClientRequest, 
                             oRowState,
                             output oMethod, 
                             output oLinks)  . 
                // the Be may have assinged linkedataset to a dynamic in which case we delete it if it is replaced  
                 if ttMember.memberHandle:type = "dataset" and ttMember.memberHandle <> hDataset and ttMember.memberHandle:dynamic then 
                    delete object ttMember.memberHandle.                
            end.
            else if valid-object(ttMember.memberObject) then
            do:
                if type-of(ttMember.memberObject,IMethod) then
                do:
                    oMethod = cast(ttMember.memberObject,IMethod).  
                end.
                else do:
                    oLinks = new Collection().
                    
                    hNewDataset = CreateLinkDataset(hDataset,2,pClientRequest,cast(ttMember.memberObject,IRelation),oRowState,oLinks,output oMethod).
                    if hNewDataset <> hDataset then 
                    do:
                        if valid-handle(hDataset) and hDataset:dynamic then
                            delete object hDataset.
                        hDataset = hNewDataset.    
                    end.  
                end.       
            end. 
        end.
          
        // link expnader needs this  
        pClientRequest:ServiceMethod = oMethod.
        
        if type-of(pClientRequest,IClientGetRequest) then
        do:
            cRequestEntity = pClientRequest:Collections[extent(pClientRequest:Collections) - if valid-object(oMethod) then 1 else 0 ].
            cExpand = GetFullExpand(cast(pClientRequest,IClientGetRequest),cRequestEntity,hDataset).
       
            if cExpand > "" then
            do:
              if pClientRequest:Method <> "GET" then
                    undo, throw new BadRequestError(subst("Expand is not supported &1 request",pClientRequest)).
                
                oLinkExpander = new LinkExpander().
                 
                // NOTE the resolved/added realtions will be collected in ClientRequest:ExpandedRelations
                // used by FilterParser and currently merged with BE:Links by BusinessObjectRequestHandler to be sued by JsonGetResponseProcessor   
                hNewDataset = oLinkExpander:Expand(hDataset, cRequestEntity, cExpand, cast(pClientRequest,IClientGetRequest), Links).
                if hNewDataset <> hDataset then 
                do:
                    if valid-handle(hDataset) and hDataset:dynamic then
                         delete object hDataset.
                    hDataset = hNewDataset.    
                end. 
            end.  
        end.
       
        // parent links needed for queryRequestbuilder to validate URL prior to the requested entity
        // There is an ExpandedRelations that tracks expands see above     
        pClientRequest:Relations = oLinks.   
        if hDataset <> Datasets[1] then 
            ResetDatasetRelations(hdataset).
           
        return hDataset.         
    end method.  
    
    // If the filter has any qualifiers that are not in the expand add them so they get added to the dataset
    // The DataAccess will get the tables that are only for the server from GetInnerJoins(output pcServerjoins) and set BIffer no-fill 
    // The FewtchDelegate:fetchData will use GetInnerJoins(output pcServerjoins) to disable the relations so that the 
    // JsonGetResponseProcsssor does not add the empty notes 
    // TODO - consider marking the relation added to the ClientReeust with some propery instead  and use that in the JsonGetResponseProcsssor       
    method protected character GetFullExpand(pClientRequest as IClientGetRequest, cRequestEntity as character, phDataset as handle):
        define variable cFilter          as character no-undo.
        define variable cStrippedFilter  as character no-undo.
        define variable cExpand          as character no-undo.
        define variable cWords           as character no-undo.
        define variable cWord            as character no-undo.
        define variable cQualifiers      as character no-undo.
        define variable i                as integer no-undo.
        define variable iLastQual        as integer no-undo.
        define variable hbuffer          as handle no-undo.
        define variable cApplyInnerJoins as character no-undo.
        define variable cEntity as character no-undo.
        cFilter          = pClientRequest:Filter.
        cExpand          = pClientRequest:Expand.
        cApplyInnerJoins = pClientRequest:ApplyInnerJoins.
        
        if cFilter > "" then 
        do: 
            hBuffer = DatasetUtil:GetBufferBySerializeName(phDataset, cRequestEntity).
            if index(cFilter,"/") > 0 then 
            do: 
                // we only care about getting the qualifiers 
                // - the LinkExpander will throw error if bad ref      
                cStrippedFilter = StringUtil:MaskQuotes(cFilter,"@").
                cStrippedFilter = replace(cStrippedFilter,"("," ").
                cStrippedFilter = replace(cStrippedFilter,")"," ").
                
                do i = 1 to num-entries(cStrippedFilter,""):
                    cWord = entry(i,cStrippedFilter,"").
                    if cWord > "" then
                    do: 
                        iLastQual = r-index(cWord,"/").
                        if iLastQual > 0 then 
                        do:
                            cEntity = substr(cWord,1,iLastQual - 1).
                            // if already in dataset then no need to track
                            
                            if valid-handle(BufferUtil:GetBufferFromUrlQualifier(hBuffer, cEntity)) = false then
                            do:
                                
                                if cExpand = ? or lookup(cEntity,cExpand) = 0 then
                                do:
                                    if num-entries(cEntity,"/") > 1 then 
                                        undo, throw new NotImplementedError(subst("Filter with more than one entity qualifier '&1/&2' without corresponding expand of '&1'",
                                                                                  cEntity,
                                                                                  entry(num-entries(cStrippedFilter,"/"),cStrippedFilter,"/")
                                                                                  )
                                                                           ). 
                                    
                                    
                                    cExpand = StringUtil:AppendEntry(cExpand, cEntity).
                                    
                                end.    
                            end.    
                        end.
                    end.
                end.    
            end.     
        end.     
        if cApplyInnerJoins > "" then 
        do:
            do i = 1 to num-entries(cApplyInnerJoins):
                cEntity = entry(i,cApplyInnerJoins).
                if cExpand = ? or lookup(entry(i,pClientRequest:ApplyInnerJoins),cExpand) = 0 then
                    cExpand = StringUtil:AppendEntry(cExpand, cEntity).
            end.    
        end.
        return cExpand.   
    end method.    
    
    method protected RowStateEnum GetMethodRowStateEnum (pcMethod as char):
        case pcMethod:
            when "GET"    then return RowStateEnum:None.
            when "PUT"    then return RowStateEnum:Modified.
            when "POST"   then return RowStateEnum:Created.
            when "DELETE" then return RowStateEnum:Deleted.
            otherwise 
                undo, throw new IllegalArgumentError(subst("Method '&1'",pcMethod)).
        end.
    end method.
    
    method protected void ResetDatasetRelations(phnewdataset as handle):
        define variable oIterator as IIterator no-undo.  
        define variable oRelation       as IRelation no-undo.
        define variable hRelation as handle no-undo.
        if not valid-handle(phnewdataset) then
            undo, throw new IllegalArgumentError("Dataset handle is not valid").
        if valid-object(Links) then
        do:
            oIterator = Links:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                
                if type-of(oRelation,IDatasetRelation) then
                do:
                    hRelation= DatasetUtil:GetRelation(phnewdataset, oRelation:ParentName, oRelation:PublicName).
                    if valid-handle(hRelation) then
                        cast(oRelation,IDatasetRelation):RelationHandle = hRelation.
                end.   
            end.       
        end.
    end method.  
     
    method protected handle CreateLinkDataset(pDataset as handle, piPosition as integer, pClientRequest as IClientRequest, pRelation as IRelation,pRowState as RowStateEnum,  pLinks as ICollection, output pMethod as IMethod ).
        define variable hDataset              as handle no-undo.
        define variable hParentBuffer         as handle no-undo.
        define variable cDsName               as character no-undo.
        create dataset hDataset.
        // progress limitation is 32 char
        // we could probably do this always the "-dyn" is added for (memory leak( debugging purposes only 
        if length(pdataset:name + "-dyn") > 32 then 
            cDsName = "dy" + substr(pdataset:name,3).
        else    
            cDsName = pdataset:name + "-dyn".
         
        hDataset:name = cDsName. // onot used iother than in memopry leak check programs...
        hDataset:serialize-name = pdataset:serialize-name.
        hParentBuffer = CreateAndAddParentRelations(pDataset,hDataset,pClientRequest:Collections[piPosition - 1]).
        ResolveLinkAndCreateTables(hDataset, hParentBuffer, piPosition, pClientRequest, pRelation, pRowState, pLinks, output pMethod). 
        
        return hDataset.
        catch e as Progress.Lang.Error :
            if valid-handle(hDataset) then 
                delete object hdataset.
            undo, throw e.
               
        end catch.     
    end method.
    
    // We may have found a child table in the BusinessEntity, but the URL may reference a child that 
    // only exists in our child's BusinessEntity. 
    // This will create a new dataset with the buffers from here and the other BE    
    method protected handle CreatePassThruLinkDataset(pDataset as handle, piPosition as integer, pClientRequest as IClientRequest, phBuffer as handle,pRowState as RowStateEnum,  pLinks as ICollection, output pMethod as IMethod ).
        define variable hDataset              as handle no-undo.
        define variable hParentBuffer         as handle no-undo.
        define variable oRelation             as IRelation no-undo.
        define variable hChild                as handle no-undo.
        if valid-object(phBuffer:parent-relation) then
        do:
            create dataset hDataset.
            hDataset:serialize-name = pdataset:serialize-name.
            hDataset:name = pdataset:name + "-dyn-passthru". // onot used iother than in memopry leak check programs...
            oRelation = CreateRelation(phBuffer:parent-relation,pRowState).
            hParentBuffer = CreateAndAddParentRelations(pDataset,hDataset,pClientRequest:Collections[piPosition - 2]).
            ResolveLinkAndCreateTables(hDataset, hParentBuffer, piPosition - 1, pClientRequest, oRelation, pRowState, pLinks, output pMethod).
            return hDataset.
        end.
        return ?. 
        catch e as Progress.Lang.Error :
            if valid-handle(hDataset) then 
               delete object hdataset.
            undo, throw e.
               
        end catch.       
    end method.
    
    method protected handle ResolveSubRequest(phParent as handle, pClientRequest as IClientRequest,pRowState as RowStateEnum, output pMethod as IMethod, output pLinks as ICollection):
        define variable iCollection   as integer no-undo.
        define variable hBuffer       as handle no-undo.
        define variable hDataset      as handle no-undo.
        define variable hParentBuffer as handle no-undo.
        define variable oRelation     as IRelation no-undo.
        define variable iStart        as integer no-undo.    
        define variable hPassThruDataset as handle no-undo.
       
        if phParent:type = "dataset" then
        do:
            hDataset = phParent.
            hParentBuffer = DatasetUtil:GetTopBufferBySerializeName(phParent,pClientRequest:Collections[3]).
            if not valid-handle(hParentBuffer) then
                undo, throw new NotFoundError(subst("Buffer &1",pClientRequest:Collections[3]),subst("Dataset",pClientRequest:Collections[2])).
            iStart = 4.    
        end.
        else do:
            hDataset = phParent:dataset.  
            hParentBuffer =  phParent.
            iStart = 3.
        end.
        
        do iCollection = iStart to extent(pClientRequest:Collections):
            hbuffer = GetChildBufferBySerializeName(hParentBuffer, pClientRequest:Collections[iCollection]).
            
            if not valid-handle(hBuffer) then 
            do:
                oRelation = GetLink(pClientRequest:Collections[iCollection - 1], pClientRequest:Collections[iCollection]).
                if valid-object(oRelation) then
                do:
                    pLinks = new Collection().
                    hDataset = CreateLinkDataset(hDataset,iCollection,pClientRequest,oRelation,pRowState,pLinks,output pMethod).
                    return hDataset.
                end.            
            end. 
            if not valid-handle(hBuffer) then 
            do: 
                 pLinks = new Collection().
                 hPassThruDataset = CreatePassThruLinkDataset(hDataset,iCollection,pClientRequest,hParentBuffer,pRowState,pLinks,output pMethod).
                 if valid-handle(hPassThruDataset) then
                     return hPassThruDataset.
            end.
            if not valid-handle(hBuffer) then
                undo, throw new NotFoundError(
                                              // parameter 1
                                              subst("Buffer, &1 or Link '&2'",if pRowState = RowStateEnum:None then "Function" else "Action",pClientRequest:Collections[iCollection]),
                                              // parameter 2
                                              subst("Buffer '&1'",pClientRequest:Collections[iCollection - 1])
                                              ).
            hParentBuffer = hBuffer.
        end.
        
        return hDataset. 
    end method.    
     
    method protected handle CreateAndAddParentRelations(pOriginalDataset as handle, pNewDataset as handle, pcParentName as character):
        define variable hOriginalParentBuffer as handle no-undo.
        define variable hParentBuffer         as handle no-undo.
        
        hOriginalParentBuffer = DatasetUtil:GetBufferBySerializeName(pOriginalDataset,pcParentName).  
      
         /* top buffer from this dataset */
        create buffer hParentBuffer for table hOriginalParentBuffer buffer-name hOriginalParentBuffer:table-handle:name.
        hParentBuffer:serialize-name =  hOriginalParentBuffer:serialize-name.
        pNewDataset:add-buffer(hParentBuffer).
        
        DatasetUtil:CopyParentRelation(pNewDataset, hParentBuffer, hOriginalParentBuffer).
        
        return hParentBuffer. 
    end method.     
    
     
    
end class.