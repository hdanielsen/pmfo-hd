 
 /*------------------------------------------------------------------------
    File        : RequestRouter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Sep 07 09:50:04 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.IMapEntry from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.BusinessLogic.DatasetRelation from propath.
using Pmfo.Core.BusinessLogic.DynamicRelation from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IDatasetRelation from propath.
using Pmfo.Core.BusinessLogic.IDynamicChild from propath.
using Pmfo.Core.BusinessLogic.IDynamicRelation from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.Router.LinkRouter from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.
using System.Buffer from assembly.
using Pmfo.Core.BusinessLogic.IDynamicBusinessEntity from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
 
 
class Pmfo.Core.BusinessLogic.Router.RequestRouter abstract  : 

     define protected temp-table ttMember  no-undo 
            field key as character
            field memberHandle as handle
            field memberObject as Object
            index key as primary unique key. 
            
   
     define protected property Actions       as IMap        no-undo get. set.
     define protected property Functions     as IMap        no-undo get. set.
     define protected property Links         as ICollection no-undo get. set.
     define protected property FixedChildren as character   no-undo get. set.
     define protected property Dataset       as handle      no-undo get. set.
     
     define protected property ServiceManager as IServiceManager no-undo 
         get():
             return  cast(Application:ServiceManager,IServiceManager).
         end. 
         
     constructor public RequestRouter (pDataset as handle , pLinks as ICollection, pActions as IMap, pFunctions as IMap, pFixedChildren as character ):
         super ( ).
         this-object:Dataset = pDataset.
         Links = pLinks.       
         Functions = pFunctions.
         Actions = pActions.
         FixedChildren = pFixedChildren.
     end constructor.
     
     // create a relation for pass thru linking when not found
     method protected IRelation CreateRelation(phRelation as handle, pRowState as  RowStateEnum):
         define variable cDisabled as character extent.   
         cDisabled = if pRowState = RowStateEnum:None then Array:ToArray("POST","PUT","DELETE") else Array:ToArray("GET"). // no
         return new DatasetRelation(phRelation,cDisabled).
     end method.    
     
     method protected void ResolveLinkAndCreateTables(pNewDataset as handle, phParentBuffer as handle, piPosition as integer, pClientRequest as IClientRequest, pRelation as IRelation,pRowState as RowStateEnum, pLinks as ICollection, output pMethod as IMethod): 
         define variable oLinkRouter           as LinkRouter no-undo.
         define variable oChild                as IBusinessEntity no-undo.
         define variable hBuffer               as handle no-undo.
         define variable cKeyWhere             as character no-undo.
         define variable cParentKeys           as character no-undo.
         define variable cParentKeyArray       as character extent no-undo.
         
         pLinks:Add(pRelation).
         
         /* the link is per defintion to the first dataset in the child */
         // we use the relation :name to find the entity - ans set the serialize-name from publci name below 
         oChild = cast(ServiceManager:CreateBusinessEntity(pRelation:Name),IBusinessEntity).
         if type-of(oChild,IDynamicBusinessEntity) then 
         do: 
             undo, throw new UnsupportedOperationError( 
                subst("Resource '&1' in URL &2 is unsupported as a navigation link. It is only intended for the expand query option.",
                            pRelation:PublicName,
                            pClientRequest:ServiceURI
                            )
                   ).   
                       
         end.
         if type-of(oChild,IDynamicChild) then 
         do:
             if type-of(pRelation,IDynamicRelation) then 
             do: 
                 
                 // TODO this need some streamlining -  duplicated efforts to get keys 
                 cParentKeys = ServiceInfo:GetKeys(phParentBuffer).
                 cParentKeyArray = Array:Split(cParentKeys).
                 // default assumption is that the child has or will have  the same fields
                 // since this is  used to create the child bufffer dynamically by the LinkRouter  
                 cast(pRelation,IDynamicRelation):CreateJoinPredicates(cParentKeyArray, cParentKeyArray).
                 cast(oChild,IDynamicChild):LoadMethod(pClientRequest,phParentBuffer,cParentKeyArray).
             end.    
             else 
                 undo, throw new IllegalStateError(subst("Dynamic child &1 requires a Dynamic Relation",oChild:Resource)).
         end.
         
         oLinkRouter = new LinkRouter(oChild:LinkDataset,
                                      if oChild:HasLink then oChild:Links else ?,
                                      if oChild:HasAction then oChild:Actions else ?,
                                      if oChild:HasFunction then oChild:Functions else ?, 
                                      oChild:GetFixedChildren(pClientRequest)). 
         
         hBuffer = oLinkRouter:ResolveRequest (pNewDataset, pRelation, piPosition, pClientRequest, pRowState, pLinks, output pMethod ).
         
         pNewDataset:add-relation(phParentBuffer,hBuffer,pRelation:PairsList(),false,true).
         
         if pClientRequest:KeyWheres[piPosition] > "" then
         do on error undo, throw:
             cKeyWhere = oChild:ConvertSingleKeyToExpression(hbuffer, pClientRequest:KeyWheres[piPosition]).
             pClientRequest:KeyWheres[piPosition] = cKeyWhere.
             catch e as Progress.Lang.Error :
                 // this will throw an error in DataRequestBuilder for now     
             end catch.
         end.   
               
         catch e1 as NotFoundError :
             undo, throw e1.
         end catch.
         catch e2 as Error :
             undo, throw new ApplicationError(subst("Failed to resolve link from '&1' to '&2'",pRelation:ParentName,pRelation:PublicName),e2).        
         end catch.
        
    end method.    
    
    method protected void CreateMembers( pRowState as RowStateEnum,pcRequestedMember as character):
   
        define variable hBuffer  as handle no-undo.
        define variable oMethod  as IMethod no-undo.
        
        hBuffer = DatasetUtil:GetTopBuffer(this-object:Dataset,1).
        
        if hBuffer:serialize-name = this-object:Dataset:serialize-name  then
        do:
            oMethod = AddMethod(pRowState,pcRequestedMember).  
            
            DeactivateDatasetLinks(hBuffer:serialize-name, pRowState, oMethod).
            
            AddChildBuffer(hBuffer,pcRequestedMember).
            AddLink(hBuffer:serialize-name,pcRequestedMember).
        end. 
        else do:
            // if this is dataset with more than one top buffer then support GetDataRequest method 
            if DatasetUtil:GetNumTopBuffersExcludeRepos(this-object:Dataset) > 1 then
            do:
                oMethod = AddMethod(pRowState,pcRequestedMember).
                if valid-object(oMethod) and valid-handle(DatasetUtil:GetBufferBySerializeName(this-object:Dataset, pcRequestedMember)) then
                    undo, throw new IllegalStateError(subst("Request for '&1' is ambiguous. There is both a &2 and Link with this name in Resurce '&3', Please contact IT.",
                                                             pcRequestedMember,
                                                             if pRowState = RowStateEnum:None then "Function" else "Action",
                                                             this-object:Dataset:serialize-name
                                                            )
                                                      ).
                  
            end.     
            if not valid-object(oMethod) then  
                AddTopBuffer(this-object:Dataset,pcRequestedMember). 
        end.
              
    end method.
     
    method protected logical AddChildBuffer(phParent as handle,pcRequestedMember as character):
        return AddChildBufferOrBuffers(phParent,pcRequestedMember).
    end method.   
    
    method protected logical AddChildBuffers(phParent as handle):
        return AddChildBufferOrBuffers(phParent,"").
    end method.

     method protected logical AddChildBufferOrBuffers(phParent as handle, pcRequestedMember as character):
        define variable i as integer no-undo.
        define variable hRelation as handle no-undo.
        define variable lAdded as logical no-undo.
        do i = 1 to phParent:num-child-relations:
             hRelation = phParent:get-child-relation (i).
             if hRelation:recursive = false 
             and hRelation:reposition = false 
             and (pcRequestedMember = "" or hRelation:child-buffer:serialize-name = pcRequestedMember) then
             do on error undo, throw:
                 create ttMember.
                 assign
                     ttMember.key = hRelation:child-buffer:serialize-name
                     ttMember.memberHandle = hRelation:child-buffer.
                     lAdded = true.
                 .
                 if pcRequestedMember > "" then return true.   
                 catch e as Progress.Lang.Error :
                    delete ttMember no-error.
                    if e:GetMessageNum(1) = 132 then 
                        ThrowAlreadyExistsError("Child Buffer", hRelation:child-buffer:serialize-name).    
                    else 
                       undo, throw e.
                end catch.
             end.      
        end.     
        return lAdded.
    end method.    
    
    method protected logical AddTopBuffer(phDataset as handle,pcRequestedMember as character):
        define variable i      as integer no-undo.
        define variable lAdded as logical no-undo.
        do i = 1 to phDataset:num-top-buffers:
            if pcRequestedMember = "" or phDataset:get-top-buffer(i):serialize-name = pcRequestedMember then
            do on error undo, throw:
                create ttMember.
                assign
                    ttMember.key = phDataset:get-top-buffer(i):serialize-name
                    ttMember.memberHandle = phDataset:get-top-buffer(i).
                    lAdded = true.
                     .
                if pcRequestedMember > "" then
                    return true.     
                catch e as Progress.Lang.Error :
                if e:GetMessageNum(1) = 132 then 
                do:   
                    delete ttMember no-error.
                    if e:GetMessageNum(1) = 132 then 
                        ThrowAlreadyExistsError("Top Buffer", phDataset:get-top-buffer(i):serialize-name).    
                    else 
                        undo, throw e.          
                end.
            end catch.
            end.
        end.     
        return lAdded.
        
    end method. 
    
    method protected IMethod AddMethod(pRowState as RowStateEnum,pcRequestedMember as character):
        define variable oIterator  as IIterator no-undo.  
        define variable oMethods   as IMap no-undo.
        define variable oMethod    as IMethod no-undo.
        oMethods = if pRowState = RowStateEnum:None then Functions else Actions. 
       
        if valid-object(oMethods) then
        do:
            oMethod = cast(oMethods:Get(new String(pcRequestedMember)),IMethod). 
            if valid-object(oMethod) then
            do:
                createMethodMember(oMethod).
                return oMethod.
            end.    
        end.
        return ?. 
    end method. 
    
    method protected character AddMethods(pRowState as RowStateEnum):
        define variable oMapEntry  as IMapEntry no-undo.
        define variable oMethods   as IMap no-undo.
        define variable oMethod    as IMethod no-undo.
        define variable oIterator  as IIterator no-undo.  
        define variable cNames     as character no-undo.
        
        oMethods = if pRowState = RowStateEnum:None then Functions else Actions. 
     
        if valid-object(oMethods) then
        do:
            oIterator = oMethods:Values:Iterator().
            oMethod = cast(oIterator:Next(),IMethod).
            CreateMethodMember(oMethod).
            cNames = cNames 
                   + (if CNames = "" then "," else "")
                   + oMethod:name.
        end.
        return cNames.
         
    end method. 
      
    method protected void CreateMethodMember(pMethod as IMethod):
        create ttMember.
        assign
            ttMember.key = pMethod:name
            ttMember.memberObject = pMethod
             .
        catch e as Progress.Lang.Error :
            delete ttMember no-error.
            if e:GetMessageNum(1) = 132 then 
                        ThrowAlreadyExistsError("Method", pMethod:name).    
                    else 
                       undo, throw e.
        end catch.
    end method.    
    
    method protected logical AddLink(pcParent as character,pcRequestedMember as character):
        return AddLinkOrLinks(pcParent,pcRequestedMember).
    end method.   
    
    method protected logical AddLinks(pcParent as character):
        return AddLinkOrLinks(pcParent,"").
    end method.
    
    method protected logical AddLinkOrLinks(pcParent as character,pcRequestedMember as character):
        define variable oIterator  as IIterator no-undo.  
        define variable oRelation       as IRelation no-undo.
        define variable lAdded as logical no-undo.
        
        if valid-object(Links) then
        do:
            oIterator = Links:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                if oRelation:ParentName  = pcParent 
                and (pcRequestedMember = "" or oRelation:PublicName = pcRequestedMember) then
                do:
                   find ttMember where ttMember.key = oRelation:PublicName no-error.
                   if avail ttMember then
                   do: 
                       if valid-handle(ttMember.memberHandle) 
                       and ttMember.memberHandle:type = "buffer"
                       and valid-handle(ttMember.memberHandle:parent-relation)
                       and type-of(oRelation,IDatasetRelation) then
                       do:
                          ttMember.memberObject = oRelation.
                       end.   
                       else 
                          undo, throw new IllegalStateError(subst("Relation &1 conflicts with a &2 and prevents URL request routing",
                                                                   oRelation:PublicName,
                                                                   getMemberTypeText(buffer ttMember)
                                                                  )
                                                             ).   
                   end.
                   else do:
                       create ttMember.
                       assign 
                           ttMember.key = oRelation:PublicName
                           ttMember.memberObject = oRelation
                           lAdded = true.
                
                   end.    
                   if pcRequestedMember > "" then
                       return true.
                end.   
            end.       
        end.  
        if lookup(pcRequestedmember,FixedChildren) > 0 then 
        do:
            if not can-find(ttMember where ttMember.key = pcRequestedmember) then 
            do:  
                oRelation = new DynamicRelation(pcParent,pcRequestedMember).             
                create ttMember.
                assign 
                    ttMember.key = oRelation:PublicName
                    ttMember.memberObject = oRelation
                    lAdded = true.
                
            end.    
        end.
        return LAdded.
    end method.     
    
    method protected handle GetChildBufferBySerializeName(phParent as handle, pcName as character):
        define variable i as integer no-undo.
        define variable hChild as handle no-undo.
        define variable hRelation as handle no-undo.
       
        do i = 1 to phParent:num-child-relations:
             hRelation = phParent:get-child-relation (i).
             if hRelation:recursive = false 
             and hRelation:reposition = false then
             do:
                 if hRelation:child-buffer:serialize-name = pcName then
                     return hRelation:child-buffer.
             end.      
        end.     
        return ?.
    end method.    
    
    method protected IRelation GetLink(pcParent as character, pcName as character):
        define variable oIterator as IIterator no-undo.  
        define variable oRelation       as IRelation no-undo.
        if valid-object(Links) then
        do:
            oIterator = Links:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                if oRelation:ParentName  = pcParent
                and oRelation:PublicName = pcName then
                   return oRelation.
            end.       
        end.
        if lookup(pcName,FixedChildren) > 0 then 
        do:
            return new DynamicRelation(pcParent,pcName).             
        end.
        return ?.
    end method.  
    
    method protected void DeactivateDatasetLinks(pcParent as character,pRowState as RowStateEnum,poMethod as IMethod):
        DeactivateDatasetLinks(Links, pcParent, pRowState, poMethod).
    end method.
    
    method protected void DeactivateDatasetLinks(pLinks as ICollection, pcParent as character,pRowState as RowStateEnum,poMethod as IMethod):
        define variable oIterator        as IIterator no-undo.
        define variable oRelation        as IRelation no-undo.
        define variable oDatasetRelation as IDatasetRelation no-undo.
        
        if valid-object(pLinks) then
        do:
            oIterator = pLinks:Iterator().
             
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                if  oRelation:ParentName = pcParent 
                and type-of(oRelation,IDatasetRelation) then
                do:
                    oDatasetRelation = cast(oRelation,IDatasetRelation).                    
                    oDatasetRelation:Deactivate(pRowState,if valid-object(poMethod) then poMethod:name else "").
                end.        
            end.
        end.
    end method.
    
    method protected void ThrowAlreadyExistsError(pcType as character,pcName as character):
        define buffer bMember for ttMember.  
        find bMember where bMember.key = pcName no-error.
             
        undo, throw new IllegalStateError(subst("&1 with &2 '&3' conflicts with a &4 and prevents URL request routing",
                                                pcType,
                                                if pcType = "Dataset" or pcType = "Buffer" then "serialize-name" else "name",
                                                pcName,
                                                getMemberTypeText(buffer bMember)
                                                )
                                          ).   
        
    end.    
    
    method protected character getMemberTypeText(buffer ttMember for ttMember):
        if avail ttMember then
        do:
            if valid-handle(ttMember.memberHandle) then 
            do:
                case ttMember.memberHandle:type:
                    when "buffer" then 
                    do:
                        if valid-handle(ttMember.memberHandle:parent-relation) then
                            return "child " + ttMember.memberHandle:type + " of  top buffer " + quoter(ttMember.memberHandle:parent-relation:parent-buffer:serialize-name) +  " in Datasets[1]".
                        else    
                            return ttMember.memberHandle:type + " in Datasets[1]".
                    end.  
                    otherwise 
                        return ttMember.memberHandle:type + " in Datasets[1]".
                end case.
            end.
            else if valid-object(ttMember.memberHandle) then 
            do: 
                if type-of(ttMember.memberObject,IMethod) then 
                    return cast(ttMember.memberObject,IMethod):BaseTypeName.
                else if type-of(ttMember.memberObject,IRelation) then 
                    return "Link".
            end.
            return "a bad record".
        end.
        return "another".
    end method.    
    
            
end class.