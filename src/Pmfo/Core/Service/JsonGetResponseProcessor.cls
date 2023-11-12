 
 /*------------------------------------------------------------------------
    File        : JsonGetResponseDecorator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Apr 20 13:02:48 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.DataAdmin.IField from propath.
using OpenEdge.Net.URI from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.ResponseMetaDataEnum from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonConstruct from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.
using Pmfo.Util.JsonUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Core.Service.JsonGetResponseProcessor: 
    define public property BaseURI        as character            no-undo get. protected set.
    define public property MetaDataEnum   as ResponseMetaDataEnum no-undo get. protected set.
    
    // optimize for meta data nul
    define protected property PathList    as character no-undo get. protected set.
    define protected property PublicChildNames as character extent no-undo get. protected set.
    define protected property PublicNames as character no-undo get. protected set.
    define protected property KeyLists    as character no-undo get. protected set. 
    define protected property NavigationLinkTemplate as character no-undo init "&1@navigationLink" get. 
        
    define public property ServiceManager as IServiceManager      no-undo
        get():
            return cast(Application:ServiceManager,IServiceManager).
        end get.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public JsonGetResponseProcessor (  ):
        super ().
        MetaDataEnum = ResponseMetaDataEnum:None.
    end constructor.
    
    constructor public JsonGetResponseProcessor (pBaseUri as character, pMetaData as ResponseMetaDataEnum):
        super ().
        BaseURI = pBaseUri.
        MetaDataEnum = pMetaData. 
    end constructor.
            
    method public void processGetResponse(pJson as JsonObject,
                                          phDataset as handle,
                                          phRequestedBuffer as handle,
                                          pRequestType as RequestTypeEnum,
                                          pLinks as ICollection,
                                          pcCollections as character extent,
                                          pcKeyWheres as character extent,
                                          pcRemoveFromDisplayFields as character):
         define variable cNames         as character no-undo.
         define variable hBuffer        as handle no-undo.  
         define variable iBuffer        as integer no-undo.
         define variable cPath          as character no-undo.
         define variable iCollection    as integer no-undo. 
         define variable lUseBuffername as logical no-undo.
         define variable iRelation      as integer no-undo.
         define variable hRelation      as handle no-undo.
         define variable lAddEmpty      as logical no-undo.
         
         cNames = Array:ToString(pJson:GetNames()).
         
         // if requested buffer then we may have removed some of the outer single row json parents
         // and need to add them to the path in case someof the child data are not public
         if valid-handle(phRequestedbuffer) then
         do iCollection = 1 to extent(pcCollections) - 1: 
             cPath = cPath 
                   + "/" 
                   + pcCollections[iCollection] 
                   + if pcKeyWheres[iCollection] > "" then subst("(&1)",pcKeyWheres[iCollection])
                     else "".
         end.
         else 
            cPath = if phDataset:get-top-buffer(1):serialize-name = phDataset:serialize-name then "" else "/" + phDataset:serialize-name.
         
         do iBuffer = 1 to if valid-handle(phRequestedBuffer) then 1 else phDataset:num-top-buffers :
             if valid-handle(phRequestedBuffer) then 
                 hBuffer = phRequestedBuffer.
             else 
                 hBuffer = phDataset:get-top-buffer(IBuffer).
             if lookup(hBuffer:serialize-name,cNames) > 0 then
             do:
                 processData(if pJson:GetType(hBuffer:serialize-name) = JsonDataType:ARRAY then pJson:GetJsonArray(hBuffer:serialize-name)
                             else pJson:GetJsonObject(hBuffer:serialize-name),
                             cPath,
                             cPath,
                             hBuffer,
                             phRequestedBuffer,
                             pLinks,
                             pcCollections,
                             pcKeyWheres,
                             pcRemoveFromDisplayFields)
                             .
             end.
             else // add empty array if no data - this is typicaly expected by clients  
             do:
                      
                 laddEmpty = true.
                 
                 do iRelation = 1 to phdataset:num-relations:
                     hRelation = phDataset:get-relation(iRelation).
                     // don't add if the top-buffer has a deactivated relation 
                     if hRelation:active = false and hRelation:child-buffer = hBuffer then
                     do:
                         lAddEmpty = false.
                         leave.
                     end.                            
                 end.    
                 if lAddEmpty then
                 do:
                   
                    if RequestTypeEnum:SingleRow:Equals(pRequestType) then
                        pJson:AddNull(hBuffer:serialize-name).
                    else if RequestTypeEnum:MultiTableHideEmpty:Equals(pRequestType) = false  then
                        pJson:Add(hBuffer:serialize-name,new JsonArray()).
                           
                 end.    
             end.
         end.
    end.    
    
    method protected void processData     (pJson    as JsonConstruct,
                                           pcPath   as character,
                                           pcLinkPath as character,
                                           phBuffer as handle,
                                           phRequestedBuffer as handle,
                                           pLinks as ICollection,
                                           pCollections as character extent,
                                           pKeyWheres as character extent,
                                           pcRemoveFromDisplayFields as character):
             
        define variable cKeys        as character extent  no-undo.
        define variable iJson        as integer no-undo.
        define variable cAblType     as character no-undo.
        define variable iKey         as integer no-undo.
        define variable cDlm         as character no-undo.
        define variable cField       as character no-undo.
        define variable hFld         as handle no-undo.
        define variable oJson        as JsonObject no-undo.
        define variable cKeyValue    as character no-undo.
        define variable cKeyExpression as character no-undo.
        define variable cId            as character no-undo.
        define variable iChild         as integer no-undo.
        define variable hChildBuffer   as handle no-undo. 
        define variable cNames         as character no-undo.
        define variable cKeyList       as character no-undo.
        define variable cRelationFields    as character no-undo.
        define variable oIterator          as IIterator no-undo.
        define variable oRelation          as IRelationDefinition no-undo. 
       
      
        define variable iField             as integer   no-undo.
        define variable cLinkName          as character no-undo.
        define variable cLinkValue         as character no-undo.
        define variable lEditable          as logical   no-undo.
        define variable cPublicname        as character no-undo.
        define variable hRelation          as handle    no-undo.
        define variable lPublic            as logical   no-undo.
        define variable oDetailURI         as URI       no-undo.
        define variable cLinkId            as character no-undo.
        define variable oJsonArray         as JsonArray no-undo.
        define variable iFieldPos          as integer no-undo.
        define variable iPath              as integer no-undo.
        define variable cChildFields       as character no-undo.
        define variable cChildKeys         as character extent no-undo.
        define variable oRelDef            as IRelationDefinition no-undo.
         
        cPublicname = phBuffer:serialize-name.
        
        if MetaDataEnum <> ResponseMetaDataEnum:None then
        do:                                              
            // this is an attempt to avpid calling variius methuds to get a key for every record
            // it is a messy and should be cleaned up  
            // if the table is exposed through a public resource then we don't need the path in the id
            // CanFindPublicBusinessEntity is expensive so keep track in Publicnames to avoid checking for each node */ 
            
            if lookup(cPublicName,PublicNames) > 0 then 
            do: 
                pcPath = "/" + phBuffer:serialize-name. 
                lPublic = true.
                // for now use no-error
                cKeyList = entry(lookup(cPublicName,PublicNames,","),Keylists,";") no-error.
                
            end.    
            if cKeylist = ""    
            and valid-object(Servicemanager) 
            and ServiceManager:CanFindPublicBusinessEntity(input cPublicname) then 
            do: 
                do on error undo, throw:
                    cKeyList =  ServiceManager:GetKeyList(phBuffer:serialize-name).
                    catch e as NotFoundError :
                       cKeylist = "". // some serialize-names may ionly exist on datalayer      
                    end catch.
                end.
                PublicNames = left-trim(PublicNames + "," + cPublicname,",").
                KeyLists    = left-trim(KeyLists + ";" + cKeyList,";").
                pcPath = "/" + phBuffer:serialize-name. 
                lPublic = true.
            end.
            else      
            do: 
                // todo store this for non opublci names also  
                do on error undo, throw:
                    cKeyList =  ServiceManager:GetKeyList(phBuffer:serialize-name).
                                cKeyList =  ServiceManager:GetKeyList(phBuffer:serialize-name).
                    catch e as NotFoundError :
                           cKeylist = "". // some serialize-names may ionly exist on datalayer      
                    end catch.
                end.
                if cKeyList = "" and valid-handle(phBuffer:parent-relation) then 
                    cKeyList = GetChildKeys(phBuffer:parent-relation,cKeylist).
                
                if cKeyList = "" then
                   cKeyList = ServiceInfo:GetChildKeys(phbuffer).
                
                pcPath = pcPath + "/" + phBuffer:serialize-name.
                 
            end.
        
            // for links we currently do not check if the public child has the same link so we need to keep the path  
            pcLinkPath = pcLinkPath + "/" + phBuffer:serialize-name.
         
            if cKeyList <> "rowid" and cKeyList > "" then
            do:
                cKeys = Array:Split(cKeyList).
                // correct casing 
                do iKey = 1 to extent(cKeys):
                    hFld = phBuffer:buffer-field(cKeys[iKey]).
                    cKeys[iKey] = hFld:serialize-name.
                end.
            end.
            
            lEditable = valid-handle(phBuffer:before-buffer).
        end.
        else  
            pcPath =  pcPath + "/" + phBuffer:serialize-name.
            
        if type-of(pJson,JsonArray) then
        do:
            oJsonArray = cast(pJson,JsonArray).
        end.    
        else if type-of(pJson,JsonObject) then
            ojson     = cast(pJson,JsonObject).
        
        Loop:
        do iJson = 1 to if valid-object(oJsonArray) then oJsonArray:length else 1:
            if valid-object(oJsonArray) then
            do:
                if oJsonArray:GetType(iJson) = JsonDataType:ARRAY then 
                do:
                     processData(oJsonArray:GetJsonArray(iJson),
                                          pcPath, 
                                          pcLinkPath,
                                          phBuffer,
                                          phRequestedBuffer,
                                          pLinks,
                                          pCollections,
                                          pKeyWheres,
                                          pcRemoveFromDisplayFields)
                                         . 
                     return.
                end.    
                else 
                assign 
                    ojson = oJsonArray:GetJsonObject(iJson).
            end.    
             
            // allow null response for find with no-error
            if valid-object(oJson) then
            do: 
                cNames         = Array:ToString(oJson:GetNames()).
                if MetaDataEnum <> ResponseMetaDataEnum:None then
                do:
                    assign
                        cKeyValue      = ""
                        cKeyExpression = ""
                        cDlm           = ""
                        iKey           = 0
                        .   
                    keyloop:
                    do iKey = 1 to extent(cKeys):
                        do on error undo, throw:
                            if oJson:GetType(cKeys[iKey]) = JsonDataType:NULL then
                                cKeyValue = "null". 
                            else if oJson:GetType(cKeys[iKey]) = JsonDataType:STRING then
                                cKeyValue = quoter(ojson:GetCharacter(cKeys[iKey]),"'"). 
                            else // resolves int64 versus decimal and logcial true/false
                                cKeyValue = JsonUtil:GetStringValue(oJson, cKeys[iKey]).
                                
                            catch e2 as Progress.Lang.Error :
                               // if no key in json just give up, but don't throw error
                               // (typically this would be private classes made public for test) 
                               
                               leave keyLoop.             
                           end catch.
                        
                        end.
                        
                        if extent(cKeys) > 1 then  
                        do:
                            assign 
                                cKeyExpression = cKeyExpression        
                                               + cDlm
                                               + subst("&1=&2",cKeys[iKey],cKeyValue)           
                                cDLm = ","
                                .
                        end.
                        else 
                            cKeyExpression = cKeyValue.
                    end. // ikey loop
                    
                    if cKeyExpression > "" then 
                    do:           
                        cKeyExpression = URI:EncodePath(cKeyExpression).
                        cId     = subst("&1(&2)",pcPath,cKeyExpression).
                        cLinkId = subst("&1(&2)",pcLinkPath,cKeyExpression).
                    end.
                    if valid-object(pLinks) then 
                    do:
                        oIterator = pLinks:Iterator().
                        do while oIterator:HasNext().
                            oRelation = cast(oIterator:Next(),IRelationDefinition).
                            if valid-object(oRelation) and oRelation:ParentName = phBuffer:serialize-name  then 
                            do: 
                                // don't add link if it is expanded 
                                if oJson:Has(oRelation:PublicName) = false then
                                do:
                                   
                                    if MetaDataEnum:IsFlagSet(ResponseMetaDataEnum:NavigationLinks) 
                                    and type-of(oRelation,IRelation) then
                                    do:
                                        assign
                                            cLinkName   = subst(NavigationLinkTemplate,oRelation:PublicName)
                                            cLinkValue  = cast(oRelation,IRelation):GetPublicLink(oJson,phBuffer)
                                           .
                                      
                                        if cLinkValue > "" then
                                        do: 
                                            if index(cLinkValue,"(") > 0 then  
                                                cLinkValue  = URI:EncodePath(cLinkValue).
                                           
                                            // for public and not onetomany add the direct link     
                                            if cast(oRelation,IRelation):Public and not RelationTypeEnum:OneToMany:Equals(oRelation:Cardinality)  then 
                                                cLinkValue = subst("&1/&2",BaseURI,cLinkValue).
                                            else 
                                                cLinkValue = subst("&1&2/&3",BaseURI,cLinkId,cLinkValue).
                                   
                                            oJson:Add(cLinkName,cLinkValue).
                                               
                                        end.
                                    end.
                                end.
                                else do:
                                    if RelationTypeEnum:OneToMany:Equals(oRelation:Cardinality) = false then
                                    do:
                                       ConvertToObject(oJson, oRelation:PublicName).
                                    end. 
                                end.        
                            end.
                        end. //  do while oIterator:HasNext(). 
                    end.
                end.   // if MetaDataEnum <> ResponseMetaDataEnum:None then            
                else do:
                    // do this only once per path for performance
                    if lookup(pcPath,PathList) = 0 then
                    do:
                        PathList = left-trim(PathList + "," + pcPath,",").
                        iPath = lookup(pcPath,PathList).
                        extent(PublicChildNames) = iPath.
                        //  plink is not valid when ResponseMetaDataEnum:None  
                        if valid-object(pLinks) then 
                        do:
                            oIterator = pLinks:Iterator().
                            do while oIterator:HasNext().
                                oRelation = cast(oIterator:Next(),IRelationDefinition).
                                
                                if valid-object(oRelation) and oRelation:ParentName = phBuffer:serialize-name  then
                                do:
                                    // don't add link if it is expanded
                                    // TODO - this is wrong in cases where the relation may be present later
                                    if oJson:Has(oRelation:PublicName) and RelationTypeEnum:OneToMany:Equals(oRelation:Cardinality) = false then
                                    do:
                                        PublicChildNames[iPath] = left-trim(PublicChildNames[iPath] + "," + oRelation:PublicName,",").
                                    end.
                                end.
                            end.
                        end.
                    end.
                end.
                do iField = 1 to num-entries(pcRemoveFromDisplayFields):
                    cField = entry(iField,pcRemoveFromDisplayFields).
                    if num-entries(cField,".") = 2 then
                    do:
                        if entry(1,cField,".") = phBuffer:serialize-name then
                        do:
                            cField = entry(2,cField,".").
                           
                                                    
                            // check if field is still there 
                            // TODO = This was encoutered with XREF links so this may be a fix of aymptom)
                            // ensure correct casing by using the field name from the json:GetNames 
                            
                            iFieldPos = lookup(cField,cNames).
                            if iFieldPos > 0 then 
                            do: 
                                cField = entry(iFieldPos,cNames).
                                oJson:Remove(cField).
                            end.
                        end.
                    end.
                    else do:
                        if (not valid-handle(phRequestedBuffer) and left-trim(pcPath,"/") = pCollections[1]) 
                           or (valid-handle(phRequestedBuffer) and phBuffer:name = phRequestedBuffer:name) then
                        do:
                            // ensure correct casing 
                            cField = entry(lookup(cField,cNames),cNames).
                            oJson:Remove(cField).
                        end.
                    end.    
                end.
                do iChild = 1 to phbuffer:num-child-relations:
                    hRelation = phBuffer:get-child-relation (iChild).
                    // there will be no child node in json for repos
                    if hRelation:reposition = false then
                    do:
                        hChildBuffer = phBuffer:get-child-relation (iChild):child-buffer.
                        if lookup(hChildBuffer:serialize-name,cNames) > 0 then
                        do:
                             if MetaDataEnum = ResponseMetaDataEnum:None then
                             do:
                                iPath = lookup(pcPath,PathList).
                                if lookup(hChildbuffer:serialize-name,PublicChildNames[iPath]) > 0 then
                                do:
                                   ConvertToObject(oJson, hChildbuffer:serialize-name) .
                                end.
                                else do: // the optimization is not reliable in cases for links that does not have data on the first check  
                                    oRelDef = GetRelation(pLinks, hRelation:parent-buffer:serialize-name,hRelation:child-buffer:serialize-name).
                                    if valid-object(oRelDef) and RelationTypeEnum:OneToMany:Equals(oRelDef:Cardinality) = false then
                                    do:
                                        ConvertToObject(oJson, hChildbuffer:serialize-name) .
                                        PublicChildNames[iPath] = left-trim(PublicChildNames[iPath] + "," + hRelation:child-buffer:serialize-name,",").
                                    end.
                                end.     
                             end.
                             processData(if oJson:GetType(hChildBuffer:serialize-name) = JsonDataType:ARRAY 
                                          then oJson:GetJsonArray(hChildBuffer:serialize-name)
                                          else oJson:GetJsonObject(hChildBuffer:serialize-name),
                                          cId, 
                                          cLinkId,
                                          hChildBuffer,
                                          phRequestedBuffer,
                                          pLinks,
                                          pCollections,
                                          pKeyWheres,
                                          pcRemoveFromDisplayFields)
                                         .
                             
                        end.  
                        else if hRelation:nested and hRelation:active then // add empty array or null object if no data - this is typicaly expected by clients - null errors is common if not
                        do: 
                           AddEmptyChild(ojson,hRelation,pLinks).
                        end.    
                    end.        
                end. // ichild loop
                
                // add this after chilkd collection so that the order is consistent in case where an empty array was added
                if cId > "" then
                do:
                    if MetaDataEnum:IsFlagSet(ResponseMetaDataEnum:Id) then
                       oJson:Add("@id",subst("&1",cId)).
                    
                    // for now don't add link on private reposition tables 
                    if lPublic = true or not valid-handle(phBuffer:parent-relation) or phBuffer:parent-relation:reposition = false then 
                    do:
                        if MetaDataEnum:IsFlagSet(ResponseMetaDataEnum:ReadLink) then
                            oJson:Add("@readLink",subst("&1&2",BaseURI,cId)).
                        if lEditable then do:
                            if MetaDataEnum:IsFlagSet(ResponseMetaDataEnum:EditLink) then
                               oJson:Add("@editLink",subst("&1&2",BaseURI,cId)).
                        end.       
                    end.
                end.    
            end.     
        end.
                                      
    end method.
    
    method protected void ConvertToObject(pJson as JsonObject, pcName as character):
        define variable oJsonArray as JsonArray no-undo.
         
        if pJson:Has(pcname) then
        do:  
            if pJson:GetType(pcName) = JsonDataType:ARRAY then
            do:
                oJsonArray = pJson:GetJsonArray(pcName).
              
                if oJsonArray:length = 1 then 
                    pJson:Set(pcName,oJsonArray:GetJsonObject(1)).
                else if oJsonArray:length = 0 then
                    pJson:SetNull(pcName).
            end.
        end.  
         
    end method.
    
    method protected character GetChildKeys(phRelation as handle, pcKeylist as character):
        define variable cChildFields    as character no-undo.
        define variable cNewChildFields as character no-undo. 
        define variable iCount as integer no-undo.
        define variable i as integer no-undo.
        define variable cChildField as character no-undo.
        if valid-handle(phRelation) then 
            cChildFields = BufferUtil:GetChildJoinFields(phRelation).
        if cChildFields > "" then 
        do: 
            cNewChildFields = pcKeylist.
            do i = 1 to num-entries(cChildFields):
               cChildField = entry(i,cChildFields).
               if lookup(cChildField,cNewChildFields) > 0 then
                   cNewChildFields = StringUtil:RemoveEntryFromList(cChildField, cNewChildFields).
               else // if any of the fields are mnssign return the input list
                   return pcKeylist.    
            end.    
            return cNewChildFields.    
        end.     
        return pcKeylist.
    end method.    
    
    method protected void AddEmptyChild(pjson as JsonObject,phRelation as handle, pLinks as ICollection):
        define variable oRelDef   as IRelationDefinition no-undo.
       
        oRelDef = GetRelation(pLinks, phRelation:parent-buffer:serialize-name,phRelation:child-buffer:serialize-name).
        if valid-object(oRelDef) = false or RelationTypeEnum:OneToMany:Equals(oRelDef:Cardinality) then
            pJson:Add(phRelation:child-buffer:serialize-name,new JsonArray()).
        else 
            pJson:AddNull(phRelation:child-buffer:serialize-name).
           
        // remove link if (empty) node was added 
        if pJson:Has(subst(NavigationLinkTemplate,phRelation:child-buffer:serialize-name)) then
            pJson:remove(subst(NavigationLinkTemplate,phRelation:child-buffer:serialize-name)).
        
    end method.
    
    method protected IRelationDefinition GetRelation(pLinks as ICollection, pcParent as character, pcPublicName as character):
        define variable oIterator as IIterator no-undo.
        define variable oRelDef   as IRelationDefinition no-undo.
        if valid-object(pLinks) then
        do:
            oIterator = pLinks:Iterator().
            do while oIterator:HasNext().
                oRelDef = cast(oIterator:Next(),IRelationDefinition).
                if  oRelDef:ParentName = pcParent
                and oRelDef:PublicName = pcPublicName then
                   return oRelDef.
            end.
        end. 
        return ?.
    end method.
      
end class.