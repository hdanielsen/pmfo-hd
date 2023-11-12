 
 /*------------------------------------------------------------------------
    File        : JsonDatasetErrorProcessor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jun 09 16:58:54 EDT 2019
    Notes       : As of currren the caller must turn off foreign-key-hidden to get keys in json errors 
                  TODO: encapsulate here and pass data set instead of json                   
   ---------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.ClientDataError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.ErrorStringUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonDataType from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.AppError from propath.
using Progress.Lang.Error from propath.
using Pmfo.Core.Error.NotFoundError from propath.

class Pmfo.Core.Service.JsonDatasetErrorProcessor: 
    
    define protected property Error as AppError no-undo get. set.
    
    define protected variable JsonErrors as JsonObject no-undo.
    
    // the serialize-name is for import 
    define temp-table ttError 
        field prodsId    as character serialize-name "prods:id"
        field prodsError as character serialize-name "prods:error"
       
     index idx as primary unique prodsId.   
     
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public JsonDatasetErrorProcessor (  ):
        super ().
    end constructor.
    
    method public Error CreateDeleteError(pjson as JsonObject, phdataset as handle ):
        return CreateSimpleError(pjson, phdataset ).
    end method.
    
    method public Error CreateSimpleError(pjson as JsonObject, phdataset as handle ):
        define variable iMsg       as integer no-undo.
        define variable cFields    as character extent no-undo.
        define variable cTypes     as character extent no-undo.
        define variable iIds       as integer  extent no-undo.
        define variable cMessages  as character extent no-undo.
        
        if pJson:has("prods:errors") then
        do: 
            JsonErrors = pJson:GetJsonObject("prods:errors"). 
            LoadErrors(JSOnErrors).
            
        end.
        for each ttError:
            extent(cMessages) = ?.
            extent(cTypes) = ?.
            extent(iIds) = ?.
            extent(cFields) = ?.
            
            cMessages = ErrorStringUtil:GetErrors(ttError.prodsError, output cTypes, output iIds,  output cFields).
            do iMsg = 1 to extent(cMessages):
                if not valid-object(this-object:Error) then
                    this-object:Error = new BadRequestError(cMessages[iMsg]).
                else 
                    this-object:Error:AddMessage(cMessages[iMsg],?).
            end.
        end.
        if not valid-object(this-object:Error) then 
            this-object:Error = ProcessBeforeBuffers(phDataset).
        if not valid-object(this-object:Error) then 
           this-object:Error = CreateGenericError(phDataset). 
           
        return this-object:Error.
 
         
    end method.
    
    method public Error CreateError(pjson as JsonObject, phdataset as handle ):
        define variable cNames  as character extent no-undo.
        define variable i       as integer no-undo.
        define variable hBuffer as handle no-undo.
        define variable oJsonTable as JsonArray  no-undo.
        define variable oJsonNewTable as JsonArray  no-undo.
        define variable oJsonResponse as JsonObject no-undo.
        define variable iCountTop as integer no-undo.
        define variable cNewNames as character no-undo extent.
       
                 
        if pJson:has("prods:errors") then
        do:
            LoadErrors(pJson:GetJsonObject("prods:errors")).
        end.
        cNames = pJson:GetNames().
        oJsonResponse = new JsonObject().
        do i = 1 to extent(cNames):
            if not cNames[i] begins "prods:" and not cNames[i] begins "@" then
            do:
                hBuffer = DatasetUtil:GetBufferBySerializeName(phdataset,cNames[i]).
                if valid-handle(hBuffer) then
                do:
                    oJsonTable = pJson:GetJsonArray(cnames[i]).
                    ProcessTable(oJsonTable,hBuffer,?,"").
                end.  
            end.    
        end.
        if not valid-object(this-object:Error) then 
            this-object:Error = ProcessBeforeBuffers(phDataset).
        if not valid-object(this-object:Error) then 
           this-object:Error = CreateGenericError(phDataset). 
        return this-object:Error.
    end method.
    
    method protected void ProcessTable(pjsonArray as JsonArray, phBuffer as handle,poParent as JsonObject, pcPArentUrl as character):
        define variable i as integer no-undo.
        define variable cKeys as character no-undo.
        
        do on error undo, throw:
           cKeys = ServiceInfo:GetPublicKeys(phBuffer).
           catch e as NotFoundError :
               // there are some cases that only have temp-table 
               // the ServiceInfo will log the case, but for now just use the buffer keys for error handling
               cKeys = BufferUtil:GetInternalKeys(phBuffer).    
           end catch.
        end.
        do i = 1 to pjsonArray:length:
            if pJsonArray:GetType(i) = JsonDataType:object then
            do: 
                ProcessRow(pJsonArray:GetJsonObject(i),phBuffer,i,cKeys,poParent,pcPArentUrl).
            end.         
        end.
    end method.
    
    // Used when we can't find errors in the dataset typically due to orphaned buffers due to bad foreign fields 
    // Note that the missing joins likely indicates a programming error. The temp-tables are no-undo, so joins should 
    // not be lost even if the transaction was undone       
    method protected AppError ProcessBeforeBuffers(phDataset as handle):
        define variable i as integer no-undo.
        define variable oError as AppError no-undo. 
        oError = new BadRequestError("Update of data failed with the following error message(s). The information about which record and field that caused the error(s) was not returned to the client. Please Contact IT.").
        do i = 1 to phDataset:num-buffers:
            AddBeforeBufferErrors(oError, phDataset:get-buffer-handle (i):before-buffer,1).
            AddBeforeBufferErrors(oError, phDataset:get-buffer-handle (i):before-buffer,2).
            AddBeforeBufferErrors(oError, phDataset:get-buffer-handle (i):before-buffer,3).
        end.  
        if oError:NumMessages > 1 then 
            return oError.
        return ?.  
    end method.
    
    method protected void AddBeforeBufferErrors(pError as AppError, phBefore as handle, pState as integer):
        define variable hQuery as handle no-undo.
        define variable cMessages as character extent no-undo.  
        define variable iErrorNums as integer extent no-undo.
        define variable iError as integer no-undo.
        define variable iNum as integer no-undo.  
        create query hQuery.
        if valid-handle(phBefore) then 
        do:
            hQuery:add-buffer(phBefore).
            hQuery:query-prepare(subst("for each &1 where row-state(&1) = &2",phBefore:name,pState)).
            hQuery:query-open ().
            hQuery:get-first().
            do while phBefore:avail:
                if phBefore:error then
                do: 
                     extent(cmessages) = ?.
                     extent(iErrorNums) = ?.
                     cMessages = ErrorStringUtil:GetErrors(phBefore:error-string, output iErrornums).
                     do iError = 1 to extent(cMessages):
                         if iErrorNums[iError] = 0  or iErrorNums[iError] = ? then iErrorNums[iError] = GetErrorNumFromMessage(cMessages[iError]).  
                         pError:AddMessage(subst("&1 failed: &2",StringUtil:UpperCaseFirst(BufferUtil:GetRowStateStringPassive(pState)),cMessages[iError]),iErrorNums[iError]). 
                     end.        
                end.
                hQuery:get-next().
            end.    
        end.
        finally:
            delete object hQuery.        
        end finally.  
    end.          
    
    // the last desperate attempt if no buffers has error 
    method protected AppError CreateGenericError(phDataset as handle):
        define variable cMode as character no-undo.
        if DatasetUtil:HasBeforeState(phDataset,row-created) then
            cMode = BufferUtil:GetRowStateStringPassive(row-created).
        if DatasetUtil:HasBeforeState(phDataset,row-modified) then
            cMode = cMode
                  + (if cMode = "" then "" else ",")
                  + BufferUtil:GetRowStateStringPassive(row-modified).
        if DatasetUtil:HasBeforeState(phDataset,row-deleted) then
            cMode = cMode
                  + (if cMode = "" then "" else ",")
                  + BufferUtil:GetRowStateStringPassive(row-deleted).
         else
             cMode = "Update".
        // NOTE to DEV: If the request ends up here then you should find out why and fix it! 
        
        return new ApplicationError(subst("&1 of data failed. The actual error messages was not returned to client. Please Contact IT. The details may be in the log.",StringUtil:GetMessageFromEntries(cMode))).
    end method.
    
    method protected JsonObject GetJsonKeys(pJson as JsonObject,phBuffer as handle,pcKeys as character, output pcStringKEy as character):
        define variable oJsonKeys as JsonObject no-undo.
        define variable i as integer no-undo.
        define variable cKey as character no-undo.
        define variable lAnyAdded as logical no-undo.
        define variable dVal as decimal no-undo.
        define variable cKeyTmpl as character no-undo.
        define variable cStringKeytmpl as character no-undo.
        oJsonKeys = new JsonObject().
        if num-entries(pcKeys) = 1 then cKeyTmpl = "&2".
        else cKeyTmpl =  "&1=&2".
        do i = 1 to num-entries(pckeys):
            cKey = entry(i,pcKeys).
            if i > 1 then
                pcStringKey = pcStringKey + ",". 
            if pJson:Has(cKey) then 
            do: 
                lAnyAdded = true.
                case pJson:GetType(cKey):
                    when JsonDataType:NULL then 
                    do:
                        
                        pcStringKey = pcStringKey + subst(cKeyTmpl,cKey,"null"). 
                        
                        oJsonKeys:AddNull(cKey).
                    end.    
                    when JsonDataType:BOOLEAN then
                    do: 
                        oJsonKeys:Add(cKey,pJson:GetLogical(cKey)).
                        pcStringKey = pcStringKey + subst(cKeyTmpl,cKey,pJson:GetLogical(cKey)). 
                    end. 
                    when JsonDataType:NUMBER then
                    do: 
                        dVal = pJson:GetDecimal(cKey).
                        if dVal = int(dVal) then
                        do: 
                            oJsonKeys:Add(cKey,int(dVal)).
                           pcStringKey = pcStringKey + subst(cKeyTmpl,cKey,int(dVal)). 
                        end.    
                        else do:
                            oJsonKeys:Add(cKey,dVal).
                            pcStringKey = pcStringKey + subst(cKeyTmpl,cKey,dVal). 
                        end.
                    end. 
                    when JsonDataType:STRING then 
                    do: 
                        oJsonKeys:Add(cKey,pJson:GetCharacter(cKey)).
                        
                        pcStringKey = pcStringKey + subst(cKeyTmpl,cKey,quoter(pJson:GetCharacter(cKey)),"'"). 
                    end.
                     
                end.
            end.
        end. 
        if lanyAdded then
        do:
            pcStringKey = subst("&1(&2)",phBuffer:serialize-name,pcStringKey) .
        
            return oJsonKeys.
        end.
        else
            return ?.         
    end.    
    
    method protected void ProcessRow(pjson as JsonObject, phBuffer as handle, piIndex as integer, pcKeys as character, poParent as JsonObject, pcParentUrI as character):
        define variable iChild as integer no-undo.
        define variable cErrorKey as character no-undo.
        define variable hRelation as handle no-undo.
        define variable hChildBuffer as handle no-undo.
        define variable oJsonTable as JsonArray  no-undo.
        define variable oChildParent as JsonObject no-undo.
        define variable oJsonKey as JsonObject no-undo.
        define variable cUrlid as character no-undo.
        
        oJsonKey = GetJsonKeys(pJson, phBuffer, pcKEys, output cUrlId). 
        
        if pcParentUrI > "" then cUrlid = pcParentUrI + "/" + cUrlid.
        if pJson:Has("prods:id") then 
        do:
            cErrorKey = pJson:GetCharacter("prods:id").
            AddErrors(poParent, piindex,phBuffer:serialize-name,cErrorKey,oJsonKey, cUrlId).
        end.
        do iChild = 1 to phbuffer:num-child-relations:
            hRelation = phBuffer:get-child-relation (iChild).
                // there will be no child node in json for repos
            if hRelation:reposition = false then
            do:
                
                hChildBuffer = phBuffer:get-child-relation (iChild):child-buffer.
                if pJson:Has(hChildBuffer:serialize-name) then
                do:
                    if pJson:GetType(hChildBuffer:serialize-name) = JsonDataType:ARRAY then 
                    do:
                        oJsonTable = pJson:GetJsonArray(hChildBuffer:serialize-name).
                        oChildParent = new JsonObject().
                        oChildParent:Add("entity",phBuffer:serialize-name).
                        oChildParent:Add("index",piIndex).
                        oChildParent:Add("key",oJsonKey).
                        oChildParent:Add("urlId",cUrlId).
                        
                        if valid-object(poParent) then
                            oChildParent:Add("parent",poParent). 
                        ProcessTable(oJsonTable,hChildBuffer,oChildParent,cUrlid). 
                    end.  
                end.        
            end. // ichild loop
        end.           
    end method.
    
    method protected integer GetErrorNumFromMessage(pcMessage as character):
        define variable iMsg as integer no-undo.
        define variable ipos as integer no-undo.
        ipos = r-index(pcMessage ,"(").
        if ipos > 0 then
        do on error undo, throw:
            iMsg = integer(StringUtil:ExtractParenthesis(substr(pcMessage ,iPos - 1),true)).
            catch e as Error :
            // nothing    
            end catch.
        end.
        return iMsg.    
    end method.    
    
    method protected void AddErrors(poParent as JsonObject, piIndex as integer, pcEntity as char,  pcProdsID as character, pJsonId as JsonObject, pcUrlId as character):
        define variable iMsg       as integer no-undo.
        define variable cFields    as character extent no-undo.
        define variable cTypes     as character extent no-undo.
        define variable iIds       as integer  extent no-undo.
        define variable cMessages  as character extent no-undo.
        define variable oJson      as JsonObject no-undo.
        define variable iNum       as integer no-undo.
        define variable clong      as longchar no-undo.
     
        find ttError where ttError.prodsId = pcProdsID no-error.
        if avail ttError then
        do:
            cMessages = ErrorStringUtil:GetErrors(ttError.prodsError, output cTypes, output iIds,  output cFields).
            
            do iMsg = 1 to extent(cMessages):
                ojson = new JsonObject().
                oJson:Add("errorType",cTypes[iMsg]).
                oJson:Add("errorMsg",cMessages[iMsg]).
                inum = IIds[iMsg].
               
                if iNum = 0 or iNum = ? then
                    iNum = GetErrorNumFromMessage(cMessages[IMsg]).
                oJson:Add("errorNum",iNum).
                
                if valid-object(poParent)  then 
                    oJson:Add("parent",poParent).
                oJson:Add("entity",pcEntity).
                if valid-object(pJsonId) then 
                    oJson:Add("key",pJsonId).
                if pcUrlId > "" then 
                    oJson:Add("urlId",pcUrlId).
                
                oJson:Add("index",piIndex).
                oJson:Add("property",cFields[iMsg]).
                 
                oJson:Write(cLong).
                if not valid-object(this-object:Error) then 
                    this-object:Error = new ClientDataError(string(cLong),iNum).
                else
                    this-object:Error:AddMessage(string(clong),iNum). 
            end.
        end.
      
    end method.    
    
    method public void  LoadErrors(pjson as JsonObject):
        define variable oJsonErrors as JsonArray no-undo.
        define variable cNames as character extent no-undo.
        define variable i as integer no-undo.
        
        cNames = pjson:GetNames().
        do i = 1 to extent(cNames) 
        on error undo, throw:
            oJsonErrors = pJson:GetJsonArray(cNames[i]).
            temp-table ttError:serialize-name = cNames[i].
            temp-table ttError:read-json("JsonArray",oJsonErrors,"append").
            catch e as Progress.Lang.Error :
               if e:GetMessageNum(1) = 132 then
               do:
                   // this means that there are more than one node with the same name
                   // this may happen with multiple grand children 
                   // in order to read the right one 
                   // - delete one (the first) and subtract 
                   pJson:Remove(cNames[i]).
                   i = i - 1.
               end.    
               else undo, throw e.
               
                    
            end catch. 
        end.   
             
    end method.
         
end class.