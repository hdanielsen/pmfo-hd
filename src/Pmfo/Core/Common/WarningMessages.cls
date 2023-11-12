 
 /*------------------------------------------------------------------------
    File        : WarningMessages
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 15 09:16:32 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Pmfo.Core.Common.INameService from propath.
using Pmfo.Core.Common.IWarning from propath.
using Pmfo.Core.Common.IWarningMessageTableHolder from propath.
using Pmfo.Core.Common.IWarningMessages from propath.
using Pmfo.Core.Common.Warning from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Util.DatasetUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.

class Pmfo.Core.Common.WarningMessages implements IWarningMessages, IWarningMessageTableHolder: 

    {Pmfo/Core/schema/warning.i}
    
    define public property TableHandle as handle no-undo 
        get():
            return temp-table ttWarning:handle. 
        end. 
        set. 
     
    define public property HasData as logical  no-undo 
        get(): 
            return TableHandle:has-records.
        end. 
        
    define protected property Dataset as handle no-undo get. set.   

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    define protected property NameService as INameService no-undo 
       get():
           if not valid-object(NameService) then 
           do:
               NameService = cast(Application:ServiceManager:getService(get-class(INameService)),INameService).
           end.
           return NameService.
       end.        
       set. 
    
    // use by entity with no dataset (ServiceEntity) 
    constructor public WarningMessages ( ):
        super (). 
    end constructor.
    
    constructor public WarningMessages (phDataset as handle  ):
        super ().
        this-object:Dataset = phDataset.
    end constructor.
    
    method public logical AddAll (pcResource as character,poWarnings as IWarningMessages):
         define variable oWarnings as IWarning extent no-undo.
         define variable i as integer no-undo. 
         oWarnings = poWarnings:GetWarnings().
         do i = 1 to extent(oWarnings):
             this-object:Add(pcResource,oWarnings[i]). 
         end.    
    end method.
    
    method public logical Add (pcEntity as character,  input poWarning as IWarning ):
        return this-object:Add(pcEntity, poWarning:Message, poWarning:FieldName).
    end method.
    
    // IFetchDelegator - NOT in IBusinessEntity
    method public logical Add(pcEntity as character, pcMsg as character):
        return this-object:Add(pcentity,pcMsg,"").
    end method. 
    
    method public logical Add(pcEntity as character, pcMsg as character, pcField as character):
        define buffer bWarning for ttWarning.
        define variable iSeq as integer no-undo.
        // the warnings will be retrieved from clients with append 
        // so we add this to warnings created on the client to ensure that warnings
        // created before the server all does not cause a conflict 
        define variable iClient as integer init 1000 no-undo.
        
        // TODO - subclass the variants with and without dataset  
        if pcField > "" and not valid-handle( this-object:Dataset) then 
            undo, throw new IllegalOperationError("Add with field called in WarningMessages with no dtaaset").
                     
               
        for each bWarning by bWarning.entity by bWarning.seq desc:
            iSeq = bWarning.seq.
            leave. 
        end.
            
        if iSeq < iClient then  
           iSeq = iClient.
               
        create ttWarning.
        assign
            ttWarning.entity    = pcEntity
            ttWarning.msg       = pcMsg     
            ttWarning.fieldName = pcField
            ttWarning.seq       = iSeq + 1
            .
    end method. 
   
    method public JsonArray ToJsonArray(  ):
        define variable oWarnings as JsonArray no-undo.
        define variable hFld    as handle no-undo.
        define variable hBuffer as handle no-undo.
        for each ttWarning :
            if ttWarning.fieldName > "" then
            do: 
                if not valid-handle(this-object:dataset) then 
                    undo, throw new IllegalStateError("Warning messages with field name references encountered in WarningMessages with no dataset").
                     
                if not valid-handle(hBuffer) or hBuffer:serialize-name <> ttWarning.entity then
                do: 
                    hBuffer = DatasetUtil:GetBufferBySerializeName(this-object:Dataset, ttWarning.entity).
                end.    
                hFld = ?.
                hFld = hBuffer:buffer-field(ttWarning.fieldName) no-error.
                if valid-handle(hFld) then
                    ttWarning.fieldName = hFld:serialize-name.
                else // just try something 
                    ttWarning.fieldName = NameService:PublicFieldName(ttWarning.fieldName).
                    
            end.        
        end.         
        
        oWarnings = new JsonArray().
        if not valid-handle(this-object:dataset) then 
            buffer ttWarning:buffer-field("fieldName"):serialize-hidden = true.
        
        temp-table ttWarning:write-json("JsonArray",oWarnings).
        
        if not valid-handle(this-object:dataset) then 
            buffer ttWarning:buffer-field("fieldName"):serialize-hidden = false.
        
        return oWarnings.
    end method.

    method public IWarning extent GetWarnings().
        define variable oWarnings as IWarning extent no-undo.
        for each ttWarning:
            extent(oWarnings) = (if extent(oWarnings) = ? then 0 else extent(oWarnings)) + 1.
            oWarnings[extent(oWarnings)] = new Warning(ttWarning.msg,ttWarning.fieldName). 
        end.   
        return oWarnings.   
    end method. 
    
    method public void GetWarningTableData(output table-handle phTable) .   
        GetStaticWarningTableData(output table-handle phTable by-reference).
    end method. 
    
    method protected void GetStaticWarningTableData(output table ttWarning) .        
    end method.    
    

end class.