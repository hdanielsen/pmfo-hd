 
 /*------------------------------------------------------------------------
    File        : GetInternalDataDelegate
    Purpose     : Delegate for operation on internal data
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 4 mar 2024 Moved from 
                 - BE  - get rid of big and rarely used stuff) 
                 - BufferUtil - get rid of BE and Service references for separation of concerns    
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Pmfo.Core.BusinessLogic.Delegate.IGetInternalDataDelegate from propath.
using Pmfo.Core.BusinessLogic.Delegate.IGetInternalDataDelegator from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntityFinder from propath.
using Pmfo.Core.Common.IKey from propath.
using Pmfo.Core.BusinessLogic.BusinessEntityFinder from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Core.Error.NotFoundError from propath.

class Pmfo.Core.BusinessLogic.Delegate.GetInternalDataDelegate implements IGetInternalDataDelegate: 

    method public logical CompareAndRefreshCurrentSelf( input pDelegator as IGetInternalDataDelegator, input phBuffer as handle, input pcRefreshFields as character, input pcTooLateFields as character, output pcRefreshInfo as character ):
        define variable oBE       as IBusinessEntity  no-undo.
        define variable hQuery    as handle no-undo.
        define variable oQuery    as QueryGroup no-undo.
        define variable oRowQuery    as QueryGroup no-undo.
        define variable oFindQuery as QueryGroup no-undo.
        define variable iKey      as integer no-undo.
        define variable cKeyField as character no-undo.
        
        define variable iFld      as integer no-undo.
        define variable hCurrentBuffer as handle no-undo.
        define variable hCurrentTable  as handle no-undo.
        define variable cIdTexttmpl    as character no-undo.
        define variable cFieldname     as character no-undo.
        define variable lRefreshed     as logical no-undo.
        define variable iRowcount      as integer no-undo.
        define variable cKeys          as character no-undo.
        
        if valid-handle(phBuffer) = false or phBuffer:type <> "buffer" then 
            undo, throw new IllegalArgumentError("Invalid buffer"). 
               
        create query hQuery.
        hQuery:add-buffer (phBuffer).
        hQuery:query-prepare(subst("for each &1",phBuffer:name)).
        hQuery:query-open().
        hQuery:get-first.
        oQuery = new QueryGroup().
        
        cKeys = pDelegator:KEYS.
        do while phBuffer:avail:
           iRowcount = iRowCount + 1.
           oRowQuery = new QueryGroup().
           do iKey = 1 to num-entries(cKeys):
               cKeyField = entry(iKey,cKeys).
               oRowQuery:AddEntry(cKeyField,Ccs.BusinessLogic.QueryOperatorEnum:Eq, phBuffer:buffer-field(cKeyField):buffer-value).
           end.   
           if iRowcount > 1 then 
               oRowQuery:Join = Ccs.BusinessLogic.JoinEnum:Or.  
           oQuery:AddEntry(oRowQuery).
            
           hQuery:get-next.
        end.
        // if the dataset has the internal table use a different BE  
        if pDelegator:DefaultDataset:num-top-buffers = 1 
        and phBuffer:table-handle = pDelegator:DefaultDataset:get-buffer-handle (1) then
        do:
            oBE = cast(Application:ServiceManager,IServiceManager):CreateBusinessEntity(pDelegator:Resource).
            oBe:GetData(oQuery,output table-handle hCurrentTable).
            hCurrentBuffer = hCurrentTable:default-buffer-handle.
        end.
        else do:  
            pDelegator:GetData(oQuery).
            hCurrentBuffer = pDelegator:DefaultDataset:get-buffer-handle (1).
        end. 
        
        hQuery:get-first.
        
        do while phBuffer:avail on error undo, throw:
            oFindQuery = new QueryGroup(). 
            do iKey = 1 to num-entries(cKeys):
                cKeyField = entry(iKey,cKeys).
                oFindQuery:AddEntry(cKeyField,Ccs.BusinessLogic.QueryoperatorEnum:EQ,phBuffer:buffer-field(cKeyfield):buffer-value).
            end.  
            do on error undo, throw:
                hCurrentBuffer:find-unique(subst("where &1",oFindQuery:ToString() )).
                catch e as Progress.Lang.Error :
                    undo, throw new IllegalStateError(subst("&1 where &2 does not exist (anymore)",
                                                           BufferUtil:GetRowName(hCurrentBuffer,yes),
                                                           oFindQuery:ToString(hCurrentBuffer,true,true)
                                                          )  
                                                     ).          
                end catch.
            end. 
            
            if pcTooLateFields > "" then
            do iFld = 1 to num-entries(pcTooLateFields):
                cFieldname = entry(iFld,pcTooLateFields).
                if hcurrentBuffer:buffer-field(cFieldName):buffer-value
                   <> 
                   phBuffer:buffer-field(cFieldName):buffer-value  then 
                    undo, throw new IllegalStateError(subst("The current &1 where &2 &3 has been changed to &4 by another user while the value was &5 when the initial selection was made",
                                                           BufferUtil:GetRowName(hCurrentBuffer,yes),
                                                           oFindQuery:ToString(hCurrentBuffer,true,true),
                                                           BufferUtil:GetFieldLabel( phBuffer:buffer-field(cFieldName)),
                                                           BufferUtil:GetFieldValueForMessage(hcurrentBuffer:buffer-field(cFieldName)) ,
                                                           BufferUtil:GetFieldValueForMessage(phBuffer:buffer-field(cFieldName))  
                                                          )  
                                                     ).          
                
            end.    
            
             
            if pcRefreshFields > "" then
            do iFld = 1 to num-entries(pcRefreshFields): 
                cFieldname = entry(iFld,pcRefreshFields).
                 
                if hcurrentBuffer:buffer-field(cFieldName):buffer-value
                   <> 
                   phBuffer:buffer-field(cFieldName):buffer-value then 
                do: 
                    lRefreshed = true.
                    pcRefreshInfo = StringUtil:AppendEntry(
                                    pcRefreshInfo,
                                    subst("The current &1 where &2 &3 has been changed to &4 by another user while the value was &5 when the initial selection was made.",
                                                           BufferUtil:GetRowName(hCurrentBuffer,yes),
                                                           oFindQuery:ToString(hCurrentBuffer,true,true),
                                                           BufferUtil:GetFieldLabel( phBuffer:buffer-field(cFieldName)),
                                                           BufferUtil:GetFieldValueForMessage(hcurrentBuffer:buffer-field(cFieldName)) ,
                                                           BufferUtil:GetFieldValueForMessage(phBuffer:buffer-field(cFieldName))  
                                                     ),  
                                   "~n"                 
                                   ).          
                end.  
            end.    
            hQuery:get-next.
            finally:
                if valid-object(oFindQuery) then 
                    delete object oFindQuery.            
            end finally.
            
        end.
        
        return lRefreshed.
        finally:
            if valid-object(hCurrentTable) and hCurrentTable:dynamic then 
                delete object hQuery.
            if valid-object(hQuery) then 
                delete object hQuery.        
        end finally. 

    end method.
    
    method public void ConvertCreatesToModified(pDelegator as IGetInternalDataDelegator,phbuffer as handle):
        ConvertCreatesToModified(pDelegator, phbuffer, "").
    end method.
    
    method public void ConvertCreatesToModified(input pDelegator as IGetInternalDataDelegator,phbuffer as handle,pcQueryExpression as character):
        define variable oBusinessEntityFinder as IBusinessEntityFinder no-undo.
        define variable hQuery                as handle no-undo.
        define variable oKey                  as IKey no-undo.
        define variable hTable                as handle no-undo.
        
        
        if pcQueryExpression = ? then pcQueryExpression = "".
        
        oBusinessEntityFinder = new BusinessEntityFinder(pDelegator:Resource).
        
        create query hQuery.
        hQuery:add-buffer(phbuffer).
        hQuery:query-prepare (right-trim(subst("for each &1 where row-state(&1) = row-created &2",
                                               phbuffer:name,
                                               if pcQueryExpression = "" then "" else subst("and &1",pcQueryExpression)
                                              )
                              )). // prepare - right-trim
        hQuery:query-open().
        hQuery:get-first.
        do while phbuffer:available:    
            oKey = ServiceInfo:GetKey(phbuffer). 
            
            do on error undo, throw:
                oBusinessEntityFinder:Find(oKey, output table-handle hTable).
                
                hTable:default-buffer-handle:find-unique(). // if we get here it exists
              
                catch e as NotFoundError :
                    // expected error from entityfinder      
                end catch.
            end.
           
            // convert to row modified  with current server data as before image
            if valid-handle(hTable) and hTable:default-buffer-handle:avail then 
            do: 
                
                phbuffer:before-buffer:find-by-rowid (phBuffer:before-rowid).
                phbuffer:before-buffer:accept-row-changes.
                phBuffer:mark-row-state (row-modified, hTable:default-buffer-handle).
            end.
            hQuery:get-next.
        end.  
        finally:
            if valid-handle(hQuery) then 
                delete object hQuery.
                
            if valid-handle(hTable) then 
                delete object hTable.    
        end.    
 
    end method.     
    
    
end class.