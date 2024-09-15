 
 /*------------------------------------------------------------------------
    File        : ValidationDelegate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Mar 26 10:41:29 EDT 2024
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.Delegate.IValidateDelegate.
using Pmfo.Core.BusinessLogic.Delegate.IBusinessEntityDelegator from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetUpdateManager from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.Common.IKey from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Core.BusinessLogic.UpdateDataEventArgs from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataEventArgs from propath.
using Pmfo.Core.Error.ValidationError from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.Delegate.IValidateDelegator from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Util.ErrorStringUtil from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Collections.ICharacterCharacterMap from propath.
using Pmfo.Core.Collections.CharacterCharacterMap from propath.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.Delegate.ValidateDelegate implements IValidateDelegate: 
    
    method public void ValidateBuffer(pDelegator as IBusinessEntityDelegator, pRequest as IUpdateDataEventArgs, phBuffer as handle):
        define variable lIsAction as logical no-undo.
        lisAction = valid-object(pRequest:Request:CustomRequest) and type-of(pRequest:Request:CustomRequest,IAction).
        InvokeValidate(pDelegator, pRequest, phBuffer,lIsAction).   
    end method.
    
    method public Object ValidateRow(
        input pDelegator as IBusinessEntityDelegator, 
        input pManager as IDatasetUpdateManager,
        input pKey as IKey,
        input poUpdateDataRequest as IUpdateDataRequest, 
        input phDataset as handle):
            
        define variable hbuffer as handle no-undo.
        hBuffer = DatasetUtil:GetBufferBySerializeName(phDataset, pDelegator:Resource).
        FindRow(hBuffer,pKey).   
        
        FieldValidate(pDelegator,hBuffer:table-handle:name, pKey,poUpdateDataRequest,phDataset).
        
        InvokeValidate(pDelegator,new UpdateDataEventArgs(pDelegator,poUpdateDataRequest,hBuffer), hBuffer, false).
    end method.
    
    method public Object ValidateChildRow(
        input pDelegator as IBusinessEntityDelegator, 
        input pManager as IDatasetUpdateManager,
        input pChildResource as character,
        input pKey as IKey,
        input poUpdateDataRequest as IUpdateDataRequest, 
        input phDataset as handle):
        
        define variable oBe          as IBusinessEntity no-undo. 
        define variable hBuffer      as handle no-undo.
        define variable hTopBuffer   as handle no-undo.
        define variable oAction      as IAction no-undo.
        define variable oCurrentMap  as ICharacterCharacterMap no-undo.
        define variable iBuffer      as integer no-undo.
        define variable cStringRowid as character no-undo.
        
        hBuffer = DatasetUtil:GetBufferBySerializeName(phDataset, pChildResource).
        
        if not valid-handle(hBuffer) then
           undo, throw new IllegalArgumentError(subst("Resource '&1' is not present in submitted data",pChildResource)).
        
        FindRow(hBuffer,pKey).
        oCurrentMap = new CharacterCharacterMap().
        
        oCurrentMap:put(hBuffer:name,string(hBuffer:rowid)).
         
        // remove othere records in orer to only valdiate this row 
        // note that we keep other rows that are children of the top buffer
        // even if they are not direct parent of this one since 
        // it may make sense to allow validations to see those     
        oAction = poUpdateDataRequest:Action.
        // we currently only allow thos for Action with no content response 
        if valid-object(oAction) and oAction:NoContentResponse then
        do:
            hTopBuffer = hBuffer.
            
            do while valid-handle(hTopBuffer:parent-relation):
                hTopBuffer = hTopBuffer:parent-relation:parent-buffer.
                hTopBuffer:find-unique (subst("where &1",BufferUtil:GetJoinParentExpression(hBuffer:parent-relation) )).
                oCurrentMap:put(hTopBuffer:name,string(hTopBuffer:rowid)).
            end. 
            RemoveOtherRows(hTopBuffer, hTopBuffer:rowid).
            RemoveOtherRows(hBuffer, hBuffer:rowid).
        end.
        else do:
            // Could in theory cach the removals and add them back, but as of current this is 
            // implemented to support validation and not update any fields 
            //  throw not implemendted sinc it is possible to change 
            if valid-object(oAction) then
                undo, throw new NotImplementedError(subst("ValidateChildRow called from Action '&1' that is not defined with NoContentResponse",oAction:name)).
            else // this is assumed to make no sense for regular update
                undo, throw new UnsupportedOperationError("ValidateChildRow called from UpdateRequest that is not an Action").
        end.     
        oBe = pManager:GetBusinessEntity(hbuffer).
        // this is likely unnecessary since ValidateTable is supposed to a for each 
        // and should also check UpdateDataEventArgs to check for parents  
        // buf for now ensure this buffer and all direct parents are avail  
        do iBuffer = 1 to phDataset:num-buffers:
            cStringRowid = oCurrentMap:Get(phDataset:get-buffer-handle(iBuffer):name). 
            if cStringRowid > "" then 
                phDataset:get-buffer-handle(iBuffer):find-by-rowid(to-rowid(cStringRowid)).
        end.    
        
        InvokeValidate(cast(oBe,IBusinessEntityDelegator),new UpdateDataEventArgs(pDelegator,poUpdateDataRequest,hBuffer), hBuffer, false).
         
        if phdataset:error = false then
            // must call the delegator to get parent level options and data for fieldValidate
            // Note: This was implemented before the removal of records above
            //      The pkey info is still needed, since we want to skip all validation fopr other tables, 
            //      but this can be simplified to just have the table name     
            FieldValidate(pDelegator,hBuffer:table-handle:name, pKey,poUpdateDataRequest,phDataset).
    end method.
    
    method protected void RemoveOtherRows(input phBuffer as handle, prRowid as rowid):
        define variable hQuery  as handle no-undo.
        define variable hBuffer as handle no-undo.
        create query hQuery.
        
        hQuery:add-buffer(phBuffer).
        hQuery:query-prepare(subst("for each &1 where rowid(&1) <> to-rowid('&2')",phBuffer:name,prRowid)).
        hQuery:query-open().
        hQuery:get-first.
        
        // create a buffer to keep the record for delete after get-next
        create buffer hBuffer for table phBuffer. 
        
        do while phBuffer:avail:
            hBuffer:find-by-rowid (phBuffer:rowid).     
            hQuery:get-next().
            hBuffer:buffer-delete.
        end.     
        finally:
            if valid-handle(hQuery) then
                delete object hQuery.
            if valid-handle(hBuffer) then
                delete object hBuffer.
        end.     
    end method.    
    
    // hook for application level field validation service  
    method protected void FieldValidate(
        input pDelegator as IBusinessEntityDelegator, 
        input pTableName as character,
        input pKey as IKey, 
        input pUpdateRequest as IUpdateDataRequest, 
        input phDataset as handle):
    end method.
    
    method protected void FindRow(phBuffer as handle, pKey as IKey):
         
        do on error undo, throw:   
            phBuffer:find-unique(subst("where &1",pKey:ToString("urlQuery"))).
            catch e as Progress.Lang.Error :
                            
                if e:GetMessageNum(1) = 138 then
                    undo, throw new TableNotFoundError(phBuffer,pKey:ToString("urlQuery")).
                else 
                    undo, throw e.     
            end catch.
             
        end.    
        
    end method.     
    
    method private void InvokeValidate(pDelegator as IBusinessEntityDelegator , pRequest as IUpdateDataEventArgs, phBuffer as handle, plAction as logical):
        define variable hTable as handle no-undo.    
        define variable cAction as character no-undo.
        define variable hLocalBuffer as handle no-undo.
         
        hTable = phBuffer:table-handle.
        do on error undo, throw:
             /** Call hook for static table   
                 There is no way to define a super method for a static temp-table so we must use dynamic-invoke  
              **/
            if plAction then
                cAction = "ValidateAction".
            else
                cAction = "ValidateTable".
            
            // bug workaround - can-find does in some cases not see the record when only one record exists and  it is avail 
            phBuffer:buffer-release().
            dynamic-invoke (pDelegator,cAction,pRequest, table-handle hTable by-reference ).
            
            catch e1 as ValidationError:
                // when ThrowBufferError is used in validationTable the buffer does not have a dataset.
                // sso add the ewerror here
                if phBuffer:error then 
                    phBuffer:dataset:error = true.
                else do:
                    // the error may have been added to the buffer of the defined dataset 
                    if pDelegator:DefaultDataset:error then 
                        phBuffer:dataset:error = true.
                      
                end.        
                undo, throw e1.        
            end.    
            catch e as Progress.Lang.Error :
                // don't throw error if the method does not exist  
                if e:GetMessageNum(1) <> 14457 then
                    undo, throw e.     
            end catch.
        end.
        if plAction = false and type-of(pDelegator,IValidateDelegator) then 
            ValidateMandatoryFields(cast(pDelegator,IValidateDelegator), pRequest, phBuffer).
            
    end method. 
    
    
    method private void ValidateMandatoryFields(pValidateDelegator as IValidateDelegator,pRequest as IUpdateDataEventArgs, phBuffer as handle).
        define variable hQuery     as handle no-undo.
        define variable hFld       as handle no-undo.
        define variable hBeforeFld as handle no-undo.
        define variable i          as integer no-undo.
        define variable hBefore    as handle no-undo.
        
        if phBuffer:error = false
        // defaults to false as of current  
        and pValidateDelegator:ValidateRequiredFields then
        do:
        
            hBefore = phbuffer:before-buffer.
            create query hQuery.   
            hQuery:add-buffer(phbuffer).
            hQuery:query-prepare (subst("for each &1 where row-state(&1) = row-created or row-state(&1) = row-modified",phBuffer:name)).
            hQuery:query-open ().
            hQuery:get-first.
            do while phBuffer:avail:
                hBefore:find-by-rowid(phBuffer:before-rowid).
                do i = 1 to phbuffer:num-fields:
                    hFld = phBuffer:buffer-field(i).
                    if phbuffer:row-state = row-modified then
                        hBeforeFld =  hBefore:buffer-field(hFld:name).
                    if phbuffer:row-state = row-created
                    or hfld:buffer-value <> hBeforeFld:buffer-value then
                    do: 
                        if pValidateDelegator:ServerFields > "" and lookup(hfld:name,pValidateDelegator:ServerFields) > 0 then
                        do:
                            if hFld:default-value = ? and hFld:buffer-value <> ? then
                                BufferError(phBuffer,subst("&1 is not editable. It will be assigned a value on server.",BufferUtil:GetFieldLabel(hFLd)),hfld:name). 
                        end.                 
                        else if (hFld:default-value <> ? or lookup(hFld:name,pValidateDelegator:RequiredFields) > 0) and hFld:buffer-value = ? then
                        do:        
                            if hFld:serialize-hidden = true then
                            do:
                                undo, throw new ApplicationError(subst("Entity Field &1 cannot be submitted with null. Please Contact IT.",hFld:name)).  
                            end.
                            else do:
                                case hFld:data-type:
                                    when "character" or when "longchar" then
                                        BufferError(phBuffer,subst("&1 is required, please enter a valid value",BufferUtil:GetFieldLabel(hFLd)),hfld:name).
                                    when "integer" or when "int64" then
                                        BufferError(phBuffer,subst("&1 is required and cannot be empty, please enter a valid integer value",BufferUtil:GetFieldLabel(hFLd)),hfld:name).
                                    when "decimal" then
                                        BufferError(phBuffer,subst("&1 is required and cannot be empty, please enter a valid number value",BufferUtil:GetFieldLabel(hFLd)),hfld:name).
                                    otherwise
                                        BufferError(phBuffer,subst("&1 is required and cannot be empty, please enter a valid value",BufferUtil:GetFieldLabel(hFLd)),hfld:name).
                                   
                                end.
                            end.
                        end.
                    end.
                        
                end.    
                hQuery:get-next.
            end.
        end.
        finally:
            if valid-object(hQuery) then
                delete object hQuery.
        end.    
    end method. 
    
    method protected void BufferError(phBuffer as handle,pcMsg as character):
        // the ErrorStringUtil only works  with changed records that has error attributes
        // TODO - add error object and collection that can provide the same capabilities 
        // - cellect many errors and return table, key and field info   
        if phBuffer:row-state = 0 then 
            undo, throw new BadRequestError(pcMsg).
               
        ErrorStringUtil:AddError(phBuffer, pcMsg).
    end method.    
    
    method protected void BufferError(phBuffer as handle,pcMsg as character, pcField as character):
        // the ErrorStringUtil only works  with changed records that has error attributes 
        if phBuffer:row-state = 0 then 
            undo, throw new BadRequestError(pcMsg).
        ErrorStringUtil:AddError(phBuffer, pcMsg, pcField).
    end method.    
    
end class.