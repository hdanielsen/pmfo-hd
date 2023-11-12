 
 /*------------------------------------------------------------------------
    File        : BusinessEntiityFinder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Oct 20 10:22:37 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.Common.Application from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntityFinder from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.Common.IKey from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.Error.NotFoundError from propath.

class Pmfo.Core.BusinessLogic.BusinessEntityFinder implements IBusinessEntityFinder: 
    
    define public property BusinessEntity     as IBusinessEntity no-undo get. protected set.
    define public property ThrowObsoleteError as logical no-undo 
       get. 
       set(plThrow as logical):
           
            
       end.    
    
    define protected property Request         as IGetDataRequest no-undo get. set. 
    
    define protected property FindPredicates  as IQueryPredicate extent no-undo get. set.
    
    define protected property KeyNames        as character no-undo get. set.
    define protected property BufferName      as character no-undo get. set.
    define protected property KeyDataType     as character no-undo get. set.
    
    define public property TableHandle as handle no-undo 
        get(): 
            define variable hTable as handle no-undo.
            TableHandle(output table-handle hTable).
            return hTable. 
        end.     
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public BusinessEntityFinder (pcResource as character):
        this-object(pcResource,"",0). 
    end constructor.
    
    constructor public BusinessEntityFinder (pcResource as character,pcKeyNames as character):
        this-object(pcResource,"",0,pcKeynames).
    end constructor.
    
    // only integer parent value currently implemented -   
    constructor public BusinessEntityFinder ( pcResource as character,pcParentFieldName as character, piParentValue as integer):
         this-object(pcResource,pcParentFieldName,piParentValue,"").
    end method.     
    
    constructor public BusinessEntityFinder (pcResource as character, pcParentFieldName as character, piParentValue as integer, pcKeyNames as character ):
        define variable hbuffer       as handle no-undo.
        define variable oTableRequest as GetDataTableRequest no-undo.
        define variable iParentKey    as integer no-undo.
        define variable i             as integer no-undo.  
        BusinessEntity = cast(cast(Application:ServiceManager,IServiceManager):CreateBusinessEntity(pcResource),IBusinessEntity).
        
        hBuffer = DatasetUtil:GetBufferByTableSerializeName(BusinessEntity:Datasets[1],pcResource).
        
        if not valid-handle(hBuffer)  then 
            undo, throw new IllegalStateError(subst("No buffer found for Resource &1 in dataset of BusinessEntity &2",pcResource,BusinessEntity:GetCLass():TypeName)).
        
        if pcKeyNames = "" then
        do:
            KeyNames = ServiceInfo:GetKeys(hBuffer).
            if num-entries(KeyNames) > 1 then
            do: 
                if pcParentFieldName > "" then 
                do:
                    iParentKey = lookup(pcParentFieldName,Keynames).
                    if iParentKey > 0 then
                        KeyNames = StringUtil:RemoveEntryFromList(iParentKey, KeyNames).
                    else
                       undo, throw new UnsupportedOperationError(subst("BusinessEntityFinder Parent Field Name '&1' does not match any of the keys '&2'",pcParentFieldName,pcResource,Keynames)).
                end.    
            end. 
            else if pcParentFieldName > "" then 
                 undo, throw new IllegalArgumentError(subst("Parent Field Name '&1' provided for Entity &2 with single key '&3'",
                                                            pcParentFieldName,
                                                            pcResource,
                                                            KeyNames
                                                            )
                                                      ).
            
            
        end.
        else do on error undo, throw:
            do i = 1 to num-entries(pcKeynames) on error undo, throw:
                hBuffer:buffer-field(entry(i,pcKeyNames)).  
                catch e as Progress.Lang.Error :
                    if e:GetMessageNum(1) = 7351 then     
                         undo, throw new IllegalArgumentError(subst("KeyName '&1' is not a field in '&2'",entry(i,pcKeyNames),pcResource)). 
                end catch.
            end.
            KeyNames = pcKeyNames.
        end.    
        
        BufferName = hBuffer:name.
     
        oTableRequest = new GetDataTableRequest(hBuffer:table-handle:name).
        oTableRequest:FindUnique = true.
        if pcParentFieldName > "" then 
            oTableRequest:AddQueryEntry(pcParentFieldName,QueryOperatorEnum:Eq,piParentValue).
        
        extent(FindPredicates) = num-entries(Keynames).
        do i = 1 to num-entries(Keynames):
            FindPredicates[i] = new QueryPredicate(entry(i,KeyNames),QueryOperatorEnum:Eq,"").
            oTableRequest:AddQueryEntry(FindPredicates[i]).
        end. 
        
        Request = new GetDataRequest(oTableRequest).
        // only  checked in Fiond primitive value
        if num-entries(Keynames) = 1 then
            KeyDataType = hBuffer:buffer-field(KeyNames):data-type.
        
    end constructor.
    
    method public void  Bind(output table-handle phTable bind ):
        TableHandle(output table-handle phTable).
    end method.

    method protected handle TableHandle(output table-handle phTable):
        phTable = DatasetUtil:GetBufferByTableSerializeName(BusinessEntity:Datasets[1],BusinessEntity:Resource):table-handle.
        finally:
           delete object phTable.    
        end finally.
    end method.     
    
    method public void Find( input piValue as integer, output table-handle phTable ):
        if valid-handle(phTable) then 
            // The BE is dataset oriented - Use FindService  or one of the GetData with Dataset output  
            undo, throw new UnsupportedOperationError("Find with output table by-reference parameter").  
        PreparePredicate(piValue).
        Execute(output table-handle phTable by-reference).
        
    end method.
    
    method public void Find(poKey as IKey,output table-handle phTable).
        if valid-handle(phTable) then 
            // The BE is dataset oriented - Use FindService  or one of the GetData with Dataset output  
            undo, throw new UnsupportedOperationError("Find with output table by-reference parameter").  
        PreparePredicate(poKey).
      
        Execute(output table-handle phTable by-reference).  
    end method. 
    
    method public void Find( input pcValue as character, output table-handle phTable ):
        if valid-handle(phTable) then 
            // The BE is dataset oriented - Use FindService  or one of the GetData with Dataset output  
            undo, throw new UnsupportedOperationError("Find with output table by-reference parameter").  
        PreparePredicate(pcValue).
      
        Execute(output table-handle phTable by-reference).
            
    end method.
    
    method protected void Execute(output table-handle phTable):
        define variable hDataset as handle no-undo.
        define variable hObsolete as handle no-undo.
        define variable hBuffer as handle no-undo.
        BusinessEntity:Clear().
        BusinessEntity:GetData(Request, output dataset-handle hDataset by-reference).
        
        hBuffer= hDataset:get-buffer-handle(BufferName).
        phTable = hBuffer:table-handle.
        hBuffer:find-unique () no-error.
        
        if hBuffer:ambiguous then 
        do:
            undo, throw new NotFoundError(subst("&1 not found. More than one record extists &3",BufferUtil:GetRowName(hBuffer),Request:GetTableRequest(hBuffer:table-handle:name):QueryDefinition:QuerySelection:ToString())).
        end. 
        else if hBuffer:avail = false then
        do:
            undo, throw new TableNotFoundError(hBuffer,Request:GetTableRequest(hBuffer:table-handle:name)).
        end.
        else if ThrowObsoleteError then 
        do:
            hObsolete = hBuffer:buffer-field("obsolete") no-error.
            if valid-handle(hObsolete) and hObsolete:buffer-value = true then 
               undo, throw new NotFoundError(subst("&1 where &2 is obsolete",BufferUtil:GetRowName(hBuffer),Request:GetTableRequest(hBuffer:table-handle:name):QueryDefinition:QuerySelection:ToString())).
        end.
        
    end method.
    
    method protected void PreparePredicate(poKey as IKey):
        define variable i as integer no-undo. 
        define variable cFields as character no-undo.
        do i = 1 to extent(FindPredicates):
            // todo loop twice to be less strict?  
            if poKey:FieldValues[i]:Name <> FindPredicates[i]:FieldName then 
                undo, throw  new UnsupportedOperationError(subst("Key Field '&1' is number &2 in the passed key and does not match the expected order. The Key Parameters must have fields in the following order &3",
                                                                 poKey:FieldValues[i]:Name,
                                                                 i,
                                                                 KeyNames
                                                                 )
                                                          ).
            FindPredicates[i]:StringValue = poKey:FieldValues[i]:Value:ToString().
          
        end.    
   end method.      
           
    method protected void PreparePredicate(piValue as integer):
        if extent(FindPredicates) > 1 then 
            undo, throw  new IllegalOperationError(subst("Find(integer) used to find record for keys &1",StringUtil:GetMessageFromEntries(KeyNames))). 
        
        if KeyDataType <> "integer" then 
            undo, throw  new IllegalOperationError(subst("Integer value parameter used to find record for &1 key",KeyDataType)). 
            
        FindPredicates[1]:StringValue = string(piValue).
    end.     
    
    method protected void PreparePredicate(pcValue as character):
        if extent(FindPredicates) > 1 then 
            undo, throw  new IllegalOperationError(subst("Find(integer) used to find record for keys &1",StringUtil:GetMessageFromEntries(KeyNames))). 
        
        if KeyDataType <> "character" then 
            undo, throw  new IllegalOperationError(subst("Character value parameter used to find record for &1 key",KeyDataType)). 
        FindPredicates[1]:StringValue = pcValue.
    end.     
   
    
end class.