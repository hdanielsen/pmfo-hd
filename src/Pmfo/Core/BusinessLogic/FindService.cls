 
 /*------------------------------------------------------------------------
    File        : FindService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jun 12 01:12:02 EDT 2019
    Notes       : The Canfind methods could in principle just call 
                  AssertFind and cath the TableFoundError. 
                  Since we want canfind as fast as possible we avoid this
                  (This maybe of little value considered there is a general 
                   overhead and the throw is likely fast and likely
                   a very small percentage overhead) 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IFindService from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.QueryDefinition from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Util.StringUtil from propath.
 

class Pmfo.Core.BusinessLogic.FindService implements IFindService: 

    constructor public FindService (  ):
        super ().
    end constructor.

    method protected IBusinessEntity GetEntity(pcName as char):
        return cast(cast(Application:ServiceManager,IServiceManager):CreateBusinessEntity(pcName),IBusinessEntity).
    end.
    
    method protected void AssertSingleKey(phbuffer as handle,pcKeyValue as char,pcKeys as character):
        if num-entries(pcKeys) > 1 then 
            undo, throw new IllegalOperationError(subst("CanFind '&1' row with single value '&2' is not allowed. A value must be specified for all key fields: &3",BufferUtil:GetRowName(phBuffer),pcKeyValue,StringUtil:GetMessageFromEntries(pckeys))).
    end method.
     
    method public logical CanFind(pcResource as character,piKeyValue as integer).
        return CanFind(pcResource,string(piKeyValue)).
    end method.
    
    method public logical CanFind(pcResource as character,pcKeyValue as character).
        define variable oEntity as IBusinessEntity no-undo.
        return CanFind(pcResource,pcKeyValue,output oEntity).
    end method.
    
    method public logical CanFind(pcResource as character,pcKeyName as char, piKeyValue as integer).
        return CanFind(pcResource,pcKeyName,string(piKeyValue)).
    end method.
    
    method public logical CanFind(pcResource as character,pcKeyName as char, pcKeyValue as character).
        define variable oEntity as IBusinessEntity no-undo.
        return CanFind(pcResource,pcKeyName,pcKeyValue, output oEntity). 
    end method.
    
    method public logical CanFind(pcResource as character,pQueryEntry as IQueryEntry).
        define variable oEntity as IBusinessEntity no-undo.
        return CanFind(pcResource,pQueryEntry, output oEntity). 
    end method.
    
    method public logical CanFind(pcResource as character,pcKeyNames as char extent, pcKeyValues as character extent).
        define variable oEntity as IBusinessEntity no-undo.
        return CanFind(pcResource,pcKeyNames,pcKeyValues, output oEntity). 
    end method.
    
     /***  can-find variations that returns the entity ****/
       
    method public logical CanFind(pcResource as character,pcKeyValue as character, output pEntity as IBusinessEntity):
        define variable hBuffer as handle no-undo.
        define variable cKeys as character no-undo.
        pEntity = PrepareResource(pcResource, output hBuffer).
        cKeys = ServiceInfo:GetKeys(hBuffer).
        AssertSingleKey(hBuffer,pcKeyValue,cKeys).
        return CanFind(pEntity,hBuffer,Array:ToArray(cKeys),Array:toArray(pcKeyValue)).
    end method.
    
    method public logical CanFind(pcResource as character,piKeyValue as integer, output pEntity as IBusinessEntity):
        return CanFind(pcResource,string(piKeyValue), output pEntity).
    end method.
    
    method public logical CanFind(pcResource as character,pcKeyName as char, piKeyValue as integer, output pEntity as IBusinessEntity).
        return CanFind(pcResource,pcKeyName,string(piKeyValue), output pEntity). 
    end method.
        
    method public logical CanFind(pcResource as character,pcKeyName as char, pcKeyValue as character, output pEntity as IBusinessEntity).
        define variable hBuffer as handle no-undo.
        pEntity = PrepareResource(pcResource, output hBuffer). 
        return CanFind(pEntity,hBuffer,Array:ToArray(pcKeyName),Array:toArray(pcKeyValue)). 
    end method.
    
    method public logical CanFind(pcResource as character,pcKeyNames as char extent, pcKeyValues as character extent, output pEntity as IBusinessEntity).
        define variable hBuffer as handle no-undo.
        pEntity = PrepareResource(pcResource, output hBuffer). 
        return CanFind(pEntity,hBuffer,pcKeyNames,pcKeyValues). 
    end method.
    
    method public logical CanFind(pcResource as character,pQueryEntry as IQueryEntry,output pEntity as IBusinessEntity).
        define variable hBuffer as handle no-undo.
        define variable oGetRequest as IGetDataRequest no-undo.
        pEntity = PrepareResource(pcResource, output hBuffer).
        oGetRequest = GetFindRequest(hBuffer,pQueryEntry).  
        return Execute(pEntity,oGetRequest,hbuffer).
        catch e as TableNotFoundError:
            return false.    
        end catch.    
    end method.
    
    method protected logical CanFind(poEntity as IBusinessEntity,phBuffer as handle,input pcKeyFieldNames as char extent, input pcKeyValues as character extent).
        define variable oGetRequest as IGetDataRequest no-undo.
        oGetRequest = GetFindRequest(phBuffer,pcKeyFieldNames,pcKeyValues,true). // true use select
        return Execute(poEntity,oGetRequest,phBuffer).
        catch e as TableNotFoundError:
            return false.    
        end catch.
    end method.
    
    method public logical CanFindFirst(pcResource as character,pcFieldName as char, piFieldValue as integer).
        return CanFindFirst(pcResource,pcFieldName,string(piFieldValue)).   
    end method.
    
    method public logical CanFindFirst(pcResource as character,pcFieldName as char, pcFieldValue as character).
        return CanFindFirst(pcResource,Array:ToArray(pcFieldName),Array:toArray(pcFieldValue)).
    end method.
    
    method public logical CanFindFirst(pcResource as character,pcFieldNames as char extent, pcFieldValues as character extent).
        define variable hBuffer as handle no-undo.
        define variable oEntity as IBusinessEntity no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer).
        return CanFindFirst(oEntity,hBuffer,pcFieldNames,pcFieldValues).
    end method.
    
    method public logical CanFindFirst(pcResource as character,pQueryEntry as IQueryEntry). 
        define variable hBuffer as handle no-undo.
        define variable oGetRequest as IGetDataRequest no-undo.
        define variable cKeys as character no-undo.
        define variable oEntity as IBusinessEntity no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer).
        cKeys = oEntity:GetKeys().
        if cKeys <> "rowid" and cKeys > "" then
           oGetRequest = GetFindRequest(hBuffer,pQueryEntry,cKeys). //keys for select
        return ExecuteCanfindFirst(oEntity,oGetRequest,hBuffer).
        catch e as TableNotFoundError:
            return false.    
        end catch.    
       
    end method.
    
    method protected logical CanFindFirst(poEntity as IBusinessEntity,phBuffer as handle,input pcKeyFieldNames as char extent, input pcKeyValues as character extent).
        define variable oGetRequest as IGetDataRequest no-undo.
        oGetRequest = GetFindRequest(phBuffer,pcKeyFieldNames,pcKeyValues,true). // true use select 
        return ExecuteCanfindFirst(poEntity,oGetRequest,phBuffer).
        catch e as TableNotFoundError:
            return false.    
        end catch.
    end method.
    
     
    method public void AssertFind(pcResource as character,piKeyValue as integer).
        AssertFind(pcResource,string(piKeyValue)).
    end method.
    
    method public void AssertFind(pcResource as character,pcKeyValue as character).
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        define variable cKeys as character no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer).
        cKeys = ServiceInfo:GetKeys(hBuffer).
        AssertSingleKey(hBuffer,pcKeyValue,cKeys).
        AssertFind(oEntity,hBuffer,Array:ToArray(cKeys),Array:toArray(pcKeyValue)).
    end method.
    
    method public void AssertFind(pcResource as character,pcKeyName as char, pcKeyValue as character).
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer). 
        AssertFind(oEntity,hBuffer,Array:ToArray(pcKeyName),Array:toArray(pcKeyValue)). 
    end method.
    
    method public void AssertFind(pcResource as character,pcKeyName as char, piKeyValue as integer).
        AssertFind(pcResource,pcKeyName,string(piKeyValue)).
    end method.
    
    method public void AssertFind(pcResource as character,pcKeyNames as char extent, pcKeyValues as character extent).
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer). 
        AssertFind(oEntity,hBuffer,pcKeyNames,pcKeyValues). 
    end method.
    
    method public void AssertFind(pcResource as character,pQueryEntry as IQueryEntry).
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        define variable oGetRequest as IGetDataRequest no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer).
        oGetRequest = GetFindRequest(hBuffer,pQueryEntry).
        Execute(oEntity,oGetRequest,hbuffer).    
    end method.
    
    method protected void AssertFind(poEntity as IBusinessEntity,phBuffer as handle,input pcKeyFieldNames as char extent, input pcKeyValues as character extent).
        define variable oGetRequest as IGetDataRequest no-undo.
        oGetRequest = GetFindRequest(phBuffer,pcKeyFieldNames,pcKeyValues).
        Execute(poEntity,oGetRequest,phBuffer).
    end method.
    
     
    method public character FindDescription( input pcResource as character, input piKeyValue as integer ):
        return FindValue(pcResource,piKeyValue,"description").   
    end method.
    
    method public character FindDescription( input pcResource as character, input pcKeyValue as character ):
        return FindValue(pcResource,pcKeyValue,"description").   
    end method.
    
    method public character FindDescription( input pcResource as character, input pcFieldName as character, input piKeyValue as integer ):
        return FindValue(pcResource,pcFieldName,piKeyValue,"description").   
    end method.
    
    method public character FindDescription( input pcResource as character, input pcFieldName as character,input pcKeyValue as character ):
        return FindValue(pcResource,pcFieldName,pcKeyValue,"description").   
    end method.
    
    method public character FindDescription( input pcResource as character, input pcFieldNames as character extent,input pcKeyValues as character extent ):
        return FindValue(pcResource,pcFieldNames,pcKeyValues,"description").   
    end method.
    
    method public character FindDescription(pcResource as character,pQueryEntry as IQueryEntry).
        return FindValue(pcResource,pQueryEntry,"description").
    end method.
    
    method public character FindName( input pcResource as character, input pcKeyValue as character ):
        return FindValue(pcResource,pcKeyValue,"name").   
    end method.
    
    method public character FindName( input pcResource as character, input piKeyValue as integer ):
        return FindValue(pcResource,piKeyValue,"name").   
    end method.
    
    method public character FindName( input pcResource as character, input pcFieldName as character, input piKeyValue as integer ):
        return FindValue(pcResource,pcFieldName,piKeyValue,"name").   
    end method.
    
    method public character FindName( input pcResource as character, input pcFieldName as character,input pcKeyValue as character ):
        return FindValue(pcResource,pcFieldName,pcKeyValue,"name").   
    end method.
    
    method public character FindName( input pcResource as character, input pcFieldNames as character extent,input pcKeyValues as character extent ):
        return FindValue(pcResource,pcFieldNames,pcKeyValues,"name").   
    end method.
    
    method public character FindName(pcResource as character,pQueryEntry as IQueryEntry).
        return FindValue(pcResource,pQueryEntry,"name").
    end method.
    
    method public character FindValue( input pcResource as character, input piKeyValue as integer, input pcField as character ):
        return FindValue(pcResource,string(piKeyValue),pcField).   
    end method.
    
    method public character FindValue( input pcResource as character, input pcKeyValue as character, input pcFieldName as character ):
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        define variable cKeys as character no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer).
        cKeys = ServiceInfo:GetKeys(hBuffer).
        if cKeys = "" then 
            undo, throw new IllegalStateError(subst("Cannot find '&1' row with key value &2. The table does not have a key.",BufferUtil:GetRowName(hBuffer),pcKeyValue)).
        
        if num-entries(cKeys) > 1 then 
            undo, throw new IllegalOperationError(subst("Cannot find '&1' row with single value '&2'. A value must be specified for all key fields: &3",BufferUtil:GetRowName(hBuffer),pcKeyValue,StringUtil:GetMessageFromEntries(ckeys))).
        return FindValue(oEntity,hBuffer,Array:ToArray(cKeys),Array:toArray(pcKeyValue),pcFieldName). 
    end method.
    
    method public character FindValue( input pcResource as character, input pcKeyFieldName as character, input piKeyValue as integer, input pcFieldName as character ):
         return FindValue(pcResource,pcKeyFieldName,string(piKeyValue),pcFieldName).  
    end method.
    
    method public character FindValue( input pcResource as character, input pcKeyFieldName as char, input pcKeyValue as character, input pcFieldName as character ):
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer). 
        return FindValue(oEntity,hBuffer,Array:ToArray(pcKeyFieldName),Array:toArray(pcKeyValue),pcFieldName). 
    end method.
    
    method public character FindValue( input pcResource as character, input pcKeyFieldNames as char extent, input piKeyValues as integer extent, input pcFieldName as character ):
        define variable cValues as character extent no-undo.
        define variable i as integer no-undo.
        extent(cValues) = extent(piKeyValues).
        do i = 1 to extent(cValues): 
            cValues[i] = string(piKeyValues[i]). 
        end. 
        return FindValue(pcResource,pcKeyFieldNames,cValues,pcFieldName). 
    end method.
    
    method public character FindValue( input pcResource as character, input pcKeyFieldNames as char extent, input pcKeyValues as character extent, input pcFieldName as character ):
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer). 
        return FindValue(oEntity,hBuffer,pcKeyFieldNames,pcKeyValues,pcFieldName). 
    end method.
    
    method protected character FindValue( input poEntity as IBusinessEntity, input phBuffer as handle, input pcKeyFieldNames as char extent, input pcKeyValues as character extent, input pcFieldName as character ):
        define variable oGetRequest as IGetDataRequest no-undo.
        oGetRequest = GetFindRequest(phBuffer,pcKeyFieldNames,pcKeyValues).
        return Execute(poEntity,oGetRequest,phBuffer,pcFieldName).
    end method.
    
    method public character FindValue(pcResource as character,pQueryEntry as IQueryEntry,pcFieldName as character).
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        define variable oGetRequest as IGetDataRequest no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer).
        oGetRequest = GetFindRequest(hBuffer,pQueryEntry,pcfieldname).
        return Execute(oEntity,oGetRequest,hBuffer,pcFieldName).   
    end method.
    
    method public character extent FindValues( input pcResource as character, input piKeyValue as integer, input pcFieldNames as character extent ):
         return FindValues(pcResource,string(piKeyValue),pcFieldNames).
    end method.
    
    method public character extent FindValues( input pcResource as character, input pcKeyValue as character, input pcFieldNames as character extent ):
        define variable oEntity     as IBusinessEntity no-undo.
        define variable cKeyString  as character no-undo.
        define variable hBuffer     as handle no-undo.
        define variable cKeys        as character extent 1 no-undo.
        define variable cKeyValues   as character extent 1 no-undo.
       
        cKeyValues[1] = pcKeyValue.
        oEntity = PrepareResource(pcResource, output hBuffer). 
        cKeyString = ServiceInfo:GetKeys(hBuffer).
        if num-entries(cKeyString) > 1 then 
            undo, throw new IllegalOperationError(subst("Cannot find '&1' row with single value '&2'. A value must be specified for all key fields: &3",BufferUtil:GetRowName(hBuffer),pcKeyValue,StringUtil:GetMessageFromEntries(cKeyString))).
        cKeys[1] = cKeyString.
        return FindValues(oEntity,hBuffer,cKeys,cKeyValues,pcFieldNames).
    end method.
     
    method public character extent FindValues( input pcResource as character, input pcKeyFieldName as character , input pcKeyValue as character, input pcFieldNames as character extent ):
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        define variable cKey    as character extent 1 no-undo.
        define variable cKeyValue    as character extent 1 no-undo.
        cKey[1] = pcKeyFieldName.
        cKeyValue[1] = pcKeyValue.
        
        oEntity = PrepareResource(pcResource, output hBuffer). 
        return FindValues(oEntity,hBuffer,cKey,cKeyValue,pcFieldNames).
    end method.
    
    method public character extent FindValues( input pcResource as character, input pcKeyFieldName as character , input piKeyValue as integer, input pcFieldNames as character extent ):
        return FindValues(pcResource,pcKeyFieldName,string(piKeyValue),pcFieldNames).
    end method.
    
    method public character extent FindValues(pcResource as character,pcKeys as char extent, piKeyValues as integer extent,pcFieldNames as character extent).
        define variable cValues as character extent no-undo.
        define variable i as integer no-undo.
        extent(cValues) = extent(piKeyValues).
        do i = 1 to extent(cValues): 
            cValues[i] = string(piKeyValues[i]). 
        end. 
        return FindValues(pcResource, pcKeys, cValues, pcFieldNames).
    end method.
        
    method public character extent FindValues( input pcResource as character, input pcKeyFieldNames as character extent, input pcKeyValues as character extent, input pcFieldNames as character extent ):
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer). 
        return FindValues(oEntity,hBuffer,pcKeyFieldNames,pcKeyValues,pcFieldNames).
    end method.
    
    method public character extent FindValues(pcResource as character,pQueryEntry as IQueryEntry,pcFieldNames as character extent).
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        oEntity = PrepareResource(pcResource, output hBuffer). 
        return FindValues(oEntity,hBuffer,pQueryEntry,pcFieldNames).
    end method. 
    
    method protected character extent FindValues( input poEntity as IBusinessEntity, input phBuffer as handle, input pcKeyFieldNames as char extent, input pcKeyValues as character extent, input pcFieldNames as character extent ):
        define variable oGetRequest as IGetDataRequest no-undo.
        oGetRequest = GetFindRequest(phBuffer,pcKeyFieldNames,pcKeyValues).
        return Execute(poEntity,oGetRequest,phBuffer,pcFieldNames).
    end method.
    
    method protected character extent FindValues( input poEntity as IBusinessEntity, input phBuffer as handle, pQueryEntry as IQueryEntry, input pcFieldNames as character extent ):
        define variable oGetRequest as IGetDataRequest no-undo.
        oGetRequest = GetFindRequest(phBuffer,pQueryEntry,Array:ToString(pcFieldnames)).
        return Execute(poEntity,oGetRequest,phBuffer,pcFieldNames).
    end method.
    
    
    method public logical FindTable( input pcResource as character, input piKeyValue as integer,output table-handle phTable):
        return FindTable(pcResource,string(piKeyValue), output table-handle phTable by-reference).
        finally:
            // if called wothoiut by-reference the by-reference on the call above is not by-reference but a deep copy 
            if valid-object(phTable) then
            do:
                delete object phTable.
            end.     
        end.
    end method.
    
    method public logical FindTable( input pcResource as character, input pcKeyValue as character, output table-handle phTable ):
        define variable oEntity     as IBusinessEntity no-undo.
        define variable cKeyString  as character no-undo.
        define variable hBuffer     as handle no-undo.
        define variable cKeys        as character extent 1 no-undo.
        define variable cKeyValues   as character extent 1 no-undo.
        define variable lByref as logical no-undo. 
        
        cKeyValues[1] = pcKeyValue.
        oEntity = PrepareFindTable(input pcResource, input phTable, output hBuffer, output lByRef).
        
        cKeyString = ServiceInfo:GetKeys(hBuffer).
        if num-entries(cKeyString) > 1 then 
            undo, throw new IllegalOperationError(subst("Cannot find '&1' row with single value '&2'. A value must be specified for all key fields: &3",BufferUtil:GetRowName(hBuffer),pcKeyValue,StringUtil:GetMessageFromEntries(cKeyString))).
        cKeys[1] = cKeyString.
        FindBuffer(oEntity,hBuffer,cKeys,cKeyValues).
        // TODO: This DeepCopy call may be unnecessary. This lByRef is false when table-handle is invalid i.e. called without by-reference (or with invalid handle)
        //       This will cause a deep copy anmyways  
        if not lByRef then
            DeepCopy(hBuffer, output table-handle phTable).
        return phTable:has-records.
        finally:
            if valid-object(phTable) then
                delete object phTable.
        end. 
    end method.
     
    method public logical FindTable( input pcResource as character, input pcKeyFieldName as character , input pcKeyValue as character,output table-handle phTable ):
        define variable oEntity   as IBusinessEntity no-undo.
        define variable hBuffer   as handle no-undo.
        define variable cKey      as character extent 1 no-undo.
        define variable cKeyValue as character extent 1 no-undo.
        define variable hBuffere  as handle no-undo.
        define variable lByref    as logical no-undo.
        cKey[1] = pcKeyFieldName.
        cKeyValue[1] = pcKeyValue.
        
        oEntity = PrepareFindTable(input pcResource, input phTable, output hBuffer, output lByRef).
      
        FindBuffer(oEntity,hBuffer,cKey,cKeyValue).
      
        // TODO: This DeepCopy call may be unnecessary. This lByRef is false when table-handle is invalid i.e. called without by-reference (or with invalid handle)
        //       This will cause a deep copy anmyways  
        if not lByRef then
            DeepCopy(hBuffer, output table-handle phTable).
        return phTable:has-records.
        finally:
            if valid-object(phTable) then
                delete object phTable. 
        end.    
    end method.
    
    method public logical FindTable( input pcResource as character, input pcKeyFieldName as character , input piKeyValue as integer,output table-handle phTable):
        return FindTable(pcResource,pcKeyFieldName,string(piKeyValue),output table-handle phTable by-reference).
        finally:
            // if called without by-reference the by-reference on the call above is not by-reference but a deep copy 
            if valid-object(phTable) then
            do:
                delete object phTable.
            end.     
        end.
    end method.
    
    method public logical FindTable( input pcResource as character, input pcKeyFieldNames as character extent, input pcKeyValues as character extent,output table-handle phTable):
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        define variable lByRef  as logical no-undo.
        oEntity = PrepareFindTable(input pcResource, input phTable, output hBuffer, output lByRef).
        FindBuffer(oEntity,hBuffer,pcKeyFieldNames,pcKeyValues).
      
        // TODO: This DeepCopy call may be unnecessary. This lByRef is false when table-handle is invalid i.e. called without by-reference (or with invalid handle)
        //       This will cause a deep copy anmyways  
        if not LByRef then 
            DeepCopy(hBuffer, output table-handle phTable).
        return phTable:has-records.
        finally:
            if valid-object(phTable) then
                delete object phTable.
                  
        end.    
    end method.
    
    method public logical FindTable( input pcResource as character, input pcKeyFieldNames as character extent, input piKeyValues as integer extent,output table-handle phTable):
        define variable cValues as character extent no-undo.
        define variable i as integer no-undo.
        extent(cValues) = extent(piKeyValues).
        do i = 1 to extent(cValues): 
            cValues[i] = string(piKeyValues[i]). 
        end. 
        return FindTable(input pcResource, input pcKeyFieldNames, input cValues, output table-handle phTable by-reference).
        finally:
            // if called without by-reference the by-reference on the call above is not by-reference but a deep copy 
            if valid-object(phTable) then
            do:
                delete object phTable.
            end.     
        end.   
    end method.
    
    method public logical FindTable( input pcResource as character, input pQueryEntry as IQueryEntry, output table-handle phTable).
        define variable oEntity as IBusinessEntity no-undo.
        define variable hBuffer as handle no-undo.
        define variable lByRef  as logical no-undo.
        
        oEntity = PrepareFindTable(input pcResource, input phTable, output hBuffer, output lByRef).
        FindBuffer(oEntity,hBuffer,pQueryEntry).
        
        // TODO: This DeepCopy call may be unnecessary. This lByRef is false when table-handle is invalid i.e. called without by-reference (or with invalid handle)
        //       This will cause a deep copy anmyways  
        if not LByRef then 
            DeepCopy(hBuffer, output table-handle phTable).
        finally:
            if valid-object(phTable) then
                delete object phTable.
        end.    
    end method.  
    
    method protected IBusinessEntity PrepareFindTable(input pcResource as character, phTable as handle, output phBuffer as handle, output pByRef as logical).
        define variable oEntity as IBusinessEntity no-undo.
        if valid-handle(phTable) then
        do:
            phTable:empty-temp-table ().
            phBuffer = phTable:default-buffer-handle.
            oEntity = GetEntity(pcResource, phBuffer).
            pByRef  = true.
        end.
        else do:
            oEntity = PrepareResource(pcResource, output phBuffer). 
        end.
        return oEntity. 
    end method.    
    
    method protected void DeepCopy(input phBuffer as handle, output table-handle phTable):
        phTable = phbuffer:table-handle.
        finally:
            if valid-handle(phTable) then
                delete object phTable.
        end.    
    end method.     
    
    method protected void FindBuffer( input poEntity as IBusinessEntity, input phBuffer as handle, input pcKeyFieldNames as char extent, input pcKeyValues as character extent):
        define variable oGetRequest as IGetDataRequest no-undo.
        oGetRequest = GetFindRequest(phBuffer,pcKeyFieldNames,pcKeyValues).
        Execute(poEntity,oGetRequest,phBuffer,true). // true keep data
    end method.
    
    method protected void FindBuffer( input poEntity as IBusinessEntity, input phBuffer as handle,input pQueryEntry as IQueryEntry):
        define variable oGetRequest as IGetDataRequest no-undo.
        oGetRequest = GetFindRequest(phBuffer,pQueryEntry).
        Execute(poEntity,oGetRequest,phBuffer,true). // true keep data
    end method.
    
    method protected character extent Execute(pEntity as IBusinessEntity,pRequest as IGetDataRequest, phBuffer as handle, pcFieldnames as character extent):
        define variable i as integer no-undo.
        define variable cValues as character extent no-undo.
        Execute(pEntity,pRequest,phBuffer). 
        extent(cValues) = extent(pcFieldNames).
        do i = 1 to extent(pcFieldNames): 
            cValues[i] = phBuffer:buffer-field(pcFieldNames[i]):buffer-value.
        end.    
        return cValues.
    end method.  
    
    method protected character Execute(pEntity as IBusinessEntity,pRequest as IGetDataRequest, phBuffer as handle, pcFieldname as character):
        define variable hField as handle no-undo.
      
        // for description also support descr (used in choice)
        if pcFieldName = "description" then 
        do on error undo, throw:
            hField = phBuffer:buffer-field(pcFieldName).
            
            catch e as Progress.Lang.Error :
                if e:GetMessageNum(1) = 7351 then
                do:
                    hField = phBuffer:buffer-field("descr") no-error.
                    if not valid-handle(hField) then 
                        undo, throw e. // throw the original error
                end.        
                else 
                    undo, throw e.
            end catch.
        end.
        else
           hField = phBuffer:buffer-field(pcFieldName).
/*        if pcDataType > "" then                                                                                         */
/*            Assert:isTrue(hField:data-type = pcDataType,subst("Field '&1' is not of type '&2'",pcFieldName,pcDataType)).*/
        // throws error if fail 
        Execute(pEntity,pRequest,phBuffer).
        return phBuffer:buffer-field(hField:name):buffer-value.
    end method.  
    
    method protected logical Execute(pEntity as IBusinessEntity,pRequest as IGetDataRequest, phBuffer as handle):
        return Execute(pEntity,pRequest,phbuffer,false).      
    end method.  
    
    method protected logical ExecuteCanfindFirst(pEntity as IBusinessEntity,pRequest as IGetDataRequest, phBuffer as handle):
        return Execute(pEntity,pRequest,phbuffer,true).
    end method.  
    
    method protected logical Execute(pEntity as IBusinessEntity,pRequest as IGetDataRequest, phBuffer as handle, plFindFirst as logical):
        define variable hDS   as handle no-undo.     
        define variable hBuffer as handle no-undo.
        create dataset hds.
        hds:name = pEntity:Resource.
        create buffer hBuffer for table phBuffer.
         
        hBuffer:serialize-name = pEntity:Resource.
        hds:add-buffer (hBuffer).
        pEntity:GetData(pRequest, output dataset-handle hDs by-reference).
        do on error undo, throw:
            if plFindFirst then
                hBuffer:find-first ().
            else
                hBuffer:find-unique ().
                
            phBuffer:find-by-rowid(hBuffer:rowid).
            return true.
            catch e as Progress.Lang.Error :
                undo, throw new TableNotFoundError(hbuffer,pRequest:TableRequests[1],e).
            end catch. 
        end.
        finally:
            delete object hds.
            if valid-handle(hBuffer) then
                delete object hBuffer.
                 
        end.    
    end method.  
    
    method protected IGetDataRequest GetFindRequest(phBuffer as handle, pcFieldNames as character extent, pcKeyValues as char extent) :
        return GetFindRequest(phBuffer,pcFieldNames,pcKeyValues,false).
    end method.
    
    method protected IGetDataRequest GetFindRequest(phBuffer as handle, pcFieldNames as character extent, pcKeyValues as char extent, plSelect as logical) :
        define variable i as integer no-undo.
        define variable cDlm as character no-undo.
        define variable oMap as StringStringMap no-undo.
        oMap = new StringStringMap().
        Assert:isTrue(extent(pcFieldNames) =extent(pcKeyValues),"Fields and values have same number of extents").
        do i = 1 to extent(pcFieldnames):
            oMap:Put(pcFieldNames[i],pcKeyValues[i]).
        end.    
        return GetFindRequest(phBuffer,oMap,if plSelect then Array:ToString(pcFieldNames) else "").
    end method.
    
     
    method protected IGetDataRequest GetFindRequest(phBuffer as handle,pFieldValueMap as StringStringMap,pcSelect as character) :
        define variable oGetTableRequest as GetDataTableRequest no-undo.
        oGetTableRequest = new GetDataTableRequest(phbuffer:table-handle:name).
        oGetTableRequest:FindUnique = true.
        oGetTableRequest:SelectFields = pcSelect.
        oGetTableRequest:QueryDefinition = new QueryDefinition( QueryUtil:GetKeyEntry(pFieldValueMap,phbuffer,false)).
        return CreateRequest(oGetTableRequest).
    end method.
    
    method protected IGetDataRequest GetFindRequest(phBuffer as handle,pQueryEntry as IQueryEntry) :
        return GetFindRequest(phBuffer, pQueryEntry,"").
    end method.
    
    method protected IGetDataRequest GetFindRequest(phBuffer as handle,pQueryEntry as IQueryEntry,pcSelect as character) :
        define variable oGetTableRequest as GetDataTableRequest no-undo.
        oGetTableRequest = new GetDataTableRequest(phbuffer:table-handle:name).
        oGetTableRequest:FindUnique = true.
        oGetTableRequest:SelectFields = pcSelect.
        oGetTableRequest:QueryDefinition = new QueryDefinition( pQueryEntry).
        return CreateRequest(oGetTableRequest).
    end method.
        
    method protected IGetDataRequest CreateRequest(pTableRequest as IGetDataTableRequest):
        return new GetDataRequest(pTableRequest).
    end method.      
        
    method protected IBusinessEntity PrepareResource( input pcResource as character, output phBuffer as handle ):
        define variable oEntity as IBusinessEntity no-undo.
        define variable hDataset as handle no-undo.
        oEntity = GetEntity(pcResource). 
        hDataset = oEntity:Datasets[1].
        if hDataset:serialize-name <> pcResource then 
            undo, throw new IllegalOperationError(subst("Cannot find row with key in Resource '&1'",pcResource)).
        phBuffer = DatasetUtil:GetTopBuffer(hDataset, 1).
        if phBuffer:serialize-name <> pcResource then 
            undo, throw new IllegalOperationError(subst("Cannot find row with key in Resource '&1'",pcResource)).
        
        return oEntity.
    
    end method.
    
    method protected IBusinessEntity GetEntity( input pcResource as character, input phbuffer as handle ):
        define variable oEntity as IBusinessEntity no-undo.
        define variable hDataset as handle no-undo.
        oEntity = GetEntity(pcResource). 
        hDataset = oEntity:Datasets[1].
        if hDataset:serialize-name <> pcResource then 
            undo, throw new IllegalOperationError(subst("Cannot find row with key in Resource '&1'",pcResource)).
      // don;t do this - prevents use of  define table like woith different name
      //  if phBuffer:table-handle:name <> DatasetUtil:GetTopBufferExcludeRepos(hDataset, 1):name then
        
        //    undo, throw new IllegalOperationError(subst("Cannot get data for table '&1' from Resource '&2'",phBuffer:table-handle:name,pcResource)).
        if phBuffer:serialize-name <> pcResource then 
            undo, throw new IllegalOperationError(subst("Cannot find row with key in Resource '&1'",pcResource)).
        
        return oEntity.
    
    end method.
    
    
    
end class.