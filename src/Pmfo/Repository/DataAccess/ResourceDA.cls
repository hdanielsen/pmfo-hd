 
 /*------------------------------------------------------------------------
    File        : ResourceDA
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Mar 22 09:59:52 EDT 2019
    Notes       : 2/12/2020 - SES - Memory leak fix
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.BusinessLogic.IGetDataRequest from propath.
using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.BusinessLogic.GetDataResponse from propath.
using Pmfo.Core.BusinessLogic.GetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.DataLayer.DataAccess from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Manager.StartupManager from propath.
using Pmfo.Repository.DataSource.ResourceSource from propath.
using Pmfo.Repository.Error.CorruptResourcesError from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.FileUtil from propath.
using Progress.Lang.Object from propath.
 
 

class Pmfo.Repository.DataAccess.ResourceDA inherits DataAccess: 
    
    define protected property DataSourceTempTableName     as character init "ttDataSource" no-undo get.
    define protected property DataAccessTempTableName     as character init "ttDataAccess" no-undo get.
    define protected property EntityTempTableName         as character init "ttEntity" no-undo get.
    define protected property DataSourcePublicName        as character init "dataSources" no-undo get.
    define protected property DataAccessPublicName        as character init "dataAccess" no-undo get.
    define protected property EntityPublicName            as character init "entities" no-undo get.
    
    define public override property Resource as character init "resources" no-undo get.
    
    define protected variable PostponeDestroy as logical no-undo.
    
    define public property ServiceManager as IServiceManager no-undo
        get():
            return cast(Application:ServiceManager,IServiceManager).
        end get.
        
    define public property SerializeFullName as char no-undo 
        get().
            define variable cDir  as character no-undo.
            if SerializeFullName = "" then 
            do:
                if not FileUtil:IsDirectory(StartupManager:ConfigDirectory) then 
                    undo, throw new IllegalOperationError(subst('ServiceManager:ConfigDirectory "&1" is not a valid directory in propath',StartupManager:ConfigDirectory)).
                
                file-info:file-name = StartupManager:ConfigDirectory.
                cDir = file-info:full-pathname.
                SerializeFullName = FileUtil:AppendToPath(cDir,"resources.json").
            end.
            return SerializeFullName.
        end.        
        private set.
    
    define public property LastModified as datetime-tz no-undo 
        get():
           if search(SerializeFullName) <> ? then
                return fileUtil:ModTime(SerializeFullName).
           return ?.     
        end.     
    
    method public logical NeedRefresh(pLastRead as datetime-tz):
        define variable jsontime as datetime-tz no-undo.
        define variable lRefresh as logical no-undo.
        if search(SerializeFullName) <> ? then
        do:
            jsontime = fileUtil:ModTime(SerializeFullName).
            if pLastRead <> ? and pLastRead < jsontime then
            do:
                lRefresh = true.
            end.
            else     
                lRefresh = jsontime < max(fileUtil:ModTime(ServiceManager:DataSourceDirectory),
                                      fileUtil:ModTime(ServiceManager:EntityDirectory),
                                      fileUtil:ModTime(ServiceManager:DataAccessDirectory)
                                       ).
        end.
        else 
            lrefresh = true. /* first time ever ... */
        return lRefresh.                          
    end method.
    
    method public character extent NeedRefreshTables():
        define variable jsontime as datetime-tz no-undo.
        define variable cTableList as character no-undo. 
        define variable cTables as character extent no-undo.
        define variable i as integer no-undo.
        define variable iNumTables as integer no-undo.
        
        if search(SerializeFullName) <> ? then 
            jsontime = fileUtil:ModTime(SerializeFullName).
        if jsontime = ? or jsontime <= fileUtil:ModTime(ServiceManager:DataSourceDirectory) then
            cTableList = DataSourceTempTableName.
        if jsontime = ? or jsontime <= fileUtil:ModTime(ServiceManager:DataAccessDirectory) then
            cTableList = cTableList + "," + DataAccessTempTableName.
        if jsontime = ? or jsontime <= fileUtil:ModTime(ServiceManager:EntityDirectory) then
            cTableList = cTableList + "," + EntityTempTableName.
        
        cTableList = trim(cTableList,",").
        iNumTables = num-entries(cTableList). 
        if iNumTables > 0 then
        do:
            extent(cTables) = iNumTables.     
            do i = 1 to extent(cTables):
                cTables[i] = entry(i,cTableList).
            end.
        end.
        return cTables.            
    end method.     
    
    method override public IGetDataResponse FetchData( input poRequest as IGetDataRequest, input-output dataset-handle phDataset ):
        define variable oResponse      as IGetDataResponse no-undo.
        define variable oEntityRequest as IGetDataTableRequest no-undo.
        define variable i as integer no-undo.
        DatasetHandle = phDataset. // setter and destructor avoid memory leak
        // ResourceBE GetData (output dataset) will add tables based on what is returned from NeedsRefresh 
        // if there are requested tables call the datasources to populate data from physical class files
        //phdataset:empty-dataset ().
        
        if valid-object(poRequest:NamedQuery) then
        do:
            case poRequest:NamedQuery:Name:
                when "JSON" then 
                    phdataset:read-json ("file",SerializeFullName).
                when "Refresh" then // read from sources and refresh json (dynamic lookups are kept as-is)
                    oResponse = RefreshData(poRequest, input-output dataset-handle phdataset by-reference, true ).
                when "Classes" then // read from sources and refresh json (dynamic lookups are kept as-is)
                    oResponse = super:FetchData(poRequest, input-output dataset-handle phdataset by-reference).
                otherwise 
                    undo, throw new IllegalOperationError(subst('FetchData with unsupported Named Query: "&1"',poRequest:NamedQuery:Name)).
            end case.         
        end.    
        else do: 
            do i = 1 to extent(poRequest:TableRequests):
                if valid-object(poRequest:TableRequests[i]:QueryDefinition) then
                do: 
                    if poRequest:TableRequests[i]:tablename = "ttEntity" then
                    do:
                        if extent(poRequest:TableRequests) > 1 then
                              undo, throw new NotImplementedError("Filter on 'Entities' when also requesting 'DataSources' and/or 'DataAccess'"). 
                        
                        oEntityRequest = poRequest:TableRequests[i].
                          
                    end.
                    else 
                        undo, throw new NotImplementedError(subst("Filter on '&1'",DatasetUtil:GetBufferByTableName(phDataset,poRequest:TableRequests[i]:tablename):serialize-name)). 
                end.
            end.
            // no queries then read from sources and update json, but don't delete anything 
            if not valid-object(oEntityRequest) then
                oResponse = RefreshData(poRequest, input-output dataset-handle phdataset by-reference,false).
            else do:
                oResponse = FetchRequestedEntitiesFromJson(oEntityRequest,input-output dataset-handle phdataset by-reference).
                
            end.        
        end.
        return oResponse.
        finally:
            delete object phdataset no-error.
        end finally.
    end method.
    
    method protected IGetDataResponse FetchRequestedEntitiesFromJson(input poEntityRequest as IGetDataTableRequest,input-output dataset-handle phDataset): 
        define variable oTableResponse as IGetDataTableResponse no-undo.
        define variable hdataset as handle no-undo.
        define variable hQuery as handle no-undo.
        define variable hFromEntity as handle no-undo.
        define variable hToEntity as handle no-undo.
        define variable iCount as integer no-undo.
         
        create dataset hDataset.
        hDataset:create-like(phDataset).
        hdataset:read-json ("file",SerializeFullName).
        hFromentity = hDataset:get-buffer-handle("ttEntity").
        hToEntity = phDataset:get-buffer-handle("ttEntity").
        create query hQuery.
        hQuery:add-buffer (hFromentity).
        hQuery:query-prepare(poEntityRequest:QueryString).
        hQuery:query-open().
        hQuery:get-first().
        
        do while hFromentity:avail:
            hToEntity:buffer-create().
            hToEntity:buffer-copy(hFromEntity).
            iCount = iCount + 1. 
            hToEntity::zz_seq = iCount. // aort in json  
            hQuery:get-next.
        end.
        
        oTableResponse = new GetDataTableResponse(poEntityRequest:Tablename).
        return new GetDataResponse(oTableResponse).
        finally:
           if valid-handle(hQuery) then
               delete object hQuery.
           delete object hDataset.
        end.            
    end method.
    
    // see comment in RefreshData where PostponeDestroy is assigned
    method protected override void DestroyDataSources(phHandle as handle):
        if PostponeDestroy = false then 
            super:DestroyDataSources(phHandle).
    end method. 
     
    method protected IGetDataResponse RefreshData(input poRequest as  IGetDataRequest,input-output dataset-handle phDataset, plSearchAndDelete as logical): 
        define variable oResponse   as IGetDataResponse no-undo.
        define variable oIterator   as IIterator no-undo.
        define variable oResourceSource as ResourceSource no-undo.
        define variable hTable      as handle no-undo.
        define variable hBuffer     as handle no-undo.
        define variable i           as integer no-undo.
        define variable oDataSourceMap as IHandleDataSourceMap no-undo.
        define variable cDump as character no-undo.
        
        // workaround bug(?) 
        // if the datasources are destroyed in the super:FetchData the dataset becomes corrupt
        // and loses ttEntity serialize-name and all ttDataSource  and ttDataAcfees are lost.
        // This always happens in v12. In v11 it sometimes wrote a lon bad string in the Json 
        // The CorruptResourcesError was thrown to workaround the v11 problem (The RespourceBE would then 
        // generate from scratch. This seemed to be too late for V12  - only tested once and got already exists errors) 
        PostponeDestroy = true. 
        oResponse = super:FetchData(poRequest, input-output dataset-handle phdataset by-reference).
     
        
        if search(SerializeFullName) <> ? then
        do on error undo, throw:
            phdataset:read-json ("file",SerializeFullName,"merge"). // merge - don't overwrite data from fetchData
            catch e as Progress.Lang.Error :
                cDump = entry(1,SerializeFullName,".") + "_bug.json".
                copy-lob file serializefullname  to file cDump.
                // The BE will catch this and regnerate the resources from source  
                undo, throw new CorruptResourcesError(SerializeFullname,e). 
            end catch.
        end.
        oDataSourceMap = GetDataSourceMap(phDataset).    
        if plSearchAndDelete then
        do i = 1 to phDataset:num-buffers:
            hBuffer = phDataset:get-buffer-handle(i).
            hTable = hBuffer:table-handle.
            oResourceSource = cast(oDataSourceMap:Get(hBuffer),ResourceSource).
            if valid-object(oResourceSource) then
                oResourceSource:Cleanup(input table-handle hTable by-reference).
        end.    
        // export the new dataset 
       
        phdataset:write-json("file",SerializeFullName,yes). 
        return oResponse.        
        finally:
            if PostponeDestroy then 
            do:
                PostponeDestroy = false.
                DestroyDataSources(phDataset) .
            end.      
        end.                               
    end method.                                                     
    
    method protected IGetDataResponse FetchSourceData(input poRequest as  IGetDataRequest,input-output dataset-handle phDataset): 
        define variable oResponse   as IGetDataResponse no-undo.
        define variable oIterator   as IIterator no-undo.
        define variable oResourceSource as ResourceSource no-undo.
        define variable hTable      as handle no-undo.
        define variable hBuffer     as handle no-undo.
        define variable i           as integer no-undo.
        define variable oDataSourceMap as IHandleDataSourceMap no-undo.
         
        oResponse = super:FetchData(poRequest, input-output dataset-handle phdataset by-reference).
         
        oDataSourceMap = GetDataSourceMap(phDataset).    
        
        do i = 1 to phDataset:num-buffers:
            hBuffer = phDataset:get-buffer-handle(i).
            hTable = hBuffer:table-handle.
            oResourceSource = cast(oDataSourceMap:Get(hBuffer),ResourceSource).
            if valid-object(oResourceSource) then
                oResourceSource:Cleanup(input table-handle hTable by-reference).
        end.    
        // export the new dataset 
       
        phdataset:write-json("file",SerializeFullName,yes). 
        return oResponse.        
                              
    end method.    
    
    method override public Object SubmitData( input poRequest as IUpdateDataRequest, input-output dataset-handle phDataset, input dataset-handle phAlertDataset, output table-handle phwarning ):
        phDataset:accept-changes ().
        phDataset:write-json ("file",SerializeFullName,true). 
        return new Object().
    end method.    
    
end class.