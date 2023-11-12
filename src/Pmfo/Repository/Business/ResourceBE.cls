 
 /*------------------------------------------------------------------------
    File        : ResourceBE
    Purpose     : Manage resources
                  Provides data for the ServiceManager factory methods
                  Provides data for the catalog
                  Is used and updated by code generator 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Mar 21 17:56:04 EDT 2019  
    Notes       : Has methoids and logic to refresh its data from source code    
  ----------------------------------------------------------------------*/
 
block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.BusinessLogic.NamedQuery from propath.
using OpenEdge.DataAdmin.DataAdminService from propath.
using OpenEdge.DataAdmin.IDataAdminService from propath.
using OpenEdge.DataAdmin.ITable from propath.
using OpenEdge.DataAdmin.ITableSet from propath.
using OpenEdge.DataAdmin.Lang.Collections.IIterator from propath.
using Pmfo.Core.BusinessLogic.BusinessEntity from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.Common.INameService from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Repository.DataAccess.ResourceDA from propath.
using Progress.Lang.Object from propath.
using Pmfo.Repository.Error.CorruptResourcesError from propath.
using Pmfo.Core.Common.WarningMessages from propath.

class Pmfo.Repository.Business.ResourceBE inherits BusinessEntity: 
    
    {Pmfo/Repository/Business/defs/resources-dataset.i} 
    {Pmfo/Repository/schema/lookuptype.i}
    
    define temp-table ttTableMap no-undo 
        field name as character
        field publicName as character
        index idx as primary unique publicName.
          
    // only for GetData with no request  
    define protected property LastGetTime as datetime-tz no-undo get. set.
    define public property LastModified as datetime-tz no-undo
        get():  
            return cast(this-object:DataAccess,ResourceDA):LastModified.
        end.
    
    define protected property NameService as INameService no-undo 
       get():
           if not valid-object(NameService) then 
           do:
               NameService = cast(Application:ServiceManager:getService(get-class(INameService)),INameService).
           end.
           return NameService.
       end.        
       set. 
          
    constructor public ResourceBE (  ):
        super (dataset dsResources:handle). 
    end constructor.
    
    method public logical NeedRefresh():
        return cast(this-object:DataAccess,ResourceDA):NeedRefresh(LastGetTime).
    end.     
    
    method public void RefreshData():
        define variable oGetDataRequest       as Pmfo.Core.BusinessLogic.IGetDataRequest no-undo.
        define variable oGetDataTableRequests as IGetDataTableRequest extent no-undo.
        define variable i                     as integer no-undo.
        define variable hDataset              as handle no-undo.
        define variable lRegenerate           as logical no-undo.
        hDataset = Datasets[1].
        extent(oGetDataTableRequests) = hDataset:num-buffers.
        do i = 1 to hDataset:num-buffers:
            oGetDataTableRequests[i] = new GetDataTableRequest(hDataset:get-buffer-handle(i):table-handle:name).
        end.    
        oGetDataRequest = new GetDataRequest(oGetDataTableRequests).
        cast(oGetDataRequest,GetDataRequest):NamedQuery = new NamedQuery("Refresh"). // Tell ResourceDA that this is a refresh 
        do on error undo, throw:
            cast(this-object:DataAccess,ResourceDA):FetchData(oGetDataRequest, input-output dataset-handle hDataset by-reference).  
            LastGetTime = now.
            catch e as CorruptResourcesError:
                hDataset:empty-dataset.
                lRegenerate = true.   
            end catch.
        end.    
        if lRegenerate then
            ReadFromSourceAndCacheData().
        
    end method.    
     
    method public void ReadFromSourceAndCacheData():
        define variable oGetDataRequest       as Pmfo.Core.BusinessLogic.IGetDataRequest no-undo.
        define variable oGetDataTableRequests as IGetDataTableRequest extent no-undo.
        define variable i                     as integer no-undo.
        define variable hDataset              as handle no-undo.
        define variable hEntityTable          as handle no-undo.
        define variable hDataSourceTable      as handle no-undo.
        define variable hLookupTypeTable      as handle no-undo. 
                
        hDataset = Datasets[1].
        extent(oGetDataTableRequests) = hDataset:num-buffers.
        do i = 1 to hDataset:num-buffers:
            oGetDataTableRequests[i] = new GetDataTableRequest(hDataset:get-buffer-handle(i):table-handle:name).
        end.    
        oGetDataRequest = new GetDataRequest(oGetDataTableRequests).
        cast(oGetDataRequest,GetDataRequest):NamedQuery = new NamedQuery("Classes"). // Tell ResourceDA to get resources from source code (no datasource of type "CodeTable or "Table") 
        cast(this-object:DataAccess,ResourceDA):FetchData(oGetDataRequest, input-output dataset-handle hDataset by-reference).  
        hEntityTable = hDataset:get-buffer-handle("ttEntity"):table-handle.
        hDataSourceTable = hDataset:get-buffer-handle("ttDataSource"):table-handle.
        // fetch all lookup types from the lookup table  
        FetchLookupTypes(output table-handle hLookupTypeTable ).
        WarningMessages = new WarningMessages(hdataset). 
        // Add datasourced of type "CodeTable" from Lookuptypes and type "Table" using DataAdminService  
        AddDynamicDataSources(input table-handle hEntityTable by-reference, input table-handle hDataSourceTable by-reference, input table-handle hLookupTypeTable by-reference ).
        // save to resources.json
        UpdateData(input-output dataset-handle hDataset by-reference). 
        empty temp-table ttTableMap.
            
    end method.
    
     method public void ReadFromSourceAndCacheData(output c as char ):
        define variable oGetDataRequest       as Pmfo.Core.BusinessLogic.IGetDataRequest no-undo.
        define variable oGetDataTableRequests as IGetDataTableRequest extent no-undo.
        define variable i                     as integer no-undo.
        define variable hDataset              as handle no-undo.
        define variable hEntityTable          as handle no-undo.
        define variable hDataSourceTable      as handle no-undo.
        define variable hLookupTypeTable      as handle no-undo. 
                
        hDataset = Datasets[1].
        extent(oGetDataTableRequests) = hDataset:num-buffers.
        do i = 1 to hDataset:num-buffers:
            oGetDataTableRequests[i] = new GetDataTableRequest(hDataset:get-buffer-handle(i):table-handle:name).
        end.    
        oGetDataRequest = new GetDataRequest(oGetDataTableRequests).
        cast(oGetDataRequest,GetDataRequest):NamedQuery = new NamedQuery("Classes"). // Tell ResourceDA to get resources from source code (no datasource of type "CodeTable or "Table") 
        cast(this-object:DataAccess,ResourceDA):FetchData(oGetDataRequest, input-output dataset-handle hDataset by-reference).  
        hEntityTable = hDataset:get-buffer-handle("ttEntity"):table-handle.
        hDataSourceTable = hDataset:get-buffer-handle("ttDataSource"):table-handle.
        // fetch all lookup types from the lookup table  
        FetchLookupTypes(output table-handle hLookupTypeTable ).
        // Add datasourced of type "CodeTable" from Lookuptypes and type "Table" using DataAdminService  
        AddDynamicDataSources(input table-handle hEntityTable by-reference, input table-handle hDataSourceTable by-reference, input table-handle hLookupTypeTable by-reference ).
        // save to resources.json
        UpdateData(input-output dataset-handle hDataset by-reference). 
        empty temp-table ttTableMap.    
    end method.
    
    method protected void AddDynamicDataSources(input table ttEntity, input table ttDataSource, input table ttLookupType):
        define variable i as integer no-undo.
        define variable cEntity as character no-undo.
        define variable lok as logical no-undo.
     //   define buffer bttchildEntity for ttEntity. 
        GetDbTables(output table ttTablemap by-reference).
        for each ttEntity where ttEntity.TypeName = get-class(IBusinessEntity):TypeName: 
            if ttEntity.IsDataContainer = false and not can-find(ttDataSource where ttDataSource.EntityName = ttEntity.Resource) then
            do:  
                lOk = AddDynamicDataSource(ttEntity.Resource, input table ttEntity by-reference,input table ttDataSource by-reference, input table ttLookupType by-reference,input table ttTablemap by-reference).
/*                if lok = false and ttEntity.ClientOnly = false then                                       */
/*                   AddWarning(subst("Business Entity '&1' does not have a DataSource",ttEntity.Resource)).*/
            end.
            // check for datasets that uses include only 
            do i = 1 to num-entries(ttEntity.EntityReferences):
                cEntity  = entry(i,ttEntity.EntityReferences).
                if not can-find(ttDataSource where ttDataSource.EntityName = cEntity) then
                do: 
                    lok = AddDynamicDataSource(cEntity, input table ttEntity by-reference,input table ttDataSource by-reference, input table ttLookupType by-reference,input table ttTablemap by-reference).
/*                    if lok = false then                                                                                                       */
/*                    do:                                                                                                                       */
/*                        //  Define a Data Source in the Public Names config (publicNames.json) or define the Business Entity as Client Only"  */
/*                        find bttchildEntity where bttchildEntity.Resource = cEntity no-error.                                                 */
/*                        if avail bttchildEntity = false or bttchildEntity.Clientonly = false then                                             */
/*                            AddWarning(subst("Business Entity '&1' has a dataset entity '&2' with no data source",ttEntity.Resource,cEntity)).*/
/*                    end.                                                                                                                      */
                end.
            end.
                
        end.    
    end method.     
    
    method protected logical AddDynamicDataSource(input pcEntity as character, input table ttEntity, input table ttDataSource, input table ttLookupType, input table  ttTablemap  ):
        if not can-find(ttDataSource where ttDataSource.EntityName = pcEntity) then
        do: 
            find ttLookupType where ttLookupType.EntityName = pcEntity no-error.
            if ambiguous ttLookupType then 
                undo, throw new IllegalStateError(subst("More than one lookup currently resolves its entity name to '&1'. The NameService must be updated to disambuguate these lookups.",pcEntity)). 
            if avail ttLookupType then
            do:     
                create ttDataSource. 
                assign 
                    ttDatasource.EntityName = ttLookupType.EntityName
                    ttDatasource.TypeCode   = ttLookupType.TypeCode
                    // just for consistency with other datasources since the typekey is not unique and init 0  
                    ttDatasource.TypeKey    = if ttLookupType.TypeKey = ? then 0 else ttLookupType.TypeKey
                    ttDatasource.Type       = "CodeTable".   
                return true.     
            end.
            else if ttEntity.ClientOnly = false then
            do: 
                find ttTableMap where ttTableMap.publicName = pcEntity no-error.
                if avail ttTableMap then 
                do:
                    create ttDataSource. 
                    assign 
                        ttDatasource.EntityName = pcEntity
                        ttDataSource.TableName  = ttTableMap.name 
                        ttDatasource.Type       = "Table".
                    return true.        
                end.      
            end.
            return false.    
        end. 
        return true.
    end method.
     
    method protected void FetchLookupTypes(output table-handle phTable):
        define variable oBe as IBusinessEntity no-undo.
        define variable oGetDataRequest       as Pmfo.Core.BusinessLogic.IGetDataRequest no-undo.
        define variable oGetDataTableRequest  as IGetDataTableRequest no-undo.
        define variable hDataset as handle no-undo.
        oBe = cast(Application:ServiceManager,IServiceManager):CreateBusinessEntity("lookupTypes").
        oGetDataTableRequest = new GetDataTableRequest("ttLookupType").
        oGetDataRequest = new GetDataRequest(oGetDataTableRequest).
        oBe:GetData(oGetDataRequest, output dataset-handle hDataset by-reference).  
        phTable = hDataset:get-buffer-handle(1):table-handle.
       
        
    end method.
    
    method public void GetDbTables( output table ttTableMap   ):  
        define variable service     as IDataAdminService no-undo.
        define variable oTables     as ITableSet no-undo.
        define variable oIterator   as IIterator no-undo.
      
        define variable cName       as character no-undo.
        define variable cPublicName as character no-undo.
        define variable cdup        as character no-undo.
        define variable iDb         as integer no-undo.
            
        define buffer bttTableMap for tttableMap.
    
        do iDb = 1 to num-dbs:
            service = new DataAdminService(ldbname(iDB)).
            oTables = service:getTables().
            oIterator = oTables:Iterator().
        
            do while oIterator:HasNext()
            on error undo, throw.
                
                cName = cast(oIterator:Next(),ITable):Name.
                create ttTableMap.
                assign 
                    ttTableMap.name = cName
                    ttTableMap.PublicName = NameService:EntityName(cName)
                    .
                catch e as Progress.Lang.Error :
                    if e:GetMessageNum(1) = 132 then
                    do:
                        find first bttTableMap where bttTableMap.PublicName =  NameService:EntityName(cName) no-error.
                        if avail bttTableMap then 
                           cDup = BttTableMAp.name.
                        
                        if avail ttTableMap then 
                            delete ttTableMap. 
                        undo, throw new IllegalStateError(subst("Both table '&1' and '&2' currently resolves its entity name to '&3'. The NameService must be updated to disambuguate these tables.",
                                                                cname,
                                                                cDup,
                                                                NameService:EntityName(cName))). 
                          
                    end.  
                    else 
                        undo, throw e.   
                end catch.     
            end.
            delete object service.
        end. 
    end.
    
    method public override Object UpdateData( input-output dataset-handle phDataset):
        define variable hWarning as handle no-undo.
        define variable hDummy as handle no-undo.
        define variable oDummy   as IUpdateDataRequest no-undo.  // pick the right overload (this has been fixed in ResourceDA, but keep )
        return this-object:DataAccess:SubmitData(oDummy, input-output dataset-handle phDataset, input dataset-handle hDummy, output table-handle hWarning).
    end method.
    
    // this is specialized to build the request from time stamp info in the DataAccess
    method override public IGetDataResponse GetData( output dataset-handle phDataset ):
        define variable cRefreshTables        as character extent no-undo.
        define variable oResourceDA           as ResourceDA no-undo.
        define variable oGetDataRequest       as IGetDataRequest no-undo.
        define variable oGetDataTableRequests as IGetDataTableRequest extent no-undo.
        define variable i                     as integer no-undo.
        define variable oResponse             as IGetDataResponse no-undo. 
        define variable lRegenerate           as logical no-undo.
        
        oResourceDa = cast(this-object:DataAccess,ResourceDA).  
        cRefreshTables = oResourceDA:NeedRefreshTables().
        if extent(cRefreshTables) <> ? then
        do:
            extent(oGetDataTableRequests) = extent(cRefreshTables).
            do i = 1 to extent(cRefreshTables):
                oGetDataTableRequests[i] = new GetDataTableRequest(cRefreshTables[i]).
            end.    
            oGetDataRequest = new GetDataRequest(oGetDataTableRequests).      
        end.
        else do:
            oGetDataRequest = new GetdataRequest(new NamedQuery("JSON")).
        end.
        do on error undo, throw:
            LastGetTime = now.
            oResponse = this-object:GetData(oGetDataRequest,output dataset-handle phDataset by-reference).
            catch e as CorruptResourcesError:
                phDataset:empty-dataset.
                lRegenerate = true.   
            end catch.
        end.
        
        if lRegenerate then  
            oResponse = GenerateAndGetData(output dataset-handle phDataset by-reference).   
       
        return oResponse.
        
    end method.
    
    method public IGetDataResponse GenerateAndGetData( output dataset-handle phDataset ):
        ReadFromSourceAndCacheData().
        return GetDataFromCache(output dataset-handle phDataset by-reference). 
    end method.
    
    method public IGetDataResponse GetDataFromCache( output dataset-handle phDataset ):
        define variable oResourceDA           as ResourceDA no-undo.
        define variable oGetDataRequest       as IGetDataRequest no-undo.
        define variable oResponse             as IGetDataResponse no-undo. 
      
        oResourceDa = cast(this-object:DataAccess,ResourceDA).  
        oGetDataRequest = new GetdataRequest(new NamedQuery("JSON")).
        oResponse = this-object:GetData(oGetDataRequest,output dataset-handle phDataset by-reference).
        LastGetTime = now.
        return oResponse.
    end method.
    
  //  method override void initialize():
/*        if cast(Application:ServiceManager,ServiceManager):CodeTableKeyType = "integer" then*/
/*            buffer ttDataSource:buffer-field("typecode"):serialize-hidden = true.           */
/*        else                                                                                */
/*            buffer ttDataSource:buffer-field("typekey"):serialize-hidden = true.            */
  //  end method.    
    
end class.