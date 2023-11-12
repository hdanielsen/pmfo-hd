 
 /*------------------------------------------------------------------------
    File        : ServiceManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 25 18:20:53 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.Common.IService from propath.
using Ccs.ServiceManager.ILifecycleScope from propath.
using OpenEdge.BusinessLogic.NamedQuery from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.ISupportInitialize from propath.
using OpenEdge.Core.WidgetHandle from propath.
using OpenEdge.Net.HTTP.Filter.Writer.EntityWriterRegistry from propath.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Web.DataObject.IServiceRegistry from propath.
using OpenEdge.Web.DataObject.ServiceRegistryImpl from propath.
using OpenEdge.Web.DataObject.ServiceRegistryLoader from propath.
using OpenEdge.Web.DataObject.Writer.OperationHandlerRegistry from propath.
using OpenEdge.Web.SendExceptionError from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.DatasetGetManager from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.DatasetUpdateManager from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetGetManager from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetUpdateManager from propath.
using Pmfo.Core.BusinessLogic.Delegate.FetchDelegate from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchDelegate from propath.
using Pmfo.Core.BusinessLogic.EntityRequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.FindService from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.IFindService from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.Catalog.BusinessEntityCatalogBuilder from propath.
using Pmfo.Core.Catalog.Catalog from propath.
using Pmfo.Core.Catalog.IBusinessEntityCatalogBuilder from propath.
using Pmfo.Core.Catalog.ICatalog from propath.
using Pmfo.Core.Catalog.IServiceEntityCatalogBuilder from propath.
using Pmfo.Core.Catalog.ServiceEntityCatalogBuilder from propath.
using Pmfo.Core.Common.IMimeTypes from propath.
using Pmfo.Core.Common.INameService from propath.
using Pmfo.Core.Common.MimeTypes from propath.
using Pmfo.Core.Common.NameService from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.DataLayer.DynamicDataAccess from propath.
using Pmfo.Core.DataLayer.DynamicDataSource from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
using Pmfo.Core.DataLayer.IDataAccessResource from propath.
using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.DataLayer.IServiceDataAccess from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.DataLayer.Util.ConflictService from propath.
using Pmfo.Core.DataLayer.Util.HandleDataSourceMap from propath.
using Pmfo.Core.DataLayer.Util.IConflictService from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.IHttpErrorStatus from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Manager.Manager from propath.
using Pmfo.Core.Service.BusinessEntityOperationHandler from propath.
using Pmfo.Core.Service.CatalogOperationHandler from propath.
using Pmfo.Core.Service.DataObjectService from propath.
using Pmfo.Core.Service.JsonEntityWriter from propath.
using Pmfo.Core.Service.OperationHandlerService from propath.
using Pmfo.Core.Service.ServiceEntityOperationHandler from propath.
using Pmfo.Repository.Business.LookupTypeBE from propath.
using Pmfo.Repository.Business.PublicNameBE from propath.
using Pmfo.Repository.Business.ResourceBE from propath.
using Pmfo.Repository.DataAccess.ResourceDA from propath.
using Pmfo.Repository.DataSource.DataAccessDS from propath.
using Pmfo.Repository.DataSource.DataSourceDS from propath.
using Pmfo.Repository.DataSource.EntityDS from propath.
using Pmfo.Repository.DataSource.LookupTypeDS from propath.
using Pmfo.Util.FileIO from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchFunctionDelegate from propath.
using Pmfo.Core.BusinessLogic.Delegate.FetchFunctionDelegate from propath.
using Pmfo.Util.Array from propath.

// There are tow different canFind implementaions. 
// One that uses can-find and one that uses find sonce can-find is faster we want 
// to keep both implementations and use this preprocesor where clause to ensure 
// the logic is the same
  
&scop canfindwhere ttEntity~
 where ttEntity.Resource = pcname~
 and (not valid-object(pClass) or ttEntity.TypeName = pClass:TypeName)~
 and (plOnlyPublic = false or ttEntity.Public = plOnlyPublic)~
 and (plOnlyPublic = false or lookup(ServerType:ToString(),ttEntity.ServerTypes) > 0)
                                  
class Pmfo.Core.Manager.ServiceManager inherits Manager implements IServiceManager : 
    
    /**
     @TODO 
    The temp-tables in resources-dataset are problematic as they are requested 
    in various create or canfind methods duriong load of ResourceBE and can then
    cause an endless loop since these methods will call refreshResouces since the 
    data has not been loaded yet. 
    This is currently handled by checking for the problematic names in the known
    cases and returning and starting objects when applicable before refreshResources 
    is called.
    Maybe a dedicated resource-bootstrap.json could be used for these 
    that is loaded directly before refreshResources is called so the data can 
    be found. The benefit of a .json is that it could be gnerated following the same 
    logic so that the subscribe* info is correct if we add hooks to them
    The challenge to figure out when to call refreshReources and then merge the 
    data with and with the call to refreshresources.
    Another alternative is to make ResourceBE self contained so that it 
    does not call here. Possibly by having overrides of DatasetGetHandler 
    and/or ServiceManager that handles the calls without refreshResources
    **/ 
      
    {Pmfo/Repository/Business/defs/resources-dataset.i} 
    
    define protected property ResourceBe as ResourceBE no-undo get. set.
    
      
    define public property CodeTableSourceName     as character no-undo get. set.
    define public property CheckForNewResources    as logical no-undo get. set. 
    define public property DataSourceDirectory     as char no-undo get. set.
    define public property DataAccessDirectory     as char no-undo get. set.
    define public property EntityDirectory         as char no-undo get. set.
    define public property ServiceNames            as char no-undo get. protected set.    
    
    define temp-table ttScopedService no-undo
        // as of current we only have dataset handle, which allows us to use index
        // with ILifeCycleScope we douwl need to use equals  
        // (not a big deal as there is not likely to be many scopes, 
        // so feel free to change to Object when necessary )
        field scope as handle  
        field service as Object
        index idxScope as primary unique scope service
        .
    
    define public property ServerType as ServerTypeEnum no-undo 
        get():
            if not valid-object(ServerType) then 
               ServerType = ServerTypeEnum:Default.
            return ServerType.   
        end.
        set(pType as ServerTypeEnum):
           define variable cErrorTmpl as character no-undo
               init "Server Type &1is invalid. The Server Type must be one of &2." . 
           
           define variable iCount as integer no-undo.   
           if pType:IsFlagSet(ServerTypeEnum:Authorization) then
              iCount = iCount + 1.
           if pType:IsFlagSet(ServerTypeEnum:General) then
              iCount = iCount + 1.
           if pType:IsFlagSet(ServerTypeEnum:Report) then
              iCount = iCount + 1.
           if pType:IsFlagSet(ServerTypeEnum:TextPlain) then
              iCount = iCount + 1.
          
           if iCount <> 1 then 
           do:
               StringUtil:GetMessageFromEntries(get-class(ServerTypeEnum):GetEnumNames(),"or").
               undo, throw new IllegalArgumentError(subst(cErrorTmpl,
                                                          if valid-object(pType) then quoter(pType:ToString()) + " " else "",
                                                          StringUtil:GetMessageFromEntries(get-class(ServerTypeEnum):GetEnumNames(),"or") 
                                                          )
                                                    ).
           end.                                         
           ServerType = ptype.           
        end.      
        
    define protected property FileIo as FileIO no-undo 
        get():
            if not valid-object(FileIO) then
                FileIO =  new FileIO().
            return FileIO.    
        end.
        set.
    
    // Singleton - keep alive forever - we do mnot use staitc since we may need to override in application ServiceNanager  
    // (Add create method if necessary )   
    define protected property FetchDelegate as IFetchDelegate 
       get().
           if not valid-object(FetchDelegate) then 
           do:
               FetchDelegate = new FetchDelegate().
               // ensure that initialize is called if defined 
               if type-of(FetchDelegate,ISupportInitialize) then
                  cast(FetchDelegate,ISupportInitialize):Initialize().
               else if type-of(FetchDelegate,IService) then
                  cast(FetchDelegate,IService):Initialize().
           end.
           return  FetchDelegate.    
       end.
       set.
       
     // Singleton - keep alive forever - we do mnot use staitc since we may need to override in application ServiceNanager  
    // (Add create method if necessary )   
    define protected property FetchFunctionDelegate as IFetchFunctionDelegate 
       get().
           if not valid-object(FetchFunctionDelegate) then 
           do:
               FetchFunctionDelegate = new FetchFunctionDelegate(FetchDelegate).
               // ensure that initialize is called if defined 
               if type-of(FetchFunctionDelegate,ISupportInitialize) then
                  cast(FetchFunctionDelegate,ISupportInitialize):Initialize().
               else if type-of(FetchFunctionDelegate,IService) then
                  cast(FetchFunctionDelegate,IService):Initialize().
           end.
           return  FetchFunctionDelegate.    
       end.
       set.
     
    // not in interface as of current - override in subclass initialize
    define public property CodeTableKeyType as character init "character" no-undo 
       get.
       protected set. 
    
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    method public override void dispose(  ):
        if valid-object(ResourceBe) then 
            delete object ResourceBE.
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ServiceManager (  ):
        super ().
    end constructor.
    
    // subclasses override if actions are needed on refresh
    // for example if a cached service has data that needs to be cleared if there 
    // are changes in BEs or similar 
    method protected void PostRefresh(): 
    end method.    
    
    // subclasses override if actions are needed on load of resourceBE
    method protected void PostLoadResources(): 
    end method.    
    
    // not in interface - used by tools only
    method public void RefreshResources():
        if CheckForNewResources then
        do:
            if not valid-object(ResourceBE) then
            do:
                LoadResources().
            end.    
            else if ResourceBE:NeedRefresh() then
            do: 
                dataset dsResources:empty-dataset().
                ResourceBE:GetData( dataset dsResources by-reference).
                PostRefresh(). 
            end.
        end.
        // catch this as we don't want 404 if something is missing for this BE        
        catch e as Progress.Lang.Error :
            undo, throw new ApplicationError("Resource config data is missing",e).
        end catch.
    end method.    
    
    /** create entity methods */
    method public IBusinessEntity CreateBusinessEntity( input pcName as character ):
        return cast(CreateEntity(pcName,false,?,get-class(IBusinessEntity)),IBusinessEntity).    
    end method.
    
    // start entity if get or receive hook - clientonly isw 
    method public IBusinessEntity CreateBusinessEntityForGet (pcName as char, output plHasGet as logical, output plHasReceive as logical, output plExists as logical, output plClientOnly as logical).   
        define variable oEntity as IBusinessEntity no-undo.
        
        if lookup(pcname,"resources") > 0 then
        do: 
            plExists = true.
            return ?.
        end.
        // called from refreshResources  - ResourceBE so avoid endless loop
        // - TODO add a DatasetGetHandler override that skips the call to this?
        //  could also generate a resource-bootstrap-json with the tttEntoty   
        if lookup(pcname,"entities,dataSources,dataaccess,lookupTypes") > 0 then
        do: 
            plExists = true.
            return ?.
        end.
        // BE used by NameService which is used by code generation/refreshreosurvces and nore  
        if lookup(pcname,"publicNames") > 0 then
        do: 
            plExists = true.
            return ?.
        end.
        
        
        oEntity = cast(CreateEntity(pcName,false,EntityRequestTypeEnum:Read,get-class(IBusinessEntity)),IBusinessEntity).
        find ttEntity where ttEntity.Resource = pcname no-error.
        if avail ttEntity then
             assign
                 plExists     = true
                 plClientOnly = ttEntity.ClientOnly.
            
        if valid-object(oEntity) then 
        do:
            // it should be avail, but if not then assume no subscribtion 
            if avail ttEntity then
            do:
                assign
                    plHasGet     =  ttEntity.SubscribeGet
                    plHasReceive =  ttEntity.SubscribeReceive
                    .
            end.
             
        end.    
        
        return oEntity.  
    end method.
    
    /** create entity if there are hooks needed on the client for the request type or receive */
    method public IBusinessEntity CreateBusinessEntityForRequest( input pcName as character,pEntityRequestType as EntityRequestTypeEnum, output plHasRequestedMethod as logical, output plHasReceive as logical):
        define variable oEntity as IBusinessEntity no-undo.
        
        oEntity = cast(CreateEntity(pcName,false,pEntityRequestType,get-class(IBusinessEntity)),IBusinessEntity).
        if valid-object(oEntity) then 
        do:
            // it should be avail, but if not then assume no subscribtion 
            find ttEntity where ttEntity.Resource = pcname no-error.
            if avail ttEntity then
            do:
                plHasReceive =  ttEntity.SubscribeReceive.
                case pEntityRequestType:
                    when EntityRequestTypeEnum:Update then
                        plHasRequestedMethod = ttEntity.SubscribeUpdate.
                    when EntityRequestTypeEnum:Read then
                        plHasRequestedMethod = ttEntity.SubscribeGet.
                    when EntityRequestTypeEnum:Action then
                        plHasRequestedMethod = ttEntity.SubscribeAction.
                end case.      
            end.
             
        end.    
        return oEntity.       
    end method.
    
    method public IServiceEntity CreateServiceEntity( input pcName as character ):
        return cast(CreateEntity(pcName,false,?,get-class(IServiceEntity)),IServiceEntity).    
    end method.
    
    method public IEntity CreateEntity( input pcName as character ):
        return CreateEntity(pcName,false,?,?).    
    end method.
    
    method protected IEntity CreateEntity( input pcName as character, plOnlyPublic as logical, pEntityRequestType as EntityRequestTypeEnum, pClass as Progress.Lang.Class ):
        define variable oEntity as IEntity no-undo.
        // cannot manage this with Temp-tables since it is used to populate them 
        // we use the method to allow overrides in subclass 
        if pcname = "resources" then
        do: 
            // NOte that if "resources" ever need to subscribe to receive CreateBusinessEntityForUpdate will need to be changed to handle this  
            if not valid-object(pClass) or get-class(Pmfo.Core.BusinessLogic.IBusinessEntity):Equals(pClass) then
            do:
                if EntityRequestTypeEnum:Update:Equals(pEntityRequestType) = false and EntityRequestTypeEnum:Action:Equals(pEntityRequestType) = false then
                do:
                    oEntity = new ResourceBE().
                    oEntity:initialize().
                    return oEntity.
                end.
                else
                    return ?.
            end.
            else if plOnlyPublic then
                undo, throw new NotFoundError(subst('Public Entity "&1"',pcName)).  
            else
                undo, throw new NotFoundError(subst('Entity "&1"',pcName)).  
       
        end.
        else if pcname = "lookupTypes" then
        do:
            // NOte that if "lookupTypes"" ever need to subscribe to receive CreateBusinessEntityForUpdate will need to be changed to handle this
            if not valid-object(pClass) or get-class(Pmfo.Core.BusinessLogic.IBusinessEntity):Equals(pClass) then
            do:
                if EntityRequestTypeEnum:Update:Equals(pEntityRequestType) = false and EntityRequestTypeEnum:Action:Equals(pEntityRequestType) = false then
                do:
                    oEntity = CreateLookupTypeBE().
                    oEntity:initialize().
                    return oEntity.
                end.
                else
                    return ?.
            end.
            else if plOnlyPublic then
                undo, throw new NotFoundError(subst('Public Entity "&1"',pcName)).
            else
                undo, throw new NotFoundError(subst('Entity "&1"',pcName)).

        end.
        else if pcname = "publicNames" then
        do:
            // NOte that if "publicNames" ever need to subscribe to receive CreateBusinessEntityForUpdate will need to be changed to handle this
            if not valid-object(pClass) or get-class(Pmfo.Core.BusinessLogic.IBusinessEntity):Equals(pClass) then
            do:
                if EntityRequestTypeEnum:Update:Equals(pEntityRequestType) = false and EntityRequestTypeEnum:Action:Equals(pEntityRequestType) = false then
                do:
                    oEntity = CreatePublicNameBE().
                    oEntity:initialize().
                    return oEntity.
                end.
                else
                    return ?.
            end.
            else if plOnlyPublic then
                undo, throw new NotFoundError(subst('Public Entity "&1"',pcName)).
            else
                undo, throw new NotFoundError(subst('Entity "&1"',pcName)).

        end.
        
        RefreshResources().  
        // expression with valid-object apparently no longer works when not valid in v12 
        // (probably a bug fix as it may have erroneosly resolved to ? in older versions )   
        if valid-object(pClass) then 
            find ttEntity where ttEntity.Resource = pcname
                            and ttEntity.Typename = pClass:TypeName no-error.
        else                     
            find ttEntity where ttEntity.Resource = pcname no-error.
        
        // if only update return null if no client behavior needed for update (we need to start it also if subscribereceive )
        if EntityRequestTypeEnum:Update:Equals(pEntityRequestType) and (avail ttEntity = false or (ttEntity.SubscribeUpdate = false and ttEntity.SubscribeReceive = false)) then 
            return ?. // don't throw error for this
        
        // if only action return null if no client behavior needed for action  (we need to start it also if subscribereceive )
        if EntityRequestTypeEnum:Action:Equals(pEntityRequestType) and (avail ttEntity = false or (ttEntity.SubscribeAction = false and ttEntity.SubscribeReceive = false)) then 
            return ?. // don't throw error for this
        
        // if only RequestOrReceive return null if no client behavior needed for get request or receive of request
        if EntityRequestTypeEnum:Read:Equals(pEntityRequestType) and (avail ttEntity = false or (ttEntity.SubscribeGet = false and ttEntity.SubscribeReceive = false)) then 
            return ?. // don't throw error for this
        
        
        if not avail ttEntity then
            undo, throw new NotFoundError(subst('Entity "&1"',pcName)).  
        
        // if public but the server type is not supported by the entity throw error
        if plOnlyPublic and lookup(ServerType:ToString(),ttEntity.ServerTypes) = 0 then
        do:
            // reports are managed in DOH since we want to allow server specific actions and fuctions  
            if ServerType <> ServerTypeEnum:Report then 
            do:  
                undo, throw new NotFoundError(subst('Public Entity "&1"',pcName)).
            end.  
        end.
        if plOnlyPublic and ttEntity.public = false then
            undo, throw new NotFoundError(subst('Public Entity "&1"',pcName)).  
             
        oEntity = dynamic-new ttEntity.ClassName ().
        
        
        if type-of(oEntity,ISupportInitialize) then 
            cast(oEntity,ISupportInitialize):Initialize().
        else if type-of(oEntity,IService) then
            cast(oEntity,ISupportInitialize):Initialize().
            
        return oEntity.    
    end method.
    
    /** can find entity methods */
    method public logical CanFindBusinessEntity( input pcName as character ):
        return CanFindEntity(input pcName,false,get-class(IBusinessEntity)).
    end method.
    
    method public logical CanFindBusinessEntity( input pcName as character, output plClientOnly as logical, output plHasReceiveHook as logical ).
        if lookup(pcname,"resources") > 0 then
        do: 
            return true.
        end.
        // called from refreshResources  - ResourceBE so avoid endless loop
        // - TODO add a DatasetGetHandler override that skips the call to this?
        //  could also generate a resource-bootstrap-json with the tttEntoty   
        if lookup(pcname,"entities,dataSources,dataaccess,lookupTypes") > 0 then
        do: 
            return false.
        end.
        RefreshResources().    
        find ttEntity where ttEntity.Resource = pcname and ttEntity.TypeName = get-class(Pmfo.Core.BusinessLogic.IBusinessEntity):TypeName  no-error.
        if avail ttEntity then 
        do:
            plClientOnly = ttEntity.ClientOnly.
            plHasReceiveHook = ttEntity.SubscribeReceive.
            return true.
        end.             
        else 
            return false.             
    end method. 
    
    method public logical CanFindServiceEntity( input pcName as character ):
        return CanFindEntity(input pcName,false,get-class(IServiceEntity)).
    end method.
    
    method public logical CanFindEntity( input pcName as character ):
        return CanFindEntity(input pcName,false,?).
    end method.
    
    method public logical CanFindBusinessEntity( input-output pcName as character ):
        return CanFindEntity(input-output pcName,false,get-class(IBusinessEntity)).
    end method.
    
    method public logical CanFindServiceEntity( input-output pcName as character ):
        return CanFindEntity(input-output pcName,false,get-class(IServiceEntity)).
    end method.
    
    method public logical CanFindEntity( input-output pcName as character ):
        return CanFindEntity(input-output pcName,false,?).
    end method.
    
    method public logical CanFindPublicBusinessEntity( input pcName as character ):
        return CanFindEntity(input pcName,true,get-class(IBusinessEntity)).
    end method.
    
    method public logical CanFindPublicServiceEntity( input pcName as character ):
        return CanFindEntity(input pcName,true,get-class(IServiceEntity)).
    end method.
    
    method public logical CanFindPublicEntity( input pcName as character ):
        return CanFindEntity(input pcName,true,?).
    end method.
    
    method public logical CanFindPublicBusinessEntity( input-output pcName as character ):
        return CanFindEntity(input-output pcName,true,get-class(IBusinessEntity)).
    end method.
    
    method public logical CanFindPublicServiceEntity( input-output pcName as character ):
        return CanFindEntity(input-output pcName,true,get-class(IServiceEntity)).
    end method.
    
    method public logical CanFindPublicEntity( input-output pcName as character ):
        return CanFindEntity(input-output pcName,true,?).
    end method.
    
    // main logic for can-find that does not return namre and thus can use can-find 
    method protected logical CanFindEntity( input pcName as character, plOnlyPublic as logical, pClass as Progress.Lang.Class ):
        RefreshResources().   
        return can-find ({&canfindwhere}).                    
    end method.
    
    // main logic for can-find that returns name for casing purposes 
    method protected logical CanFindEntity( input-output pcName as character, plOnlyPublic as logical, pClass as Progress.Lang.Class ):
        RefreshResources().
        find {&canfindwhere} no-error.
        if avail ttEntity then 
        do:
            pcName = ttEntity.Resource.
            return true.
        end.             
        else 
            return false.                
    end method.
    
    method public logical CanFindDataSource( input pcName as character ):
        return can-find(ttDataSource where ttDataSource.EntityName = pcname). 
    end method.    
    
    method public logical CanFindDataSource( input pcName as character, output pcType as character).
        find ttDataSource where ttDataSource.EntityName = pcName no-error. 
        if avail ttDataSource then 
        do:
            pcType = ttDataSource.Type.
            return true.
        end.    
        return false.
    end method.
    
    method public IDataAccess CreateNewDataAccess(input pcName as character):
        return new DynamicDataAccess(pcName).     
    end method.
    
    method public IDataAccess CreateDataAccess( input pcName as character ):
        define variable oDataAccess as IDataAccess no-undo.
        // The meta DataAccess class cannot be managed by the temp-table since it is used to create them 
        // handle it here to allow override in application 
        // and to allow any code to use it as a regular class 
        if pcName = "resources" then 
            return new ResourceDA(). 
        
        RefreshResources().          
        
        find ttDataAccess where ttDataAccess.Resource = pcname no-error. 
        if avail ttDataAccess and ttDataAccess.TypeName = get-class(IDataAccess):TypeName then  
        do:
            oDataAccess = dynamic-new ttDataAccess.ClassName ().
        end.
        else if avail ttDataAccess then 
            undo, throw new NotFoundError(subst("'&1' is type '&2'. DataAccess '&1'",pcName,ttDataAccess.TypeName)).
        else do:
            oDataAccess = CreateNewDataAccess(pcName).
        end.     
        if type-of(oDataAccess,ISupportInitialize) then
            cast(oDataAccess,ISupportInitialize):Initialize().
        else if type-of(oDataAccess,IService) then
            cast(oDataAccess,IService):Initialize().  
        return oDataAccess.    
    end method.
    
    // as of current only callable from GetService 
    method protected IDataAccessResource CreateDataAccessResource( input pcName as character ):
        define variable oDataAccessResource as IDataAccessResource no-undo.

        RefreshResources().          
        find ttDataAccess where ttDataAccess.Resource = pcname no-error. 
        if avail ttdataAccess then 
        do:
            oDataAccessResource = dynamic-new ttDataAccess.ClassName ().
        end.
        if type-of(oDataAccessResource,ISupportInitialize) then
            cast(oDataAccessResource,ISupportInitialize):Initialize().
        else if type-of(oDataAccessResource,IService) then
            cast(oDataAccessResource,IService):Initialize().  
        
        return oDataAccessResource.    
    end method.
    
    // must not call super ! will cause endless loop 
    method public override IServiceDataAccess CreateServiceDataAccess( input pcName as character ):
        define variable oDataAccess as IServiceDataAccess no-undo.
        
        RefreshResources().          
        find ttDataAccess where ttDataAccess.Resource = pcname
                            and ttDataAccess.TypeName = get-class(IServiceDataAccess):TypeName no-error. 
        if not avail ttDataAccess then
            undo, throw new NotFoundError(subst('ServiceDataAccess "&1"',pcName)).                      
         
        oDataAccess = dynamic-new ttDataAccess.ClassName ().
        if type-of(oDataAccess,ISupportInitialize) then
            cast(oDataAccess,ISupportInitialize):Initialize().
        else if type-of(oDataAccess,IService) then
            cast(oDataAccess,IService):Initialize().
           
        return oDataAccess.
    end method.
    
    method public IServiceDataAccess CreateLegacyDataAccess( input pcName as character ):
        define variable oDataAccess as IServiceDataAccess no-undo.
            
        RefreshResources().          
        
        find ttDataAccess where ttDataAccess.Resource = pcname
                            and ttDataAccess.LegacyName = pcName  
                            and ttDataAccess.TypeName = get-class(IServiceDataAccess):TypeName no-error. 
        if not avail ttDataAccess then
            undo, throw new NotFoundError(subst('ServiceDataAccess "&1"',pcName)).                      
         
        oDataAccess = dynamic-new ttDataAccess.ClassName ().
        if type-of(oDataAccess,ISupportInitialize) then
            cast(oDataAccess,ISupportInitialize):Initialize().
        else if type-of(oDataAccess,IService) then
            cast(oDataAccess,IService):Initialize().
           
        return oDataAccess.
    end method.
    
    method public IUpdateTarget CreateUpdateTarget( input pcName as character ):
        define variable oSource as IDataSource no-undo.
        oSource = CreateDataSource(pcName).
        if type-of(oSource,IUpdateTarget) then 
            return cast(oSource,IUpdateTarget).
        else
            undo, throw new NotFoundError(subst('UpdateTarget "&1"',pcName)).    
    end method.
    
    // allow subclass override
    method protected EntityDS CreateEntityDS():
        return new EntityDS().       
    end method.
    
     // allow subvlass override
    method protected DataSourceDS CreateDataSourceDS():
        return new DataSourceDS().       
    end method.
      
     // allow subvlass override
    method protected DataAccessDS CreateDataAccessDS():
        return new DataAccessDS().       
    end method.
    
     // The pmfo LookupTypeDS is abstract, so we need to new the application subclass 
     // default implementation - subclass will typically override with static code
    method protected LookupTypeDS CreateLookupTypeDS():
        define variable cClass as character no-undo.
        define variable oDS    as LookupTypeDS no-undo.
        cClass =  subst("&1.&2",DataSourceDirectory,"LookupTypeDS").
        oDs = dynamic-new cClass ().
        return oDs.       
    end method.
    
     // The pmfo LookupTypeBE is abstract, so we need to new the application subclass 
     // default implementation - subclass will typically override with static code
    method protected LookupTypeBE CreateLookupTypeBE():
        define variable cClass as character no-undo.
        define variable oBE    as LookupTypeBE no-undo.
        cClass =  subst("&1.&2",EntityDirectory,"LookupTypeBE").
        oBE = dynamic-new cClass ().
        return oBE.       
    end method.
    
     // The pmfo PublicNameBE is abstract, so we need to new the application subclass 
    method protected PublicNameBE CreatePublicNameBE():
        define variable cClass as character no-undo.
        define variable oBE    as PublicNameBE no-undo.
        cClass =  subst("&1.&2",EntityDirectory,"PublicNameBE").
        oBE = dynamic-new cClass ().
        return oBE.       
    end method.
        
    method public final IDataSource CreateDataSource( input pcName as character ):
        define variable oSource as IDataSource no-undo.
        
        // The meta classes cannot be managed by the temp-table since they create them 
        // handle them here to allow override in application 
        // and to allow any code to use them as regular classes 
        if pcName = "dataSources" then
            return CreateDataSourceDS().
        else if pcName = "entities" then 
            return CreateEntityDS().       
        else if pcName = "dataAccess" then 
            return CreateDataAccessDS().
        // used in resource.json generation, ans must wrok if there is none    
        else if pcName = "lookupTypes" then 
            return CreateLookupTypeDS().
         
        RefreshResources().          
        
        find ttDataSource where ttDataSource.EntityName = pcName no-error. 
        if avail ttDataSource then
        do: 
            case ttDataSource.Type:
                when "class" then 
                do:
                    oSource = dynamic-new ttDataSource.ClassName().
                end.             
                when "table" then
                do:
                    oSource = new DynamicDataSource(ttDataSource.TableName).
                end.
                when "codeTable" then
                do:
                    if CodeTableKeyType = "integer" then
                        oSource = dynamic-new CodeTableSourceName (ttDataSource.TypeKey).
                    else
                        oSource = dynamic-new CodeTableSourceName (ttDataSource.TypeCode).
                end.     
            end.
            if type-of(oSource,ISupportInitialize) then
                cast(oSource,ISupportInitialize):Initialize().
            else if type-of(oSource,IService) then
                cast(oSource,IService):Initialize().
           
            return osource.
            
        end.
        else 
            undo, throw new NotFoundError(subst('DataSource "&1"',pcName)).
        return ?. 
    end method.
    
    method protected OpenEdge.Web.DataObject.DataObjectService CreateDataObjectService(pcName as character):
        return new DataObjectService(pcName).
    end method.
    
    method public Object GetService( input poService as Progress.Lang.Class ):
        define variable oRegistry as IServiceRegistry no-undo.
        define variable oService as Object no-undo.
        define variable i as integer no-undo.
        case poService:
            when get-class(IServiceRegistry) then
            do:
                oRegistry = new ServiceRegistryImpl().
                do i = 1 to num-entries(ServiceNames):
                    oRegistry:RegisterService(CreateDataObjectService(entry(i,ServiceNames))).
                end.
                return oRegistry.
            end.
            when get-class(OperationHandlerService) then
            do:
                oService = new OperationHandlerService().
            end.
            when get-class(ServiceRegistryLoader) then
            do:
                // ignore - let the DataObjectHandler default kick in
            end.
            when get-class(IBusinessEntityCatalogBuilder) then
            do:
                oService = new BusinessEntityCatalogBuilder().
            end.
            when get-class(IServiceEntityCatalogBuilder) then
            do:
                oService = new ServiceEntityCatalogBuilder().
            end.
            when get-class(IFindService) then
            do:
                oService = new FindService().
            end.
            when get-class(IMimeTypes) then
            do:
                oService = new MimeTypes().
            end.
            when get-class(INameService) then
            do:
                oService = new NameService().
            end.
            when get-class(IConflictService) then 
            do:
                // do not keep alive since it is assumed to be needed only once in a while
                return new ConflictService().
            end.    
            when get-class(IFetchDelegate) then
            do:
                return FetchDelegate. // singleton - initialized in getter
            end.
            when get-class(IFetchFunctionDelegate) then
            do:
                return FetchFunctionDelegate. // singleton - initialized in getter
            end.
            
            otherwise        
                undo, throw new NotImplementedError(subst("GetService (Class &1)",poService:TypeName)).
        end.
        
        if type-of(oService,ISupportInitialize) then
            cast(oService,ISupportInitialize):Initialize().
        else if type-of(oService,IService) then
            cast(oService,IService):Initialize().
                     
        return oService.
        
    end method.
    
    //  allpw pverride by subclass
    method protected IDatasetGetManager CreateDatasetGetManager():
        return new DatasetGetManager().
    end method.    
    
    method public Object GetService( input poService as Progress.Lang.Class, input poScope as ILifecycleScope ):
        return GetServiceInternal(poService, poScope, false).
    end method.
    
    method public Progress.Lang.Object getExistingService(input poService as class Progress.Lang.Class, input poScope as ILifecycleScope).
        return GetServiceInternal(poService, poScope, true).
    end method.
    
    method private Object GetServiceInternal( input poService as Progress.Lang.Class, input poScope as ILifecycleScope, plExistingOnly as logical ):
        define variable oScope  as Object no-undo.
        define variable oService as Object no-undo.
        define buffer bt for ttScopedservice. 
        define variable hHandle as handle no-undo.
        Assert:NotNull(poScope, "Scope").
        oScope = poScope:getScope().
        if type-of(oScope,WidgetHandle) then
        do:
            hHandle = cast(oScope,WidgetHandle):Value.
            for each ttScopedService where ttScopedService.scope = hHandle: 
                
                 if valid-object(ttScopedService.service) then
                 do:
                     if ttScopedService.service:getclass():IsA(poService) then
                         return ttScopedService.service.
                 end.
                 else // if somehow gone get rid of the recoird 
                     delete ttScopedService.        
            end.
            
            if plExistingOnly then   
                return ?.
            
            case poService:
                when get-class(IHandleDataSourceMap) then
                do:
                    oService = new HandleDataSourceMap().
                end.
                when get-class(IDatasetGetManager) then
                do:
                    oService = CreateDatasetGetManager().
                end.
                when get-class(IDatasetUpdateManager) then
                do:
                    oService = new DatasetUpdateManager().
                end.
/*                when get-class(IValidationFindService) then      */
/*                do:                                              */
/*                    oService = GetValidationFindService(hHandle).*/
/*                end.                                             */
                otherwise
                    undo, throw new NotImplementedError(subst("GetService (Class &1), ILifecycleScope",poService:TypeName)).
            end case.
            
            if valid-object(oService) then
            do:
                create ttScopedService.
                assign
                    ttScopedService.scope   = hHandle
                    ttScopedService.service = oService
                    .
              
                if type-of(oService,ISupportInitialize) then
                    cast(oService,ISupportInitialize):Initialize().
                else if type-of(oService,IService) then
                    cast(oService,IService):Initialize().    
            end.
            return oService.    
        end. 
        else
            undo, throw new UnsupportedOperationError(subst("GetService with Scope of type &1",poScope:getScope():GetClass():TypeName)). 
    end method.
    
    /* Returns a usable instance of the requested service if it already is started.
       @param P.L.Class The service name requested
       @param character An alias for the service. The implementation may choose to ignore this value.
       @return P.L.Object A usable instance 
       @throws P.L.AppError Thrown when no implementation can be found */
  
    /* Destroys and flushes from any cache(s) objects of a specifc class scoped to the argument scope.
       @param P.L.Class The service name requested
       @param ILifecycleScope A requested scope for which to stop services. */
    method public void stopServices(input poScope as Ccs.ServiceManager.ILifecycleScope,
                                    input poService as class Progress.Lang.Class).  
        stopServicesInternal(poScope,poService).
    end method. 
    
    method public void stopServices( input poScope as ILifecycleScope ):
        stopServicesInternal(poScope,?).
    end method.
    
    method protected void stopServicesInternal(input poScope as ILifecycleScope,input poService as class Progress.Lang.Class):
        define variable oObject as Object no-undo.
        define variable hHandle as handle no-undo.
        
        // seems pointless to complain at this point so just ignore
        if valid-object(poScope) then
        do:
            oObject = poScope:getScope().
            if valid-object(oObject) and type-of(oObject,WidgetHandle) then
            do:
                hHandle = cast(oObject,WidgetHandle):Value.
                for each ttScopedService where ttScopedService.scope = hHandle:
                    if avail ttScopedService then
                    do:
                        if valid-object(ttScopedService.service) then
                        do:
                            if valid-object(poService) = false 
                            or ttScopedService.service:GetClass():isa(poService) then
                            do:    
                                if type-of(ttScopedService.service,ISupportInitialize) then
                                    cast(ttScopedService.service,ISupportInitialize):Destroy().
                                else if type-of(ttScopedService.service,IService) then
                                    cast(ttScopedService.service,IService):dispose().
                            end.
                        end.        
                        
                        if valid-object(ttScopedService.service) then
                        do:
                            if valid-object(poService) = false 
                            or ttScopedService.service:GetClass():isa(poService) then
                               delete object ttScopedService.service.
                          
                        end.    
                    end.
                    delete ttScopedService.    
                end.
            end.
        end.
    end method.
    
    
    // allowo override
    method protected ICatalog CreateCatalog(pcName as character):
        return new Catalog(pcName).
    end method.
    
    // override in subclass
/*    method protected IValidationFindService GetValidationFindService(phHandle as handle):*/
/*        return new ValidationFindService(phHandle).                                      */
/*    end method.                                                                          */
        
    method public Object GetService( input poService as Progress.Lang.Class, input pcName as character ):
        define variable oErrorStatus as StatusCodeEnum no-undo.
        define variable pOriginalError as Error no-undo.
        case poService:
            when get-class(IEntity) then
            do:
                // true only if pubiic
                return CreateEntity(pcName,true,?,?).
            end.
            when get-class(IBusinessEntity) then
            do:
                // true only if pubiic
                return CreateEntity(pcName,true,?,get-class(IBusinessEntity)).
            end.
            when get-class(IServiceEntity) then
            do:
                // true only if pubiic
                return CreateEntity(pcName,true,?,get-class(IServiceEntity)).
            end.
            when get-class(IDataAccess) then
            do:
                return CreateDataAccess(pcName).
            end.
            when get-class(IServiceDataAccess) then
            do on error undo, throw: 
                do on error undo, throw:
                    return CreateServiceDataAccess(pcName).
                    catch e1 as NotFoundError :
                        // supress, but keep track  and continue below
                        pOriginalError = e1.
                    end catch.
                end.
                return CreateServiceDataAccess(pcName).
                catch e2 as NotFoundError :
                    if valid-object(pOriginalError) then  
                        undo, throw pOriginalError.
                    else 
                        undo, throw e2.      
                end.
            end.
            when get-class(IDataAccessResource) then
            do:
                return CreateDataAccessResource(pcName).
            end.
            when get-class(IDataSource) then
            do:
                return CreateDataSource(pcName).
            end.
            when get-class(ICatalog) then 
            do:
                return CreateCatalog(pcName).
            end. 
            otherwise 
                undo, throw new NotImplementedError(subst("GetService (Class &1,name &2)",poService:TypeName,pcName)).
        end case.
        catch e as Progress.Lang.Error :
            if type-of(e,IHttpErrorStatus) then
                oErrorStatus = cast(e,IHttpErrorStatus):StatusCode.
            else 
                oErrorStatus = StatusCodeEnum:InternalServerError.    
            undo, throw new SendExceptionError(oErrorStatus,e).
        end catch.
    end method.
    
  
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method protected void ImportConfig(  ):
        define variable oJson as JsonObject no-undo.
        oJson = LoadConfig("serviceConfig.json").
        
        EntityDirectory         = oJson:GetCharacter("EntityDirectory").
        DataAccessDirectory     = oJson:GetCharacter("DataAccessDirectory").
        DataSourceDirectory     = oJson:GetCharacter("DataSourceDirectory").
        CodeTableSourceName     = oJson:GetCharacter("CodeTableSourceName").
        CheckForNewResources    = oJson:GetLogical("CheckForNewResources").
        return.

    end method.
    
    method public override void initialize(  ):
       
        ImportConfig().
        
        // Note this should be based on config and possibly moved elsewhere 
        // when the Servicemanager is supporting non web clients
         /* Add the extended JsonEntityWriter  to the registry */
        EntityWriterRegistry:Registry:Put('application/json':u,              get-class(JsonEntityWriter)).
        EntityWriterRegistry:Registry:Put('application/vnd.progress+json':u, get-class(JsonEntityWriter)).
        /* Add the OperationHandlers to the registry */
        OperationHandlerRegistry:Registry:Put(get-class(IBusinessEntity):TypeName,GetBusinessEntityOperationHandlerClass()).
        OperationHandlerRegistry:Registry:Put(get-class(IServiceEntity):TypeName,GetServiceEntityOperationHandlerClass()).
        OperationHandlerRegistry:Registry:Put(get-class(ICatalog):TypeName,GetCatalogOperationHandlerClass()).
        
        LoadResources().
        // not needed anymore if no refresh is needed
        if not CheckForNewResources then
            delete object ResourceBe. 
             
    end method.
    
    // allow subclasses to override    
    method protected Progress.Lang.Class GetBusinessEntityOperationHandlerClass():  
        return get-class(BusinessEntityOperationHandler).
    end method. 
    
    // allow subclasses to override    
    method protected Progress.Lang.Class GetServiceEntityOperationHandlerClass():  
        return get-class(ServiceEntityOperationHandler).
    end method. 
    
    // allow subclasses to override    
    method protected Progress.Lang.Class GetCatalogOperationHandlerClass():  
        return get-class(CatalogOperationHandler).
    end method. 
     
    method protected void LoadResources():
        define variable oGetDataRequest as IGetDataRequest no-undo.
        // use CreateBusinessEntity method to allow subclass override  
        if valid-object(ResourceBE) then 
            delete object ResourceBE. 
            
        ResourceBE = cast(CreateBusinessEntity("resources"),ResourceBE).
        if CheckForNewResources then 
            ResourceBE:GetData(output dataset dsResources by-reference).
        else do:
            oGetDataRequest = new GetdataRequest(new NamedQuery("JSON")).
            ResourceBE:GetData(oGetDataRequest,output dataset dsResources by-reference).
        end.
        // hook for subclasses 
        PostLoadResources().
    end method.    
    
    method public character extent GetKeys(input pcName as character).
        define variable cKeys    as character extent no-undo.
        define variable cKeyList as character no-undo.
        cKeylist  = GetKeyList(pcname).
        if cKeyList > "" then
            cKeys = Array:Split(cKeylist).
        return cKeys.    
    end method.   
    
    method public character GetKeyList(input pcName as character).
        define variable cKeyList as character  no-undo.
        
        RefreshResources().  
        
        find ttEntity where ttEntity.Resource = pcname no-error.
        if not avail ttEntity then
        do: 
            if lookup(pcName,"resources,lookupTypes,publicNames") > 0 then 
                undo, throw new UnsupportedOperationError(subst("GetKeys for meta entity &1 (before resources are loaded)",pcName)).
            undo, throw new NotFoundError(subst("Entity '&1'",pcName)). 
        end.     
        if ttEntity.TypeName <> "Pmfo.Core.BusinessLogic.IBusinessEntity" then 
            undo, throw new UnsupportedOperationError(subst("GetKeys for &1",ttEntity.TypeName)).
        else                     
            return ttEntity.KeyList. 
    end method. 
    
     
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	destructor public ServiceManager ( ):
        dispose().
	end destructor.

end class.