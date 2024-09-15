
/*------------------------------------------------------------------------
    File        : IServiceManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 25 18:35:58 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.Common.IServiceManager from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.DataLayer.IServiceDataAccess from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.BusinessLogic.EntityRequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.

interface Pmfo.Core.Manager.IServiceManager inherits IServiceManager: 
    define public property CodeTableSourceName     as character no-undo get. set. 
    define public property CheckForNewResources    as logical   no-undo get.  
    define public property DataSourceDirectory     as character no-undo get. set.
    define public property DataAccessDirectory     as character no-undo get. set.
    define public property EntityDirectory         as character no-undo get. set.
    define public property ServiceNames            as character no-undo get.  
    define public property ServerType              as ServerTypeEnum no-undo get. set. 
    
    /* Returns a usable instance of the requested service if it already is started.
       @param P.L.Class The service class requested
       @param character An alias for the service. The implementation may choose to ignore this value.
       @return P.L.Object A usable instance 
       @throws P.L.AppError Thrown when no implementation can be found */
    method public Progress.Lang.Object getExistingService(input poService as class Progress.Lang.Class,
                                                          input poScope as Ccs.ServiceManager.ILifecycleScope).
    
    /* Destroys and flushes from any cache(s) objects of a specifc class scoped to the argument scope.
       @param P.L.Class The service class  requested
       @param ILifecycleScope A requested scope for which to stop services. */
    method public void stopServices(input poScope   as Ccs.ServiceManager.ILifecycleScope,
                                    input poService as class Progress.Lang.Class).  
    
    method public IDataSource CreateDataSource (pcName as char). 
    method public IUpdateTarget CreateUpdateTarget (pcName as char). 
    method public IDataAccess CreateDataAccess (pcName as char).
    method public IServiceDataAccess CreateServiceDataAccess (pcName as char).
    method public IServiceDataAccess CreateLegacyDataAccess (pcName as char).
    
    method public IEntity CreateEntity (pcName as char).   
    method public IBusinessEntity CreateBusinessEntity (pcName as char).   
    method public IBusinessEntity CreateDynamicBusinessEntity (phContextHandle as handle,poRelation as IRelation).   
    method public IBusinessEntity CreateBusinessEntityForRequest   (pcName as char, pEntityRequestType as EntityRequestTypeEnum,  output plHasRequestedMethod as logical, output plHasReceive as logical).   
    method public IBusinessEntity CreateBusinessEntityForGet       (pcName as char, output lHasGet as logical, output plHasReceive as logical,output plExists as logical, output plClientOnly as logical).   
    method public IBusinessEntity CreateBusinessEntityForTableRead (pcName as character).   
    method public logical BusinessEntityHasRequestType             (pcName as char, pEntityRequestType as EntityRequestTypeEnum).   
    
    
    method public IServiceEntity CreateServiceEntity (pcName as char).   
     
    method public logical CanFindEntity( pcName as character ).
    method public logical CanFindBusinessEntity( input pcName as character ).
    method public logical CanFindBusinessEntity( input pcName as character, output plClientOnly as logical, output plHasReceiveHook as logical ).
    method public logical CanFindServiceEntity( input pcName as character ).
    
    // trick method that also ensures correct spelling for serialize-name
    method public logical CanFindEntity( input-output pcName as character ).
    // trick method that also ensures correct spelling for sertialize-name
    method public logical CanFindBusinessEntity( input-output pcName as character ).
    // trick method that also ensures correct spelling for sertialize-name
    method public logical CanFindServiceEntity( input-output pcName as character ).
    
    method public logical CanFindPublicEntity( pcName as character ).
    method public logical CanFindPublicBusinessEntity( input pcName as character ).
    method public logical CanFindPublicServiceEntity( input pcName as character ).
    // trick method that also ensures correct spelling for serialize-name
    method public logical CanFindPublicEntity( input-output pcName as character ).
    // trick method that also ensures correct spelling for sertialize-name
    method public logical CanFindPublicBusinessEntity( input-output pcName as character ).
    // trick method that also ensures correct spelling for sertialize-name
    method public logical CanFindPublicServiceEntity( input-output pcName as character ).
    
    method public logical CanFindDataSource( input pcName as character ).
    method public logical CanFindDataSource( input pcName as character, output pcType as character).
    
    method public character extent GetKeys(input pcName as character).
    method public character GetKeyList(input pcName as character).
      
end interface.