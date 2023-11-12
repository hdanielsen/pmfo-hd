
/*------------------------------------------------------------------------
    File        : IDataService
    Purpose     : Common interface for objects that are accessible 
                  from ServiceManager GetService(class, name) 
                  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 25 19:08:20 EST 2019
    Notes       : The name is implementeded inconsistently as of current
                  - IResource - defines resource as the name (IEntity 
                    and IDataAccessResource
                  - IDataSource - EntityName      
                  - ICatalog  - hard-coded as Catalog 
  ----------------------------------------------------------------------*/

using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
 
interface Pmfo.Core.Common.IDataService:  
   
end interface.