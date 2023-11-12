 
 /*------------------------------------------------------------------------
    File        : Manager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Apr 27 01:24:23 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Ccs.Common.IManager from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.DataLayer.IServiceDataAccess from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Util.FileUtil from propath.
using Pmfo.Util.JsonUtil from propath.

class Pmfo.Core.Manager.Manager abstract implements IManager:
    
    constructor public Manager ( ):
        super().
    end constructor.
    
    method public abstract void Initialize():
    method public abstract void Dispose():
    
    method protected JsonObject LoadConfig(pcFileName as char):
        define variable oJson as JsonObject no-undo.
        
        define variable cConfigFile as character no-undo.
      
        file-info:file-name = Pmfo.Core.Manager.StartupManager:ConfigDirectory.  
        
        if file-info:full-pathname = ? then 
            undo, throw new IllegalOperationError(subst("Config Directory [&1] not found in propath",Pmfo.Core.Manager.StartupManager:ConfigDirectory)).
        
        cConfigfile = FileUtil:AppendToPath(file-info:full-pathname,pcFileName).
        if search(cConfigfile) = ? then
            undo, throw new IllegalOperationError(subst("Service Config File [&1] not found",cConfigfile)).
        
        oJson = JsonUtil:ReadJsonFile(cConfigfile).
        
        return oJson.

    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method protected IServiceDataAccess CreateServiceDataAccess(pcname as character):
        return cast(Application:ServiceManager,IServiceManager):CreateServiceDataAccess(pcname).    
    end.
        
end class.