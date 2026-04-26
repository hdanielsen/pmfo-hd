 
/*------------------------------------------------------------------------
   File        : DataSourceErrorDS
   Purpose     : Data source Errors logged from ResourceBE when generating resources
                 These are only considered errors when actually requested from ServiceManager:CreateDataSource
                 
                 There may be records here that are logged for BEs that controls this with UpdateRequest:AddNoTargetTable
                 and there may also be BEs that uses the data source of its super BE and does no need one fpor the subclass
                 There are other valid consitions that may ned up here     
   Syntax      : 
   Description : 
   Author(s)   : hdaniels
   Created     : 7/7/2025 
   Notes       :  
 ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Manager.ServiceManager from propath.
using Pmfo.Repository.DataSource.DataAccessDS from propath.
using Pmfo.Repository.DataSource.EntityDS from propath.
using Pmfo.Repository.DataSource.ResourceSource from propath.

class Pmfo.Repository.DataSource.DataSourceErrorDS inherits ResourceSource: 
    {Pmfo/Repository/schema/dataSourceError.i reference-only}
    
    define public override property EntityName as char init "dataSourceErrors"  no-undo get. 
      
    constructor public DataSourceErrorDS (  ):
        super ().
    end constructor.
    
    method override protected character GetFileDirectory():
        return ServiceManager:DataSourceDirectory. // not needed
    end method. 
     
    method override public void FillFromObject( input poObject as Progress.Lang.Object ):
        // do nothing errors are only added for errors when when generating datasources
    end method.
    
    method protected void Bind(input table ttDataSourceError bind ):
    end method.     

    method override protected void PrepareBuffer( input phBuffer as handle ):
        define variable hTable as handle no-undo.
        hTable = phBuffer:table-handle.
        Bind(table-handle hTable bind).
        super:PrepareBuffer(phBuffer).
    end method.
    
    method public override void Cleanup (input table-handle pDataSource):
        CleanupDataSource(input table-handle pDataSource by-reference).
    end method.

    method protected void CleanupDataSource (input table ttDataSourceError):
        for each ttdatasourceError:
           delete ttDataSourceError. 
        end.    
    end method.     
end class.