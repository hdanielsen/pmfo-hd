 
 /*------------------------------------------------------------------------
    File        : DataSourceDS
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 25 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.Common.IResource from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
using Pmfo.Core.DataLayer.IDataAccessResource from propath.
using Pmfo.Core.DataLayer.IServiceDataAccess from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Manager.ServiceManager from propath.
using Pmfo.Repository.DataSource.ResourceSource from propath.

class Pmfo.Repository.DataSource.DataAccessDS inherits ResourceSource: 
    {Pmfo/Repository/schema/dataAccess.i reference-only}
    
    define public override property EntityName as char init "dataAccess"  no-undo get. 
      
    constructor public DataAccessDS (  ):
        super ().
    end constructor.
    
    method override protected character GetFileDirectory():
        return ServiceManager:DataAccessDirectory.
    end method.    
    
    method override public void FillFromObject( input poObject as Progress.Lang.Object ):
        define variable oResource as IResource no-undo.
        define variable cLegacyName as character no-undo.
        if type-of(poObject,IDataAccessResource) then 
        do on error undo, throw:
            oResource = cast(poObject,IDataAccessResource). 
            if oResource:Resource = "" then
                undo, throw new ApplicationError(GetCodeCreateErrorMessage("DataAccess Resource","with blank Resource property",oResource:getClass():TypeName,?)). 
            
            if oResource:Resource = "resources" then 
            do:
                undo, throw new ApplicationError(ReservedResourceMessage("DataAccess" ,oResource:GetClass():TypeName, "Resource","resources")).
            end.  
              
            create ttDataAccess.
            assign
                ttDataAccess.ClassName  = oResource:getClass():TypeName
                ttDataAccess.Resource   = oResource:Resource 
                .    
         
            if type-of(oResource,IDataAccess) then
                 ttDataAccess.TypeName = get-class(IDataAccess):TypeName.
            else if type-of(oResource,IServiceDataAccess) then 
            do:
                 ttDataAccess.TypeName = get-class(IServiceDataAccess):TypeName.
                 cLegacyName = cast(oResource,IServiceDataAccess):LegacyName.
                 if cLegacyName > "" then
                     ttDataAccess.LegacyName = cLegacyName. 
            end.
            else
                 ttDataAccess.TypeName = get-class(IDataAccessResource):TypeName.     
            catch e1 as ApplicationError:
                undo, throw e1.
            end catch.        
            catch e2 as Progress.Lang.Error :
                if avail ttdataAccess then 
                    delete ttDataAccess.
                undo, throw new ApplicationError(GetCodeCreateErrorMessage("DataAccess Resource",oResource:Resource,oResource:getClass():TypeName,e2),e2). 
            end catch.        
        end.    
    end method.
     
    method protected void Bind(input table ttdataAccess bind ):
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

    method protected void CleanupDataSource (input table ttDataAccess):
       // define variable icount as int no-undo.
        for each ttDataAccess where ttDataAccess.ClassName > "":
         //   iCount = iCount + 1.
            if search(subst("&1.r",replace(ttDataAccess.ClassName,".","/"))) = ?
            and search(subst("&1.cls",replace(ttDataAccess.ClassName,".","/"))) = ? then
                delete ttDataAccess.
        end. 
     
    end method.     
end class.