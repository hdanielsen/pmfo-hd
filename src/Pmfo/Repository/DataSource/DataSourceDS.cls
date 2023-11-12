 
 /*------------------------------------------------------------------------
    File        : DataSourceDS
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Mar 22 11:57  EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Manager.ServiceManager from propath.
using Pmfo.Repository.DataSource.DataAccessDS from propath.
using Pmfo.Repository.DataSource.EntityDS from propath.
using Pmfo.Repository.DataSource.ResourceSource from propath.

class Pmfo.Repository.DataSource.DataSourceDS inherits ResourceSource: 
    {Pmfo/Repository/schema/dataSource.i reference-only}
    
    define public override property EntityName as char init "dataSources"  no-undo get. 
      
    constructor public DataSourceDS (  ):
        super ().
    end constructor.
    
    method override protected character GetFileDirectory():
        return ServiceManager:DataSourceDirectory.
    end method. 
     
    method override public void FillFromObject( input poObject as Progress.Lang.Object ):
        define variable oSource as IDataSource no-undo.
        
        if type-of(poObject,IDataSource) then 
        do on error undo, throw:
            oSource = cast(poObject,IDataSource).
            
            if oSource:EntityName = "" then
                undo, throw new ApplicationError(GetCodeCreateErrorMessage("DataSource","with blank EntityName property",oSource:getClass():TypeName,?)). 
            
            if oSource:EntityName = Entityname then 
            do:
                undo, throw new ApplicationError(ReservedResourceMessage("DataSource" ,oSource:GetClass():TypeName, "EntityName",EntityName)).
            end.       
            else if oSource:EntityName = "entities" then 
            do:
                undo, throw new ApplicationError(ReservedResourceMessage("DataSource" ,oSource:GetClass():TypeName, "EntityName","entitites")).
            end.       
            else if oSource:EntityName = "dataAccess" then 
            do:
                undo, throw new ApplicationError(ReservedResourceMessage("DataSource" ,oSource:GetClass():TypeName, "EntityName","dataAccess")).
            end.   
             
            create ttDataSource.
            assign
                ttDataSource.ClassName  = oSource:getClass():TypeName
                ttDataSource.EntityName = oSource:EntityName 
                ttDataSource.Type       = "Class"
                .
           
            catch e1 as ApplicationError :
                FillErrors:Add(e1).
                undo, throw e1.
            end catch.    
            catch e as Progress.Lang.Error :
                if avail ttDataSource then
                    delete ttDataSource.
                FillErrors:Add(e1).
                undo, throw new ApplicationError(GetCodeCreateErrorMessage("DataSource",oSource:EntityName,oSource:getClass():TypeName,e)). 
            end catch.
        end.
    end method.
    
    method protected void Bind(input table ttDataSource bind ):
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

    method protected void CleanupDataSource (input table ttDataSource):
        for each ttdatasource where ttDataSource.ClassName > "":
            if  search(subst("&1.r",replace(ttDataSource.ClassName,".","/"))) = ? 
            and search(subst("&1.cls",replace(ttDataSource.ClassName,".","/"))) = ? then
                delete ttDataSource. 
        end.    
    end method.     
end class.