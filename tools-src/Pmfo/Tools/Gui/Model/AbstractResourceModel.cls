 
 /*------------------------------------------------------------------------
    File        : AbstractResourceModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 09 19:59:17 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Common.INameService from propath.
using Ccs.Common.Application from propath.
using Pmfo.Tools.Gui.Model.Model from propath.
using Pmfo.Tools.Appbuilder.ICodeTableModel from propath.

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.AbstractResourceModel abstract inherits Model: 
     
     &scop mode protected
     {Pmfo/Tools/AppBuilder/resourcedata.i}
     &undefine mode
     
     define dataset dsFile for resourceData.
     
     define public property CodeTableModel as ICodeTableModel no-undo get. set.
     define public property DefinitionDir  as char no-undo init "schema" get. set.
      
     // Not necessary to have setter any more since we are suing the service
     define public property NameService as INameService no-undo  
        get():
            if not valid-object(NameService) then 
                NameService = cast(Application:ServiceManager:getService(get-class(INameService)),INameService).    
            return NameService.    
        end get. 
        set.
     
     define protected variable KeyCounter as integer no-undo. 
     
     constructor public AbstractResourceModel (name as char):
         super(name,dataset dsFile:handle).
     end.    
     
     method public handle GetTable(pcTable as char):
        define variable hBuffer as handle no-undo.
        hBuffer = dataset dsFile:get-buffer-handle(pctable).
        return hBuffer:table-handle. 
     end.   
     
     method public void AfterRow(input table resourcedata  ):
        
        if resourceData.resourceNum = 0 then 
        do:
            assign
                resourceData.resourceNum    = KeyCounter + 1  
                KeyCounter                  = resourceData.resourceNum.
        end.    
        assign
            resourceData.classname       = caps(substr(resourceData.className,1,1)) + substr(resourceData.className,2)
            
            resourceData.includefile     = lc(substr(resourceData.className,1,1)) + substr(resourceData.className,2)
            resourceData.entityName      = NameService:PluralizeName(lc(substr(resourceData.className,1,1)) + substr(resourceData.className,2)) 
            resourceData.resourceName    = resourceData.entityName
            resourceData.NotInUse        = NameService:isTableNotInUse(resourceData.tableName) when resourceData.tableName > ""
            resourceData.dataSource      = resourceData.className + "DS"  
            resourceData.businessEntity  = resourceData.className + "BE"  
            resourceData.tempdb          = resourceData.databaseName = "tempdb"
            resourceData.tempTableName   = "tt" + resourceData.className
            resourceData.beforeTableName = "bi" + resourceData.className
            
            . 
            if resourceData.NotInUse then 
                resourceData.entityName = "<not in use>".
                
    end method.
    
    

end class.