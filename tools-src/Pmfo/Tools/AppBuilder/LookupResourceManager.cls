 
 /*------------------------------------------------------------------------
    File        : LookupResourceManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 09 20:49:45 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.AppBuilder.CodeGenerator from propath.
using Pmfo.Core.Common.INameService from propath.
using Pmfo.Tools.Gui.Model.LookupResourceModel from propath.
using Pmfo.Tools.Gui.Model.IResourceModel from propath.
using Pmfo.Tools.Appbuilder.ICodeTableModel from propath.
using Pmfo.Tools.Gui.Model.BusinessEntityModel from propath.
using Pmfo.Tools.AppBuilder.ErrorTracker from propath.
using Pmfo.Tools.AppBuilder.CodeConverter from propath.
using Pmfo.Tools.Gui.Model.SdoModel from propath.
using Pmfo.Tools.Gui.Model.DlpModel from propath.
using Pmfo.Core.Error.ApplicationError from propath.

block-level on error undo, throw.

class Pmfo.Tools.AppBuilder.LookupResourceManager: 
    {Pmfo/Tools/AppBuilder/resourcedata.i}
     
    define public property CodeGenerator as CodeGenerator  no-undo get. protected set.
    define public property CodeConverter as CodeConverter  no-undo get. protected set.
    define public property Model         as IResourceModel no-undo get. protected set.
    
    define public property DefinitionDir as char no-undo 
        get:
            return CodeGenerator:DefinitionDir.
        end.    
     
    define public property DataSourceDir as char no-undo 
        get:
            return CodeGenerator:DataSourceDir.
        end.  
    define public property BusinessDir as char no-undo 
        get:
            return CodeGenerator:BusinessDir.
        end.  
      
     
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public LookupResourceManager (pModel as LookupResourceModel, pGenerator as CodeGenerator, pCodeModel as ICodeTableModel , pCodeconverter as CodeConverter ):
        super ().
        Model = pModel.
        CodeGenerator = pGenerator.
        CodeConverter = pCodeConverter.
        pModel:CodeTableModel = pCodeModel.
      
    end constructor.
    
    method public void Generate(plDefs as logical, plSource as logical, plBe as logical):  
         
        define variable hTable as handle no-undo.
        Model:FetchData().
        hTable = Model:GetTable(temp-table resourceData:name).
        
        ForEachResource(input table-handle hTable,"",plDefs,plSource,plBe).
        
        
    end method.
    
    method public void Generate(pcBegins as character, plDefs as logical, plSource as logical, plBe as logical):  
         
        define variable hTable as handle no-undo.
        Model:FetchData().
        hTable = Model:GetTable(temp-table resourceData:name).
        
        ForEachResource(input table-handle hTable, pcBegins ,plDefs,plSource,plBe).
        
        
    end method.
    
    method protected void ForEachResource(table resourceData,pcBegins as character,plDefs as logical, plSource as logical, plBe as logical):
        current-window:width = 300.
        for each resourceData 
            where 
               resourceData.codeTableName begins pcBegins
               and (
                  (plBe = true and search( subst("&1/&2.cls",right-trim(BusinessDir,"/"),resourceData.businessEntity)) =  ?)
                  and (plDefs or search( subst("&1/&2.i",right-trim(DefinitionDir,"/"),resourceData.includefile)) <>  ?)  
                  or  
                  (plSource = true and search( subst("&1/&2.cls",right-trim(DataSourceDir,"/"),resourceData.dataSource)) =  ?)
                  or  
                  (plDefs = true and search( subst("&1/&2.i",right-trim(DefinitionDir,"/"),resourceData.includefile)) =  ?)
              )
               
       with width 350: 
           resourcedata.BusinessEntityModel = CreateBusinessEntityModel(input table  resourceData by-reference).
           CodeGenerator:Generate(rowid(resourceData), plDefs, plSource, plBe, table resourceData by-reference, true).
           disp resourceData.businessEntity format "x(32)"
                 resourceData.dataSource     format "x(32)"
                 resourceData.includefile    format "x(32)"
                 resourceData.codeTableKey   format ">>>9"
                 resourceData.codeTableName  format "x(40)"
                 search( subst("&1/&2.cls",right-trim(BusinessDir,"/"),resourceData.businessEntity)) <>  ? label "BE" 
                 search( subst("&1/&2.i",right-trim(DefinitionDir,"/"),resourceData.includefile)) <>  ? label "Include"
                 search( subst("&1/&2.cls",right-trim(DataSourceDir,"/"),resourceData.DataSource)) <>  ? label "DS"
                 .
            .
        end.     
    end method.
    
    method protected void FetchResources():
        
    end method.    
    
    method protected BusinessEntityModel CreateBusinessEntityModel (input table resourceData ):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    define variable oModel as BusinessEntityModel no-undo.    
    define variable oErrorTracker as ErrorTracker no-undo.
    if valid-object(resourceData.Error) then
        oErrorTracker = cast(resourceData.Error,ErrorTracker).
    else     
        oErrorTracker = new ErrorTracker().
    oModel = CodeConverter:CreateBusinessEntity(resourceData.businessEntity,
                                              cast(resourceData.SdoModel,SdoModel), 
                                              cast(resourceData.DlpModel,DlpModel), 
                                              resourceData.resourceName,
                                              resourceData.tempTableName, 
                                              resourceData.beforeTableName,
                                              resourceData.includefile,
                                              oErrorTracker).  
    if not valid-object(resourceData.Error) and oErrorTracker:HasMessage() then
    do: 
        undo, throw new ApplicationError(string(oErrorTracker:GetMessages(1))). 
       
    end. 
    return oModel.                                                  
end function.
     
end class.