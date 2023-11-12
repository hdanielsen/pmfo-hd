 
 /*------------------------------------------------------------------------
    File        : FileModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Feb 20 08:51:57 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.Set from propath.
using OpenEdge.Core.StringConstant from propath.
using OpenEdge.DataAdmin.DataAdminService from propath.
using OpenEdge.DataAdmin.IDataAdminService from propath.
using OpenEdge.DataAdmin.IField from propath.
using OpenEdge.DataAdmin.IFieldSet from propath.
using OpenEdge.DataAdmin.ITable from propath.
using OpenEdge.DataAdmin.ITableSet from propath.
using OpenEdge.DataAdmin.Lang.Collections.IIterator from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.Common.INameService from propath.
using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Tools.Appbuilder.ICodeTableModel from propath.
using Pmfo.Tools.Gui.Model.CancelError from propath.
using Pmfo.Tools.Gui.Model.DlpModel from propath.
using Pmfo.Tools.Gui.Model.FieldModel from propath.
using Pmfo.Tools.Gui.Model.IMonitor from propath.
using Pmfo.Tools.Gui.Model.IMonitorTarget from propath.
using Pmfo.Tools.Gui.Model.IResourceModel from propath.
using Pmfo.Tools.Gui.Model.Model from propath.
using Pmfo.Tools.Gui.Model.SdoModel from propath.
using Pmfo.Util.FileIO from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Constructor from propath.
using Pmfo.Tools.Gui.Model.AbstractResourceModel from propath.

class Pmfo.Tools.Gui.Model.ResourceModel inherits AbstractResourceModel implements IResourceModel: 
    {Pmfo/Util/filedata.i}
   
   
    define protected variable fileIo as FileIO no-undo.
  
    define public property MonitorSources   as character extent no-undo get. set.
    define protected property ServiceManager as IServiceManager no-undo 
        get():  
            return cast(Application:ServiceManager,IServiceManager).
        end.
    
   
    define stream filestream.
        
    constructor public ResourceModel ( ):
        define variable i as integer no-undo.
        super (input "File").
        fileio = new FileIO().
        extent(MonitorSources) = 5 + num-dbs.
        MonitorSources[1] = "Load SDOs and DLPs".
        do i = 1 to num-dbs:
            MonitorSources[1 + i] = subst("Load Tables from &1",ldbname(i)).
        end.
        MonitorSources[i + 1] = "Load Code Table Data".
        MonitorSources[i + 2] = "Load DataSources".
        MonitorSources[i + 3] = "Load Entities".
        MonitorSources[i + 4] = "Load Includes".
        
    end constructor.
    
    method override public void  FetchData().
        undo, throw new NotImplementedError("FetchData()").
    end.      
    
    method override public void FetchData(pcQuery as character).
        FetchData (pcQuery,?).
    end method.
    
    method override public void FetchData(pcQuery as character, pMonitor as IMonitor):
    
        define variable i as integer no-undo.
        define variable iFiles as integer no-undo.
        define variable service as IDataAdminService  no-undo.
        define variable rrowid as rowid no-undo.
        define variable oTable as ITable no-undo.
        define variable iMonitor as integer no-undo.
        define variable oMonitorTarget as IMonitorTarget no-undo.
     
        empty temp-table fileData.
        empty temp-table resourceData.
        if pcQuery > "" then
        do:
            iFiles = fileio:getFiles(pcQuery, table fileData).
            if valid-object(pMonitor) then
            do:
                pMonitor:Targets[1]:Total = ifiles.
            end.    
            for each fileData:
                if fileData.fileExt = "w" then
                do:
                    CreateSdoResource(fileData.fullPath).
                end.
                else if fileData.fileExt = "p" then
                do:
                     CreateDlpResource(fileData.fullPath).
                end.
                if valid-object(pMonitor) then
                   pMonitor:Targets[1]:Increment().
            
            end.
        end.
        do i = 1 to num-dbs:
            service = new DataAdminService(ldbname(i)).
            
            if valid-object(pMonitor) then
               oMonitorTarget = pMonitor:Targets[i + 1].
             
            AddTables(service,oMonitorTarget).
            iMonitor = i + 1. 
        end.
        if valid-object(CodeTableModel) then
        do:
            iMonitor = iMonitor + 1.
            if valid-object(pMonitor) then
               oMonitorTarget = pMonitor:Targets[iMonitor].
            CodeTableModel:AddCodes(this-object,oMonitorTarget,input-output table resourceData by-reference).
            MergeCodeTableSDOs(input-output table resourceData by-reference).
        end.
        iMonitor = iMonitor + 1.
        if valid-object(pMonitor) then
            oMonitorTarget = pMonitor:Targets[iMonitor].
        FetchManualCodeFromDir(ServiceManager:DataSourceDirectory,true,oMonitorTarget).
        
        iMonitor = iMonitor + 1.
        if valid-object(pMonitor) then
            oMonitorTarget = pMonitor:Targets[iMonitor].
        FetchManualCodeFromDir(ServiceManager:EntityDirectory,false,oMonitorTarget).
        iMonitor = iMonitor + 1.
        if valid-object(pMonitor) then
            oMonitorTarget = pMonitor:Targets[iMonitor].
        // this is supposed to run last     
        FetchManualIncludesFromDir(DefinitionDir,oMonitorTarget).
        catch e as CancelError:
           undo, throw e.    
        end catch.
    end.      
    
    method protected character GetFileName (pcFullpath as character):
        define variable cFilename as character no-undo.
        define variable iPos as integer no-undo.
        cFilename = entry(num-entries(pcFullPath,StringConstant:BACKSLASH),pcFullPath,StringConstant:BACKSLASH).
        ipos = r-index(cFilename,".").
        return substr(cFilename,1,ipos - 1).
    end.
    
    method protected void FetchManualCodeFromDir(pcdir as char, plSource as logical,pMonitorTarget as IMonitorTarget):
        empty temp-table fileData.
        define variable cfile as character no-undo.
        define variable iPos as integer no-undo.
        define variable iNumfiles as integer no-undo.
        release resourceData. 
        file-info:file-name = pcdir.
        if file-info:full-pathname > "" then
        do:
            iNumFiles = fileio:getFiles(file-info:full-pathname, table fileData).
            if valid-object(pMonitorTarget) then
                pMonitorTarget:Total = iNumfiles.
            for each fileData:
                if fileData.fileExt = "cls" then
                do:
                    cfile = (fileData.fullName).
                    ipos = r-index(cFile,".").
                    cFile = substr(cFile,1,ipos - 1).
                    
                    if plSource then
                    do:
                        if not can-find(first resourceData where resourcedata.dataSource = cfile) then
                        do:
                            AddManualResourceData(pcdir, fileData.fullName, plSource).
                        end.    
                    end.
                    else do: 
                        if not can-find(first resourceData where resourcedata.businessEntity = cfile) then
                            AddManualResourceData(pcdir, fileData.fullName, plSource).
                    
                    end.
                end.
                if valid-object(pMonitorTarget) then
                    pMonitorTarget:Increment().
            end.
        end.
    end.    
    
    method protected void FetchManualIncludesFromDir(pcdir as char,pMonitorTarget as IMonitorTarget):
        empty temp-table fileData.
        define variable cfile as character no-undo.
        define variable cPluralName as character no-undo.
        define variable iPos as integer no-undo.
        define variable iNumFiles as integer no-undo.
        release resourceData. 
        file-info:file-name = pcdir.
        if file-info:full-pathname > "" then
        do:
            iNumFiles = fileio:getFiles(file-info:full-pathname, table fileData).
            if valid-object(pMonitorTarget) then
                pMonitorTarget:Total = iNumfiles.
            for each fileData:
                if fileData.fileExt = "i" then
                do:
                    cfile = (fileData.fullName).
                    ipos = r-index(cFile,".").
                    cFile = substr(cFile,1,ipos - 1).
                    if not can-find(first resourceData where resourcedata.includefile = cfile) then
                    do:
                        cPluralName = NameService:PluralizeName(cfile).
                        find resourceData where resourceData.entityName = cPluralName and resourcedata.includefile = "" and resourcedata.NotFromData = true no-error.
                        if not avail resourceData then  
                            find resourceData where resourceData.resourcename = cPluralName and resourcedata.includefile = "" and resourcedata.NotFromData = true no-error.
                        if not avail resourceData then
                        do:
                            create resourceData.
                            assign
                                KeyCounter               = KeyCounter + 1
                                resourceData.resourceNum = KeyCounter
                                resourceData.NotFromData = true
                                resourceData.classname   = cfile
                                resourceData.onlyInclude = true // used to allow code generation even when notfromdata 
                                .
                             AfterRow(table resourcedata by-reference).   
                        end.
                        else      
                            resourceData.includefile = cFile.
                    end.
                end.
                if valid-object(pMonitorTarget) then
                    pMonitorTarget:Increment().
            end.
        end.
    end.    
    
    method protected void MergeCodeTableSDOs(input-output table resourceData ):
        define variable oSDO as SdoModel no-undo.
        define variable cKey as character no-undo.
        define variable iKey as integer no-undo.
        define buffer bResourceData for resourceData.
        for each resourceData where resourceData.sdoName   > "" 
                                and resourceData.tableName = CodeTableModel:CodeTableName:
            oSdo = cast(resourceData.SdoModel,SdoModel).
            case CodeTableModel:CodeTableKeyType:
                when "integer" then do:
                    iKey = CodeTableModel:ResolveIntegerKeyFromQuery(oSDO:BaseQuery).
                    if iKey <> ? then 
                        find bResourceData where bResourceData.codetablekey = iKey no-error.
                end.     
                when "character" then do:
                    cKey = CodeTableModel:ResolveCharacterKeyFromQuery(oSDO:BaseQuery).
                    if cKey <> ? then 
                        find bResourceData where bResourceData.codetablename = cKey no-error.
                end. 
                otherwise 
                    undo, throw new ApplicationError(subst("&1:CodeTableKeyType return invalid data '&2'. the property shouldreturn integer or character",CodeTableModel:getClass():TypeName,CodeTableModel:CodeTableKeyType)).    
            end.    
            if avail bResourceData then 
            do:
                 resourceData.className     = bResourceData.classname.
                 resourceData.CodeTableName = bResourceData.codeTablename.
                 resourceData.codeTableKey  = bResourceData.codeTableKey.
                 AfterRow(input table resourceData by-reference).
                 delete bResourceData.
            end.             
        end.     
        
    end.     
    
    method protected void AddTables(pservice as IDataAdminService, pMonitorTarget as IMonitorTarget):
         define variable oTables      as ITableSet no-undo.
         define variable oIter        as IIterator no-undo.
       
         define variable oTable       as ITable     no-undo.
         define variable cTable       as character  no-undo.
         define variable oDataFields  as ISet no-undo.
         
         oTables = pService:getTables().
         if valid-object(pMonitorTarget) then
            pMonitorTarget:Total = oTables:Count.
         oIter = oTables:Iterator().
         do while oIter:HasNext():
             oTable = cast(oIter:Next(),ITable).
             cTable = oTable:name.
             
             find resourceData where resourceData.tableName = cTable no-error .
             if avail resourceData then do:
                 resourceData.tempdb = pservice:Name = "tempdb".
                 resourceData.definedinDb = true.
                 oDataFields = GetDataFields(oTable).
                 AddPublicNameToFields(oDataFields,NameService).
                 
                 // merge table fields with sdo fields 
                 if valid-object(resourceData.sdoFields) then 
                 do:
                     cast(resourceData.sdoFields,ISet):AddAll(oDataFields).
                 end.
                 else // ?? 
                      resourcedata.dbFields = oDataFields. 
           
             end.
             else if ambiguous resourceData then do:
                 for each resourceData where resourceData.tableName = cTable:
                     resourceData.tempdb = pservice:Name = "tempdb" .
                     resourceData.definedinDb = true.
                 end.
             end.
             
             else do:    
                 create resourceData.
                 assign 
                     resourceData.tablename      = cTable
                     resourceData.className      = NameService:ClassName(resourceData.tableName)
                     resourceData.databaseName   = pservice:Name
                     resourceData.definedinDb    = true
                     .
                     
                  AfterRow(input table resourceData by-reference).  
                  oDataFields = GetDataFields(oTable).
                  resourcedata.dbFields = oDataFields. 
                  AddPublicNameToFields(oDataFields,NameService).
             end.     
             if valid-object(pMonitorTarget) then
                pMonitorTarget:Increment().
         end.
    end.      
    
    method public ISet GetDataFields(pTable as ITable):
        define variable oFieldIter   as IIterator no-undo.
       
        define variable ofield       as IField     no-undo.
        define variable odatafield   as FieldModel no-undo.
        define variable oDataFields  as ISet no-undo.
        define variable oFields      as IFieldSet no-undo.
         
        oFields     = pTable:Fields.
        oFieldIter  = oFields:Iterator().
        oDataFields = new Set().
        do while oFieldIter:HasNext():
            oField = cast(oFieldIter:Next(),IField).
            odatafield = new FieldModel().
            oDataField:name = oField:Name.
            oDataField:sourceTable = pTable:name.
            oDataField:dataType = oField:DataType.
            oDataFields:Add(oDataField).
        end.          
        return oDataFields.
    end method.     
    
    // public - currently used as util 
    method public void AddPublicNameToFields(pfields as ICollection,pNameService as INameService):
        define variable oIter  as OpenEdge.Core.Collections.IIterator no-undo.
        define variable oField as FieldModel no-undo.
        
        if valid-object(pFields) then 
        do:
            oIter = pFields:Iterator().
            do while oIter:HasNext():
                 oField = cast(oIter:Next(),FieldModel).
                 oField:serializeName = pNameService:PublicFieldName(oField:Name).
            end.
        end.    
    end method.    
    
    method protected void PostProcessRecords(table resourceData):
 
      
    end method.    
    
    method protected rowid CreateSdoResource(pcfilename as char):
        define variable oSdoModel  as SdoModel no-undo.
        define variable lNew       as logical no-undo.
        define buffer bResourceData for resourceData.
        oSdoModel = new SdoModel().
        
        if oSdoModel:Parse(pcFilename) then
        do:
            // check if there is resource created for the dlp 
            find resourceData where resourceData.dlpname = oSDoModel:DataLogicProcedure 
                              and   resourceData.sdoname = "" no-error.
            if not avail resourcedata then do: 
                                 
                create resourceData.
                assign 
                    resourceData.dlpname   = oSdoModel:DataLogicProcedure
                    lnew = true.
                /* in the odd case that the dlp is used by more than one SDO it may already 
                   have been assigned to an sdo resource - in that case use the same model
                   use find first as there could be more than one   
                */ 
                find first bresourceData where bresourceData.dlpname = oSDoModel:DataLogicProcedure 
                                           and valid-object(bresourceData.dlpModel) no-error.
                if avail bresourceData then
                    resourceData.dlpModel = bresourceData.dlpModel.    
            end.  
        
            assign  
                resourceData.SdoName        = oSdoModel:Name
                resourceData.tableName      = oSdoModel:Table
                resourceData.databaseName   = oSdoModel:DataBase
                resourceData.SdoModel       = oSdoModel
                resourceData.sdoFields      = oSdoModel:Fields 
                resourceData.className      = NameService:ClassName(resourceData.tableName)
                .
                 
            AfterRow(input table resourceData by-reference). 
            AddPublicNameToFields(oSdoModel:Fields,NameService).
            return rowid(resourceData).
        end.
        else do:
            delete object oSdoModel.
            return ?. 
        end. 
    end.
   
    method protected void AddManualResourceData(pcTypeDirectory as char, pFilename as char, plSource as logical):
        define variable cClass       as character no-undo.
        define variable cType        as character no-undo.
        define variable oType        as Progress.Lang.Class no-undo.
        define variable oParamList   as ParameterList no-undo.
        define variable oConstructor as Constructor no-undo. 
        define variable oObject      as Progress.Lang.Object no-undo.
        define variable cResource    as character no-undo.
        define variable cEntity      as character no-undo.
        define variable cFilename    as character no-undo.
        cClass = pfilename.
        entry(num-entries(cClass,"."),cClass,".") = "".
        cClass = right-trim(cClass,".").      
        cfilename = cClass.
        cClass = substitute("&1.&2",pcTypeDirectory,cClass).
        oType = Progress.Lang.Class:GetClass(cClass).
        if not oType:IsAbstract() then 
        do: 
             oParamList = new ParameterList(0).
             oConstructor = oType:GetConstructor(oParamList).
             if valid-object(oConstructor) then 
             do on error undo, leave: 
                 oObject = dynamic-new cClass( ).
                 if plSource then
                 do:
                     if type-of(oObject,IDataSource) then
                     do.
                         cEntity = cast(oObject,IDataSource):EntityName.
                          // link by resource and Entity if possible
                         find resourceData where resourcedata.resourceName = cEntity
                                           and resourceData.dataSource = "" 
                                           and resourceData.NotFromData = true no-error.
                     end.
                     else cEntity = "unknown".     
                 end.
                 else do:
                     if type-of(oObject,IEntity) then
                     do.
                        cResource = cast(oObject,IEntity):Resource.
                         // link by resource and Entity if possible
                        find resourceData where resourcedata.entityName = cResource 
                                            and resourcedata.businessEntity = "" 
                                            and resourceData.NotFromData = true no-error.
                     end.
                     else 
                         cResource = "unknown".
                 end.
                 if not avail resourceData then
                 do:
                    create resourceData.
                    assign
                        KeyCounter               = KeyCounter + 1
                        resourceData.resourceNum = KeyCounter.
                 end.   
                 if cEntity > "" then
                     resourceData.entityname = cEntity.
                 if cResource > "" then
                     resourceData.resourceName = cResource.
                      
                 catch e as Progress.Lang.Error :
                  //    undo, throw new ApplicationError(subst("Start of &1 failed with error - &2",cClass,e:GetMessage(1)),e).
                 end catch. 
            end.
            else do:
                create resourceData.
                assign
                    KeyCounter               = KeyCounter + 1
                    resourceData.resourceNum = KeyCounter
                    resourceData.hasParam    = true.
            end.
        end.
        else do:
            create resourceData.
            assign
                KeyCounter               = KeyCounter + 1
                resourceData.resourceNum = KeyCounter
                resourceData.isAbstract  = true.
        end. 
        resourceData.NotFromData = true.
        if plSource then 
            resourcedata.dataSource = cfilename.
        else    
           resourcedata.businessEntity = cFilename.
    end method.    
    
    
    method protected void CreateDlpResource(pcfilename as char):
        define variable oDlpModel  as DlpModel  no-undo.
        define variable lFound as logical no-undo.
        oDlpModel = new DlpModel ().
        if oDlpModel:Parse(pcFilename) then
        do:
            for each resourceData where resourceData.dlpname = oDlpModel:Name:
                resourceData.DlpModel  = oDlpModel.
                lFound = true.
            end.     
            if not lFound then 
            do:
                create resourceData.
                assign 
                    resourceData.dlpName     = oDlpModel:Name
                    resourceData.DlpModel    = oDlpModel 
                    KeyCounter               = KeyCounter + 1
                    resourceData.resourceNum = KeyCounter 
                    . 
            end.
        end.
        else            
            delete object odlpModel.
    end.
    
    method override public void  FetchData(pcTables as char extent,pcQueries as character extent).
    end.      
     
end class.