 
 /*------------------------------------------------------------------------
    File        : CodeGenerator
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Mar 02 21:58:46 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.StringConstant from propath.
using OpenEdge.DataAdmin.DataAdminService from propath.
using OpenEdge.DataAdmin.Error.IllegalOperationError from propath.
using OpenEdge.DataAdmin.IDataAdminService from propath.
using OpenEdge.DataAdmin.IIndex from propath.
using OpenEdge.DataAdmin.IIndexField from propath.
using OpenEdge.DataAdmin.IIndexSet from propath.
using OpenEdge.DataAdmin.ITable from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.Common.DataService from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Repository.Business.ResourceBE from propath.
using Pmfo.Tools.Gui.Model.BusinessEntityModel from propath.
using Pmfo.Tools.Gui.Model.ClassModel from propath.
using Pmfo.Tools.Gui.Model.DataSourceModel from propath.
using Pmfo.Tools.Gui.Model.FieldModel from propath.
 
using Pmfo.Tools.Gui.Model.SdoModel from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Tools.Gui.Model.MethodModel from propath.
using Pmfo.Tools.Gui.Model.ParameterModel from propath.
using OpenEdge.Core.DataTypeEnum from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Tools.Appbuilder.ICodeTableModel from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Tools.Gui.Model.ISdoModel from propath.

class Pmfo.Tools.AppBuilder.CodeGenerator inherits DataService: 
    {Pmfo/Tools/AppBuilder/resourcedata.i }
    
    {Pmfo/Repository/Business/defs/resources-dataset.i} 
    
    define stream definitions.
    define stream sources.
    define stream business.
    define stream inputstream.
    
    define temp-table ttfield 
         field name   as character
         field object as Progress.Lang.Object
         index name   as primary unique name. 
    
    define public property CodeTableModel as ICodeTableModel no-undo get. set.
    
    define public property TempDbName as char no-undo 
        get():
            define variable i as integer no-undo.
            if TempDbName = "" then 
            do i = 1 to num-dbs:
                if ldbname(i) begins "temp" 
                and ((ldbname(i) matches "*db*")
                     or 
                     (ldbname(i) matches "*table*")) then
                do:
                    TempDbName = ldbname(i).
                end.    
            end.    
            return TempDbName.
        end. 
        set.
  
/*    define public Property NameService as INameService no-undo*/
/*        get():                                                */
/*            if not valid-object(NameService) then             */
/*                NameService = new NameService().              */
/*            return NameService.                               */
/*        end get.                                              */
/*        set.                                                  */
        
    define protected property ServiceManager as IServiceManager no-undo 
        get():
            if not valid-object(ServiceManager) then 
            do:
                // NOTE-  fullpath - does not resolve from using when referenced  from /work in other workspace!
                //                     somehow the cls is picked up - works if only r-code 
                //                     related to 12.7  pds problems with cache of code (seems unlikely, but) ?? !   
                 
                Servicemanager = cast(Ccs.Common.Application:ServiceManager,IServiceManager). 
            end.
            return ServiceManager.
        end get. 
        set.
        
    define protected variable Services as IDataAdminService extent no-undo.
    
    define public property ApplicationPath          as char no-undo  
        get. 
        set(ppath as char):
            ApplicationPath = replace(pPath,"~\","/").
        end.
    
    define public property DataLayerPath          as char no-undo  
        get. 
        set(ppath as char):
            DataLayerPath = replace(pPath,"~\","/").
        end.
        
    define public property DefinitionDir      as char no-undo init "schema" get. set. 
    
    define public property DataSourceDir      as char no-undo 
        get():
            return ServiceManager:DataSourceDirectory.
        end. 
         
    define public property BusinessDir        as char no-undo 
        get():
            return ServiceManager:EntityDirectory.
        end.  
        
    define public property HeaderTemplate     as char no-undo init "Pmfo/Tools/template/HeaderComment.txt" get. set. 
    
    define public property MethodBindTemplate as char no-undo init "Pmfo/Tools/template/MethodBind.txt" get. set. 
    define public property MethodPrepareBufferTemplate as char no-undo 
                                                              init "Pmfo/Tools/template/MethodPrepareBuffer.txt" get. set. 
    define public property MethodPrepareSaveBufferTemplate as char no-undo 
                                                              init "Pmfo/Tools/template/MethodPrepareSaveBuffer.txt" get. set. 
    
    define public property OverwriteDefinitions as logical no-undo get. set. 
    define public property OverwriteDataSources as logical no-undo get. set. 
    define public property OverwriteBusinessEntities as logical no-undo get. set. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public CodeGenerator (  ):
        super ().
    end constructor.
    
    method override public void Destroy(  ):
        
    end method.

    method protected IDataAdminService GetService(pcdbname as char):
        define variable i as integer no-undo.
        if extent(Services) = ? then
           extent(Services) = num-dbs.
        do i = 1 to num-dbs:
            if ldbname(i) = pcdbname then
            do:
               if not valid-object(Services[i]) then 
                   Services[i] = new DataAdminService(pcdbname).
                return Services[i].                   
            end.    
        end.  
        return ?.     
    end.    
    
    method protected void GenerateResource( pdefs as logical, psource as logical, pbe as logical,input table resourceData):
        define variable cDefinitionRelativeFile as character no-undo.
        cDefinitionRelativefile = DefinitionDir + "/" + lc(substr(resourceData.className,1,1)) + substr(resourceData.className,2) + ".i". 
        if pDefs then
        do: 
            if resourceData.onlyInclude = true then
                undo, throw new IllegalOperationError(subst("Cannot generate include '&1' since there is an existing include that likely has been created manually and for which the fields are not known",cDefinitionRelativefile)).
            GenerateDefinition( input table resourceData by-reference).
        end.    
        if psource then 
            GenerateDataSource(cDefinitionRelativefile, input table resourceData by-reference).
        if pbe then 
        do:
            GenerateBusinessEntity(input table resourceData by-reference).
            StoreBusinessEntity(input table resourceData by-reference).
        end.    
        StoreDataSource(input table resourceData by-reference).   
    end method.
     
    method protected void GenerateDefinition(input table resourceData):
        define variable oFields         as ICollection no-undo.
        define variable oIndexes        as IIndexSet no-undo.
        define variable cTableName      as character no-undo.
        define variable cIndexAsStrings as character extent no-undo.
        define variable services        as IDataAdminService extent no-undo.
        define variable cDbName         as character no-undo.
        define variable oTable          as ITable no-undo.
        define variable dbIndex         as integer no-undo.
        define variable cInfo           as character no-undo.
        define variable cSdoShortName   as character no-undo.
        define variable i               as integer no-undo.
        define variable oService        as IDataAdminService no-undo.
         
        // TODO add some logic - this is assuming class name ends with DS
           // todo add property for deffields with changed names or serializenames 
        if resourcedata.sdoname > "" then 
        do on error undo, throw:
            oFields = cast(resourcedata.sdofields,ICollection).
            cTableName = resourcedata.tableName.
            cDbName = resourceData.databaseName.
            cSdoShortname = replace(resourceData.sdoName,"~\","/").
            cSdoShortname = entry(num-entries(cSdoShortname,"/"),cSdoShortname,"/").
            /* dbname temp-tables is used in field defs which is where we got it from  */
            if cDbName = "Temp-tables" then
            do: 
               cDbName = TempDbName. 
               cinfo = subst("Fields copied from temp-table in SDO &1",cSdoShortname).
               oTable = GetService(cDbName):GetTable(cTableName).
               if valid-object(oTable) then
                   oIndexes = oTable:indexes.
               else do:
                  extent(cIndexasStrings) = 1. 
                  cindexAsStrings[1] = "/* index defintions not copied from SDO  */". 
               end.           
            end.
            else do:
                oService = GetService(cDbName).
                if not valid-object(oService) then
                do:
                     undo, throw new IllegalOperationError(subst("Database &1 used in SDO &2 is not connected",cDbName,cSdoShortname)).
                end.
                cinfo = subst("Mapped to database &1 table &2. Fields copied from SDO &1",cDbName,cTableName,cSdoShortname).
                oTable = oservice:GetTable(cTableName).
                oIndexes = oTable:indexes.
            end.
        end.
        else if resourceData.tableName > "" then
        do:
            cTableName = resourcedata.tableName.
            
            cDbName = resourceData.databaseName.
            cinfo = subst("Mapped to database &1 table &2",cDbName,cTableName). 
            oTable = GetService(cDbName):GetTable(cTableName).
            oIndexes = oTable:indexes.
            oFields = cast(resourceData.dbFields,ICollection).
        end.
        else if resourceData.codeTableName > "" then 
        do: 
            if not valid-object(resourcedata.codeTableFields) then
                resourcedata.codeTableFields = codeTableModel:GetCodeTableFields(resourceData.codeTableName).
             
            if valid-object(resourcedata.codeTableFields) then     
                ofields = cast(resourceData.codeTableFields,ICollection).
            cindexAsStrings = CodeTableModel:IndexAsString.
            cinfo = codeTableModel:GetCodeTableNotes(resourceData.codeTableName).
               
        end. 
        
        if valid-object(oFields) then       
            GenerateDefinition(resourceData.includefile,resourceData.tempTableName,resourcedata.entityName,resourceData.beforeTableName,oFields,oIndexes,cindexAsStrings,cInfo).
       
        finally:
            do i = 1 to extent(Services):
                if valid-object(Services[i]) then
                    delete object Services[i].
            end.    
        end.    
    end method.
    
    method public void Generate(pcQuery as char, pdefs as logical, psource as logical, pbe as logical, pcDir as char, input table resourceData, pNoRepository as logical):
        define variable hQuery       as handle no-undo. 
        define variable hTable       as handle no-undo.
        define variable cfile        as character no-undo.
        define variable oResourceBE  as ResourceBE no-undo.
         
        if not pNoRepository then 
        do: 
            oResourceBE = cast(ServiceManager:CreateBusinessEntity("resources"),ResourceBE).
            oResourceBE:GetData(output dataset dsResources by-reference).
            DatasetUtil:SetTrackingChanges(dataset dsResources:handle, true).
        end.
        else do:
            pSource = false.
            pBe = false.
            pdefs = true. 
        end.
        create query hQuery.
        hQuery:add-buffer(buffer Resourcedata:handle).
        hQuery:query-prepare(pcQuery).
        hQuery:query-open().
        hQuery:get-first.
        
        
        do while avail Resourcedata:
            GenerateResource(pdefs,psource,pbe,input table resourceData by-reference). 
            hQuery:get-next.
        end.
        if not pNoRepository then
        do:
            oResourceBE:UpdateData(input-output dataset dsResources by-reference).
            DatasetUtil:SetTrackingChanges(dataset dsResources:handle, false).
        end.
        DatasetUtil:SetTrackingChanges(dataset dsResources:handle, false).
            
    end method.
    
    method public void Generate(prowid as rowid, pdefs as logical, psource as logical, pbe as logical, input table resourceData, pNoRepository as logical):
        define variable oResourceBE  as ResourceBE no-undo.
        define variable lContinue    as logical   no-undo.
        
        if not pNoRepository then         
        do: 
            oResourceBE = new ResourceBE().
            oResourceBE:GetData(output dataset dsResources by-reference).
            DatasetUtil:SetTrackingChanges(dataset dsResources:handle, true).
        end.
/*        else do:            */
/*            pSource = false.*/
/*            pBe = false.    */
/*            pdefs = true.   */
/*        end.                */
/*                            */
        find resourcedata where rowid(resourcedata) = prowid no-error.
        GenerateResource(pdefs,psource,pbe,input table resourceData by-reference).  
        
        if not pNoRepository then
        do on error undo, throw:   
            oResourceBE:UpdateData(input-output dataset dsResources by-reference).
            DatasetUtil:SetTrackingChanges(dataset dsResources:handle, false).
            catch e as Progress.Lang.Error :
                message e:GetMessage(1) skip
                e:CallStack
                view-as alert-box. 
                undo, throw e. 
            end catch. 
        end.
    end method.
    
    method protected character GetApplicationFullPath(pcdir  as char, pcFile as char,pcExt as char):
        define variable cFullName as character no-undo.
        cFullName = pcdir + "/" + pcFile + "." + pcExt.
        if ApplicationPath > "" then
            cFullname = right-trim(ApplicationPath,"/") + "/" + cFullName. 
        return cFullName.     
    end method.    
    
    method protected character GetDataSourceFullPath(pcdir  as char, pcFile as char,pcExt as char):
        define variable cFullName as character no-undo.
        cFullName = pcdir + "/" + pcFile + "." + pcExt.
        if DataLayerPath > "" then
            cFullname = right-trim(DataLayerPath,"/") + "/" + cFullName. 
        return cFullName.     
    end method.   
    
    method public character GetRelativeIncludeName (pcIncludeName as char ):
        return DefinitionDir + "/" + pcIncludeName + ".i". 
    end method.    
    
    method protected longchar ConvertTemplate(pcTemplate as char):
        define variable cLine as character no-undo.
        define variable cCode as longchar no-undo.
     
        file-info:file-name = pcTemplate.
        
        input stream inputstream from value(file-info:full-pathname).
        repeat:
            cLine = "".
            import stream inputstream unformatted cLine.
            cCode = cCode + cLine + StringConstant:LF.    
        end.  
        return ccode.
        finally:
            input stream inputstream close.
        end.
    end method.
 
    method override public void Initialize(  ):
        
    end method.

    method protected longchar SerializeCodeFromTemplate(phStream as handle,pcTemplate as char, pcParam as longchar extent):
        define variable cLine as character no-undo.
         
        file-info:file-name = pcTemplate.
        
        input stream inputstream from value(file-info:full-pathname).
        repeat:
            cLine = "".
            import stream inputstream unformatted cLine.
            case extent(pcParam):
                when 1 then
                    put stream-handle phStream unformatted subst(cLine,pcParam[1]) skip.
                when 2 then
                    put stream-handle phStream unformatted subst(cLine,pcParam[1],pcParam[2]) + StringConstant:LF.
                when 3 then
                    put stream-handle phStream unformatted subst(cLine,pcParam[1],pcParam[2],pcParam[3]) + StringConstant:LF.
                when 4 then
                    put stream-handle phStream unformatted subst(cLine,pcParam[1],pcParam[2],pcParam[3],pcParam[4]) + StringConstant:LF.
                when 5 then
                    put stream-handle phStream unformatted subst(cLine,pcParam[1],pcParam[2],pcParam[3],pcParam[4],pcParam[5]) + StringConstant:LF.
                when 6 then
                    put stream-handle phStream unformatted subst(cLine,pcParam[1],pcParam[2],pcParam[3],pcParam[4],pcParam[5],pcParam[6]) + StringConstant:LF.
                when 7 then
                    put stream-handle phStream unformatted subst(cLine,pcParam[1],pcParam[2],pcParam[3],pcParam[4],pcParam[5],pcParam[6],pcParam[7]) + StringConstant:LF.
            end.    
        end.  
        finally:
            input stream inputstream close.
        end.
    end method.
    
    
    method protected longchar ConvertTemplate(pcTemplate as char, pcParam as longchar extent):
        define variable cLine as character no-undo.
        define variable cCode as longchar no-undo.
        
        file-info:file-name = pcTemplate.
        
        input stream inputstream from value(file-info:full-pathname).
        repeat:
            cLine = "".
            import stream inputstream unformatted cLine.
            case extent(pcParam):
                when 1 then
                    cCode = cCode + subst(cLine,pcParam[1]) + StringConstant:LF.
                when 2 then
                    cCode = cCode + subst(cLine,pcParam[1],pcParam[2]) + StringConstant:LF.
                when 3 then
                    cCode = cCode + subst(cLine,pcParam[1],pcParam[2],pcParam[3]) + StringConstant:LF.
                when 4 then
                    cCode = cCode + subst(cLine,pcParam[1],pcParam[2],pcParam[3],pcParam[4]) + StringConstant:LF.
                when 5 then
                    cCode = cCode + subst(cLine,pcParam[1],pcParam[2],pcParam[3],pcParam[4],pcParam[5]) + StringConstant:LF.
                when 6 then
                    cCode = cCode + subst(cLine,pcParam[1],pcParam[2],pcParam[3],pcParam[4],pcParam[5],pcParam[6]) + StringConstant:LF.
                when 7 then
                    cCode = cCode + subst(cLine,pcParam[1],pcParam[2],pcParam[3],pcParam[4],pcParam[5],pcParam[6],pcParam[7]) + StringConstant:LF.
            end.    
        end.  
        return ccode.
        finally:
            input stream inputstream close.
        end.
    end method.
    
    // hook for subclass
    method protected void BeforeFieldGenerate(pField as FieldModel):
        
    end method.    
            
    method public void GenerateDefinition(pcIncludeFile as char,pcTempTableName as char,pcSerializename as char,pcBeforeTablename as char, pFields as ICollection, pIndexes as IIndexSet, pIndexAsStrings as char extent, pcInfo as char):
        define variable oIndexIterator as OpenEdge.DataAdmin.Lang.Collections.IIterator no-undo.
        define variable oIndexFieldIterator as OpenEdge.DataAdmin.Lang.Collections.IIterator no-undo.
        define variable oIterator       as IIterator no-undo.
        define variable oField          as FieldModel  no-undo.
        define variable oIndex          as IIndex  no-undo.
        define variable oIndexField     as IIndexField no-undo.
        define variable cFullName       as character no-undo.
        define variable cBeforeTableDEf as character no-undo.
        define variable lNoIndex        as logical no-undo.
        define variable cfieldlist      as character no-undo.
        define variable cIndexdef       as character no-undo.
        define variable cComment        as character no-undo.
        define variable cRelativefile   as character no-undo.
        define variable cSeqName        as character no-undo.
        define variable cSeqType        as character no-undo init "int64".
        define variable iIndex          as integer   no-undo.
        cSeqName = BufferUtil:SeqFieldName.
        cRelativefile = GetRelativeIncludeName(pcIncludeFile).
        if ApplicationPath > "" then
            cFullname = right-trim(ApplicationPath,"/") + "/" + cRelativeFile.
        else
            cFullname = cRelativeFile.
         
        if valid-object(pIndexes) and pIndexes:count = 1 then 
        do:
            if pIndexes:CanFind("default") then lNoIndex = true. 
        end.  
        if not lNoIndex and pcBeforeTablename > "" then   
            cBeforeTabledef = " before-table " + pcBeforeTablename.
         
        output stream definitions to value(cFullname).
        GenerateHeaderComments(stream definitions:handle,cRelativeFile,pcInfo).
        put stream definitions unformatted
           subst("define temp-table &1 no-undo serialize-name ~"&2~" ~{1~} &3",pcTempTablename,pcSerializename,cBeforeTableDef)   skip.
        oIterator = pFields:Iterator().
        empty temp-table ttfield.
        do while oIterator:hasNext():
            oField = cast(oIterator:Next(),FieldModel).
            create ttField.
            ttField.name = oField:name.
            ttField.obj  = oField.
        end.    
        for each ttField by ttField.name:
            oField = cast(ttField.obj,FieldModel).
            BeforeFieldGenerate(oField).
            if oField:IsExcluded = false then 
            do:
                put stream definitions unformatted
                    subst("field &1 as &2 &3serialize-name ~"&4~"",
                           ofield:name + fill(" ",32 - length(oField:name)),
                           lc(ofield:dataType) + fill(" ",11 - length(oField:dataType)),
                           if ofield:serializeHidden then "serialize-hidden " else "",
                           oField:serializeName) 
                    at 4 skip.
                cFieldList = cfieldList
                            + (if cFieldList = "" then "" else ",")
                            + oField:name.
            end.            
        end.
        // argh 
        put stream definitions unformatted  
            subst("field &1 as &2 serialize-hidden",cSeqName + fill(" ",32 - length(cSeqName)),cSeqType + fill(" ",11 - length(cSeqType)))  at 4 skip .
        
        if extent(pIndexAsStrings) > 0 then
        do iIndex = 1 to extent(pIndexAsStrings): 
            put stream definitions unformatted pIndexAsStrings[iIndex] at 4.
        end.
        else if lnoIndex then  
            put stream definitions unformatted "/* No index defined in source table */" at 4.
            
        else if valid-object(pIndexes) then     
        do:
            oIndexIterator = pIndexes:Iterator().                                                                
            do while oIndexIterator:hasNext():
                assign
                    cComment = ""
                    cIndexDef = "".
                oIndex = cast(oIndexIterator:Next(),IIndex).
                if oIndex:isWordIndex = false then do:
                    cIndexdef = subst("index &1&2&3&4&5",
                                      oIndex:name,
                                      if /*oindex:isPrimary or*/ oindex:isUnique then " as " else "",
                                //   need seq as primary
                                //    if oindex:isPrimary then "primary" else "",
                                  //    if /*oindex:isPrimary and*/ oindex:isUnique then " " else "",
                                      if oindex:isUnique then "unique" else ""
                                ).
                    oIndexFieldIterator = oIndex:IndexFields:Iterator().
                    do while oIndexFieldIterator:HasNext():
                         oIndexField = cast(oindexFieldIterator:Next(),IIndexField).
                         if lookup(oIndexField:Field:Name,cfieldlist) = 0 then 
                            cComment = cComment + "   Field " +  oIndexField:Field:Name  + " not defined in temp-table. "  .
                         cIndexdef = cIndexdef + " " + oIndexField:Field:Name.
                    end.
                    if cComment > "" then
                       put stream definitions unformatted "/*-----------------" at 4 skip  cComment .  
                    put stream definitions unformatted skip cIndexdef at 4 skip.
                    if cComment > "" then
                       put stream definitions unformatted "-------------------*/" at 4  skip .  
                    
                end.
            end.
        end.
        put stream definitions unformatted subst("index &1 as primary &1",cSeqName) at 4 skip.
        put stream definitions  "." at 4.
        finally:
            output stream definitions close.
        end.
    end method.    
    
    method protected void WriteCode(phStream as handle, pCode as longchar):
       define variable i as integer no-undo.
       define variable lLastBlank as logical no-undo.
       define variable cLine as character no-undo.
       do i = 1 to num-entries(pCode,StringConstant:LF).
           cLine = string(entry(i,pCode,StringConstant:LF)).
           //if cLine > "" and lLastBlank = false then
               put stream-handle phStream unformatted cLine at 5 skip .
          // lLastBlank = cLine = "".    
       end. 
    end method.
    
    // public to be used by anyone
    method protected void GenerateHeaderComments(phStream as handle, pfilename as char, pcNote as char):
        GenerateHeaderComments(phStream, pfilename, pcNote,this-object:GetClass():TypeName).
    end method.   
    
     // public to be used by anyone
    method public void GenerateHeaderComments(phStream as handle, pfilename as char, pcNote as char, pGenerator as char):
        define variable cLine as character no-undo.
        file-info:file-name = HeaderTemplate.
        
        input stream inputstream from value(file-info:full-pathname).
        repeat:
            import stream inputstream unformatted cLine.
            put stream-handle phStream unformatted subst(cLine,pfilename,pGenerator,now,pcNote) skip.    
        end.  
        finally:
            input stream inputstream close.
        end.    
    end method.   
    
    
    method protected character GetDatatypeString(pDataType as DataTypeEnum):
        case pDataType:
            when DataTypeEnum:Dataset then
                return lc(pDataType:ToString() + "-handle").
            otherwise
                return lc(pDataType:ToString()). 
        end.    
    end method.    
    
    method protected character GetMethodParameters (pMethod as MethodModel):
        define variable oIter   as IIterator no-undo.
        define variable oParam  as ParameterModel no-undo.
        define variable cString as character no-undo.
        define variable cSpace  as character no-undo.
        define variable cComma as character no-undo.
        oIter = pMethod:Parameters:Iterator().
        
        do while oIter:HasNext():
           oParam = cast(oIter:Next(),ParameterModel).
           cString = cString 
                   + cSpace
                   + oParam:ToString()
                   + ",".
           cSpace = StringConstant:SPACE.
        end.    
        return right-trim(cString,",").
    end method.
    
    method protected void GenerateDataSource(pcRelativeInclude as char,input table resourceData):
        define variable cFullName            as character no-undo.
        define variable cRelativeName        as character no-undo.
        define variable oDataSourceModel     as DataSourceModel no-undo.
        define variable i                    as integer no-undo.
        define variable oIter                as IIterator no-undo.
        define variable oMethod              as MethodModel no-undo.
        define variable oFields              as ICollection no-undo.
        define variable lAddAfterRowComments as logical no-undo.
        define variable oFieldIterator       as IIterator no-undo.
        define variable oField               as FieldModel no-undo.
        define variable lAnyCalc             as logical no-undo.
        oDataSourceModel = cast(resourceData.DataSourceModel,DataSourceModel).         
        cFullName = GetDataSourceFullPath(DataSourceDir, resourceData.dataSource,"cls").
        cRelativeName = replace(DataSourceDir,"/",".") + "." + resourceData.dataSource.
        
     
        output stream sources to value(cFullname).
        GenerateHeaderComments(stream sources:handle,cRelativeName,"").
        
        put stream sources skip(1).
        
        if resourceData.codeTableName > "" then
        do:
             put stream sources unformatted "block-level on error undo, throw." skip.
             put stream sources unformatted subst("using &1 from propath.",ServiceManager:CodeTableSourceName) skip.      
        end.
        else
            GenerateDeclarations(stream sources:handle,oDataSourceModel).
        put stream sources skip(1).
        
        put stream sources unformatted subst("class &1 inherits &2:",
                                             cRelativename,
                                             if resourceData.codeTableName > "" 
                                             then entry(num-entries(ServiceManager:CodeTableSourceName,"."),ServiceManager:CodeTableSourceName,".") 
                                             else oDataSourceModel:InheritsString) skip.
        put stream sources skip(1).
       
        GenerateIncludeRefs(stream sources:handle,oDataSourceModel,true).
        put stream sources skip(1).                                 
        
        if resourceData.codeTableName > "" then
        do:
            CodeTableModel:GenerateDataSourceConstructor(stream sources:handle,input table resourceData by-reference).
        end.
        
        else do:
            put stream sources unformatted subst('define public override property EntityName as character init "&1" no-undo get.',resourceData.entityName) at 5 skip.
            put stream sources skip(1).                                 
            put stream sources unformatted subst("constructor &1():",resourceData.dataSource)           at 5 skip.
            if resourceData.tableName = "" then 
               put stream sources unformatted "super()." at 9 skip.
            else 
               put stream sources unformatted subst("super(buffer &1:handle).",resourceData.tableName) at 9 skip.
               
            put stream sources unformatted "end constructor."                                               at 5 skip.
        end.
        put stream sources skip(1). 
        
        put stream sources unformatted subst("method protected void Bind( input table &1 bind):",resourceData.tempTableName)           at 5 skip.
        put stream sources unformatted "end method."                                               at 5 skip.
        put stream sources skip(1). 
        
        put stream sources unformatted string(ConvertTemplate(MethodPrepareBufferTemplate)). 
        
        put stream sources unformatted string(ConvertTemplate(MethodPrepareSaveBufferTemplate)). 
        
     
        
        oIter = oDataSourceModel:Methods:Iterator().
        
        if resourcedata.sdoname > "" then 
            oFields = cast(resourcedata.sdofields,ICollection).
        else if resourceData.tableName > "" then
            oFields = cast(resourceData.dbFields,ICollection).
        else if resourceData.codeTableName > "" then
        do:
            if not valid-object(resourcedata.codeTableFields) then
                resourcedata.codeTableFields = codeTableModel:GetCodeTableFields(resourceData.codeTableName).
             
            if valid-object(resourcedata.codeTableFields) then     
                ofields = cast(resourceData.codeTableFields,ICollection).
        end.
        
        methodloop: 
        do while oIter:HasNext():
            oMethod = cast(oIter:Next(),MethodModel).
            lAnyCalc = false.
            if oMethod:name = "AfterRow" and valid-object(oFields) then 
            do:
                oFieldIterator = oFields:Iterator().
                do while oFieldIterator:hasNext():
                    oField = cast(oFieldIterator:Next(),FieldModel).
                    if oField:isCalc and oField:IsExcluded = false then 
                    do: 
                        lAnyCalc = true.
                        leave.
                    end.          
                end.
                if lAnyCalc = false then
                    next methodloop.
            end.        
            put stream sources unformatted subst("method &1 override &2 &3(&4):",
                                                 // TODO move this to ParameterModel
                                                 if oMethod:name = "AfterRow" then "public" else "protected",
                                                 if oMethod:DataType = ? then "void"
                                                 else if oMethod:DataType:ToString() matches "*array" then lc(OpenEdge.Core.DataTypeHelper:GetMask(oMethod:DataType))  
                                                      else lc(oMethod:DataType:ToString()),
                                                 oMethod:name,
                                                 GetMethodParameters(oMethod)
                                                 )
                at 5 skip.
                
            // TODO move to CodeConverter
            lAddAfterRowComments = false.
            if oMethod:name = "AfterRow" and index(oMethod:code,"super:AfterRow") = 0 then
            do:
                put stream sources unformatted "super:AfterRow(input dataset-handle phDataSet by-reference)."           at 9 skip.
                put stream sources unformatted "/**** calculated fields in SDO "           at 9 skip.
                lAddAfterRowComments = true. //
            end.                                             
            WriteCode(stream sources:handle, oMethod:Code).                                     
            
            if lAddAfterRowComments //oMethod:name = "AfterRow"
             then
            do:
                put stream sources unformatted "****/ "           at 9 skip.
            end.
            put stream sources unformatted "end method."                                      at 5 skip.
            put stream sources skip(1).     
        end.
           
        put stream sources unformatted "end class." skip.  
        
        catch e as Progress.Lang.Error :
            
              message e:GetMessage(1)
              view-as alert-box.  
        end catch.
        
        finally:
            output stream sources close.
        end.    
          
    end method.
    
    method protected void GenerateBusinessEntity(input table resourceData):
        define variable cFullName            as character no-undo.
        define variable cRelativeName        as character no-undo.
        define variable oIter                as IIterator no-undo.
        define variable oBusinessEntityModel as BusinessEntityModel no-undo.
        define variable i                    as integer no-undo.
        define variable oMethod              as MethodModel no-undo.
        
        oBusinessEntityModel = cast(resourceData.BusinessEntityModel,BusinessEntityModel).         
        cFullName = GetApplicationFullPath(BusinessDir, resourceData.businessEntity,"cls").
        cRelativeName = replace(BusinessDir,"/",".") + "." + resourceData.businessEntity.
        output stream business to value(cFullname).
         
        GenerateHeaderComments(stream business:handle,cRelativeName,"").
        put stream business skip(1).
        
        GenerateDeclarations(stream business:handle,oBusinessEntityModel).
        put stream business skip(1).
        
        put stream business unformatted subst("class &1 inherits &2:",cRelativename,oBusinessEntityModel:InheritsString) skip.
        put stream business skip(1).
        if valid-object(oBusinessEntityModel) then
            GenerateIncludeRefs(stream business:handle,oBusinessEntityModel,false).
        else     
            GenerateIncludeRefs(stream business:handle,resourcedata.includefile,false).
        put stream business skip(1).
        
        put stream business unformatted subst("define dataset ds&1 for &2.",resourceData.className,resourceData.tempTableName) at 5 skip.
        put stream business skip(1).
        
        put stream business unformatted subst("constructor &1():",resourceData.businessEntity)           at 5 skip.
        put stream business unformatted subst("super(dataset ds&1:handle).",resourceData.classname) at 9 skip.
        put stream business unformatted "end constructor."                                               at 5 skip.
        put stream business skip(1).
        oIter = oBusinessEntityModel:Methods:Iterator().
        
        do while oIter:HasNext():
            oMethod = cast(oIter:Next(),MethodModel).
            put stream business unformatted subst("method &1 void &2(&3):",
                                                 // TODO move this to ParameterModel
                                                 "public" ,
                                                 oMethod:name,
                                                 GetMethodParameters(oMethod)
                                                 )                                            at 5 skip.
            WriteCode(stream business:handle, oMethod:Code).                                     
            put stream business unformatted "end method."                                      at 5 skip.
            put stream business skip(1).     
        end.
        
        put stream business unformatted "end class." skip.
        finally:
            output stream business close.
        end.    
    end method.    
    
    method protected void StoreBusinessEntity(table resourceData):
        find ttEntity where ttEntity.Resource = resourcedata.resourceName no-error.
        
        if not avail ttEntity then do:
            create ttEntity.
            assign 
                ttEntity.Resource    = resourcedata.resourceName
                // it is possible to serialize as object, but error prone  
                ttEntity.ServerTypes = ServerTypeEnum:Default:ToString()
                ttEntity.CanCreate   = true
                ttEntity.CanDelete   = true
                ttEntity.ReadOnly    = false
                ttEntity.Public      = true
                ttEntity.TypeName    = get-class(IBusinessEntity):TypeName
                .
              
        end.
        ttEntity.ClassName = right-trim(replace(replace(BusinessDir,StringConstant:BACKSLASH,"/"),"/","."),".") + "." + resourceData.businessEntity.
         
    end method.     
    
    method protected void StoreDataSource(table resourceData):
        define variable cSourceName as character no-undo.
        define variable oSDo as ISdoModel no-undo.
        define variable ofields as ICollection no-undo.
        define variable oIter as IIterator no-undo.
        define variable ofield as FieldModel no-undo.
        define variable cAssignList as character no-undo.
        define variable cFullName as character no-undo.
        
        find ttDataSource where ttDataSource.EntityName = resourcedata.entityName no-error.
        
        assign
            cFullName = GetDataSourceFullPath(DataSourceDir, resourceData.dataSource,"cls").
        
        if not avail ttDataSource then 
        do:
            create ttDataSource.
            assign ttDataSource.EntityName  = resourcedata.entityName.
        end.
        else do:
            assign
               ttDataSource.ClassName = ""
               ttdataSource.TypeKey   = 0
               ttdataSource.TypeCode  = ""
               ttdataSource.TableName = ""
               .
        end.   
        if search(cFullname) <> ? then
        do:
            assign 
                csourcename = right-trim(replace(replace(DataSourceDir,StringConstant:BACKSLASH,"/"),"/","."),".") + "." + resourceData.dataSource
                ttDataSource.ClassName = csourcename
                ttdataSource.Type      = "Class"
                .
        end.   
        else if resourceData.codeTableName > "" then 
        do:
            assign
                ttdataSource.TypeCode  = resourceData.codeTableName
                ttdataSource.TypeKey   = resourceData.codeTableKey
                ttdataSource.Type      = "CodeTable"
                .
        end.
        else do:    
            
            assign
                ttdataSource.TableName = resourceData.tableName
                ttdataSource.Type      = "Table"
                .
            if valid-object(resourceData.SdoModel) then 
            do: 
                oSDO = cast(resourceData.SdoModel,ISDOModel).   
              //  ttdataSource.FieldMap = oSdo:GetAssignListNoRowObject().
            end.    
        end.
                 
    end method.  
        
    method public void GenerateDeclarations (phStream as handle, phClass as ClassModel):
        define variable i       as integer no-undo.
        define variable cUsings as character extent no-undo.
        cUsings = phClass:GetUsing().
        put stream-handle phStream unformatted "block-level on error undo, throw." skip.
        put stream-handle phStream skip(1).
        do i = 1 to extent(cUsings):
            put stream-handle phStream unformatted cUsings[i] skip.
        end.    
        
    end method.
    
    method public void GenerateIncludeRefs (phStream as handle, phClass as ClassModel, plbyRef as logical):
        define variable i         as integer no-undo.
        define variable cIncludes as character extent no-undo.
        cIncludes = phClass:GetIncludes().
        do i = 1 to extent(cIncludes):
            put stream-handle phStream unformatted subst("&1&2/&3.i&4&5",
                                                   StringConstant:CURLY_OPEN,
                                                   DefinitionDir,
                                                   cIncludes[i],
                                                   if plByRef then " reference-only" else "",
                                                   StringConstant:CURLY_CLOSE) at 5 skip.
        end.    
        
    end method.
    
    method public void GenerateIncludeRefs (phStream as handle, pcIncludename as character, plbyRef as logical):
        define variable cRelativeFile as character no-undo.
        define variable cFullname as character no-undo.
        cRelativefile = GetRelativeIncludeName(pcIncludename).
        if ApplicationPath > "" then
            cFullname = right-trim(ApplicationPath,"/") + "/" + cRelativeFile.
        else
            cFullname = cRelativeFile.
         
            put stream-handle phStream unformatted subst("&1&2/&3.i&4&5",
                                                   StringConstant:CURLY_OPEN,
                                                   DefinitionDir,
                                                   cFullname,
                                                   if plByRef then " reference-only" else "",
                                                   StringConstant:CURLY_CLOSE) at 5 skip.
           
        
    end method.
    
    
    
end class.