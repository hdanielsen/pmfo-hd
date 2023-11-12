 
 /*------------------------------------------------------------------------
    File        : CodeConverter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Mar 09 16:10:06 EST 2019
    Notes       : Subclasses Calls AddError for convertion problems 
                  Caller will collect from Error 
               -  All of this code is written fast and hacky   
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.BusinessEntityModel from propath.
using Pmfo.Tools.Gui.Model.SdoModel from propath.
using Pmfo.Tools.Gui.Model.DataSourceModel from propath.
using Pmfo.Tools.Gui.Model.DlpModel from propath.
using Pmfo.Tools.Gui.Model.ParameterModel from propath.
using Pmfo.Util.Array from propath.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Tools.Gui.Model.CodeModel from propath.
using Pmfo.Tools.Gui.Model.MethodModel from propath.
using Pmfo.Tools.Gui.Model.AppBuilderModel from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.DataTypeEnum from propath.
using OpenEdge.Core.IOModeEnum from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Tools.AppBuilder.ErrorTracker from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Tools.AppBuilder.ErrorTrackerEnum from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Tools.Gui.Model.ClassModel from propath.


block-level on error undo, throw.

class Pmfo.Tools.AppBuilder.CodeConverter: 
    define variable DataSourceMethodMap as character no-undo
        init "~
preTransactionValidate,PreTransactionValidate,~
beginTransactionValidate,BeginTransactionValidate,~
endTransactionValidate,EndTransactionValidate,~
postTransactionValidate,PostTransactionValidate,~
createPreTransValidate,CreatePreTransValidate,~
writePreTransValidate,WritePreTransValidate,~
deletePreTransValidate,DeletePreTransValidate,~
createBeginTransValidate,CreateBeginTransValidate,~
writeBeginTransValidate,WriteBeginTransValidate,~
deleteBeginTransValidate,DeleteBeginTransValidate,~
createEndTransValidate,CreateEndTransValidate,~
writeEndTransValidate,WriteEndTransValidate,~
deleteEndTransValidate,DeleteEndTransValidate,~
createPostTransValidate,CreatePostTransValidate,~
writePostTransValidate,WritePostTransValidate,~
deletePostTransValidate,DeletePostTransValidate,~
ModifyNewRecord,GetNewRow~
".
  
    define variable BusinesEntityMethodMap as character no-undo
        init "~
RowObjectValidate,ValidateTable~
".
       
    // doesn't exist as of current
    define public property BusinessEntityTypeString as char no-undo init "Pmfo.Core.BusinessLogic.BusinessEntity" get. set.  
    // doesn't exist as of current
    define public property DataSourceTypeString     as char no-undo init "Pmfo.Core.DataLayer.DataSource" get. set.  
    define public property UpdateDataEventArgsString  as char no-undo init "Pmfo.Core.BusinessLogic.IUpdateDataEventArgs" get. set.  
   
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public CodeConverter (  ):
        super ().
    end constructor.

    constructor public CodeConverter (pcBusinessEntityType as char, pcDataSourceType as char , pcUpdateDataRequestType as char):
        super ().
        BusinessEntityTypeString = pcBusinessEntityType.
        DataSourceTypeString = pcDataSourceType.
        UpdateDataEventArgsString = pcUpdateDataRequestType.
    end constructor.

        
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    method protected logical AddForEach(pcMethodname as character):
       return pcMethodname = "ValidateTable". 
    end.    
    
    method protected logical IsBusinessEntityNethod(pcMethodname as character):
       return pcMethodname = "ValidateTable". 
    end.       
    
    method public final BusinessEntityModel CreateBusinessEntity (pcName as char, 
                                                            pSDO as SdoModel, 
                                                            pDLp as DlpModel, 
                                                            pcResource as char, 
                                                            pcTempTableName as char, 
                                                            pcBeforeName as char, 
                                                            pcDefInclude as char,
                                                            pErrorTracker as ErrorTracker):
        return CreateBusinessEntity(pcName,pSDO,pDLP,pcResource,pcTempTableName, pcBeforeName,Array:ToArray(pcDefInclude),pErrorTracker).
    end method.
    
    method public final BusinessEntityModel CreateBusinessEntity (pcName as char, 
                                                            pSDO as SdoModel, 
                                                            pDlp as DlpModel, 
                                                            pcResource as char, 
                                                            pcTempTableName as char, 
                                                            pcBeforeName as char,
                                                            pcDefIncludes as char extent,
                                                            pErrorTracker as ErrorTracker):
        define variable oBusinessEntity as BusinessEntityModel no-undo.
        define variable i as integer no-undo.
        oBusinessEntity = new BusinessEntityModel(pcName,BusinessEntityTypeString).
        oBusinessEntity:Resource = pcResource.
        
        do i = 1 to extent(pcDefIncludes):
            oBusinessEntity:AddInclude(pcDefIncludes[i]).
        end.    
        
        if valid-object(pSdo) then 
        do:
            ConvertSdoHooks(oBusinessEntity,pSDo,BusinesEntityMethodMap,pcTempTableName,pErrorTracker).
        end.    
        if valid-object(pDlp) then 
        do:
            ConvertDLpHooks(oBusinessEntity,pDlp,BusinesEntityMethodMap,pcTempTableName,pcBeforeName,pErrorTracker).
        end.    
        return oBusinessEntity.
    end method.
    
    method public final DataSourceModel CreateDataSource (pcName as char, 
                                                          pSDO as SdoModel, 
                                                          pDlp as DlpModel, 
                                                          pDbTable as char,
                                                          pcTempTable as char, 
                                                          pcBeforeName as char,
                                                          pcDefInclude as char ,
                                                          pErrorTracker as ErrorTracker):
        
       return CreateDataSource(pcName,pSDO,pDLP,pDbTable,pcTempTable, pcBeforeName,Array:ToArray(pcDefInclude),pErrorTracker).
                                                              
    end method.                                                          
    
    method public final DataSourceModel CreateDataSource (pcName as char, 
                                                          pSDO as SdoModel, 
                                                          pDlp as DlpModel, 
                                                          pDbTable as char,
                                                          pcTempTable as char, 
                                                          pcBeforeName as char,
                                                          pcDefIncludes as char extent,
                                                          pErrorTracker as ErrorTracker):
        define variable oDataSource as DataSourceModel no-undo.
        define variable i as integer no-undo.
        define variable lPrepareSave as logical no-undo.
        define variable lPrepareAfter as logical no-undo.
        oDataSource = new DataSourceModel(pcName,DataSourceTypeString).
        
        do i = 1 to extent(pcDefIncludes):
           oDataSource:AddInclude(pcDefIncludes[i]).
        end.    
        
        if valid-object(pSdo) then 
        do:
            oDataSource:FieldMapping = pSdo:GetAssignListNoRowObject().
            oDataSource:BaseQuery    = FormatQuery(pSDO:BaseQuery).
            extent(oDataSource:BufferNames) = num-entries(pSdo:QueryTables).
            do i = 1 to extent(oDataSource:BufferNames):
                 oDataSource:BufferNames[i] = entry(i,pSdo:QueryTables).
            end.
            lPrepareSave = ConvertSdoHooks(oDataSource,pSDo,DataSourceMethodMap,pcTempTable,pErrorTracker).
            // todo move into setter of methods?? 
            if lPrepareSave then 
                oDataSource:OverridePrepareSaveBuffer = true.
            lPrepareAfter = ConvertDataCalculate(oDataSource:Methods, pSDo, pcTempTable,pErrorTracker).
            if lPrepareAfter then
                oDataSource:OverridePrepareBuffer = true.
        end.    
        else do:
            if pDbTable  > "" then
                oDataSource:BaseQuery = subst("for each &1 no-lock indexed-reposition",pDbTable).
        end.    
        if valid-object(pDlp) then 
        do:
           lPrepareSave = ConvertDlpHooks(oDataSource,pDlp,DataSourceMethodMap,pcTempTable,pcBeforeName,pErrorTracker).
           if lPrepareSave then 
               oDataSource:overridePrepareSaveBuffer = true.
        end.    
        return oDataSource.
    end method.
    
    method protected character FormatQuery(pQuery as character):
        do while index(pQuery,StringConstant:SPACE + StringConstant:SPACE) > 0:
            pQuery = replace(pQuery,StringConstant:SPACE + StringConstant:SPACE,StringConstant:SPACE).
        end.    
        return lc(pQuery).
    end method.    
                                                                                                                               
    method protected logical ConvertSdoHooks(oModel as ClassModel, 
                                         pSdoModel as SdoModel, 
                                         pMethodMap as char, 
                                         pcTempTableName as char, 
                                         pErrorTracker as ErrorTracker):
        return ConvertHooks(oModel, pSdoModel, pMethodMap, pcTempTableName, "", "RowObjUpd",  "",pErrorTracker).
    end method.     
    
    method protected logical ConvertDlpHooks(oModel as ClassModel,  
                                         pDlpModel as DlpModel, 
                                         pMethodMap as char, 
                                         pcTempTableName as char,
                                         pcBeforeTableName as char,
                                         pErrorTracker as ErrorTracker ):
        define variable cOrigTempTableName as character no-undo.
        define variable cOrigBeforeTableName as character no-undo.
        cOrigTempTableName = pDlpModel:DataLogicTable.
        cOrigBeforeTableName = "old_" + cOrigTempTableName.
        cOrigTempTableName   = "b_" + cOrigTempTableName.
        return ConvertHooks(oModel, pDlpModel, pMethodMap, pcTempTableName, pcBeforeTableName, cOrigTempTableName, cOrigBeforeTableName,pErrorTracker).
    end method.     
    
    method protected logical ConvertDataCalculate(pMethods as ICollection, 
                                                  pModel as SDOModel, 
                                                  pcTempTableName as char,
                                                  pErrorTracker as ErrorTracker):
        define variable oIter        as IIterator no-undo.  
        define variable oCode        as CodeModel no-undo.
        define variable oMethod      as MethodModel no-undo.
        define variable lMethodAdded as logical no-undo.
        
        oIter = pModel:Procedures:Iterator().                      
        do while oIter:HasNext():
            oCode = cast(oIter:Next(),CodeModel).
            if oCode:name = "DATA.CALCULATE" then  
            do:
                oMethod = new MethodModel("AfterRow").
                oMethod:code = ConvertDataCalculateHook(pModel,oCode,pcTempTableName,pErrorTracker).
                pMethods:Add(oMethod).
                oMethod:Parameters:Add(new ParameterModel("phDataset",IOModeEnum:Input,"dataset-handle")).
                lMethodAdded = true.
                leave.
            end.    
        end.    
        return lMethodAdded.
    end method.    
    
    method protected longchar ConvertDataCalculateHook
                                         (pModel as SDOModel,
                                          pCode as CodeModel, 
                                          pcTempTableName as char,
                                          pErrorTracker as ErrorTracker):
         define variable cLong         as character no-undo.
         
         cLong = replace(pCode:Code,"RowObject.",pcTempTableName + ".").
         // end will be added during serialization */
         cLong = replace(cLong,"end procedure.","").
         
         // TODO Convert RETURN "some error" to throw - check for quotes and replace with undo, throw
         return cLong.              
    end method.    
    
    method protected final logical ConvertHooks(pClassModel as ClassModel,  
                                          pModel as AppBuilderModel, 
                                          pMethodMap as char, 
                                          pcTempTableName as char,
                                          pcBeforeTableName as char, 
                                          pcOriginalTempTableName as char, 
                                          pcOrignalBeforeTableName as char,
                                          pErrorTracker as ErrorTracker):
        define variable oIter        as IIterator no-undo.  
        define variable oCode        as CodeModel no-undo.
        define variable cMappedname  as character no-undo.
        define variable oMethod      as MethodModel no-undo.
        define variable lMethodAdded as logical no-undo.
        
        oIter = pModel:Procedures:Iterator().                      
        do while oIter:HasNext():
            oCode = cast(oIter:Next(),CodeModel).
            cMappedname = StringUtil:GetMappedName(oCode:name,pMethodMap).
            if cMappedName > "" then 
            do:
                oMethod = new MethodModel(cMappedName).
                oMethod:code = ConvertHook(cMappedName,oCode,pcTempTableName,pcBeforeTableName,pcOriginalTempTableName,pcOrignalBeforeTableName,pErrorTracker).
                pClassModel:Methods:Add(oMethod).
                if type-of(pClassModel,DataSourceModel) then
                    oMethod:Parameters:Add(new ParameterModel("phDataset",IOModeEnum:Input,"dataset-handle")).
                else do:
                    oMethod:Parameters:Add(new ParameterModel("pUpdateDataEventArgs",IOModeEnum:Input,"IUpdateDataEventArgs")).
                    
                    oMethod:Parameters:Add(new ParameterModel(pcTempTableName,IOModeEnum:Input,"table")).
                end.    
                lMethodAdded = true.
            end. 
        end.   
        if lMethodAdded and type-of(pClassModel,BusinessEntityModel) then
            pClassModel:AddUsing(UpdateDataEventArgsString).
        return lMethodAdded.
    end method.    
    
    method protected longchar ConvertHook(pMethodName as char, /* for override error log */
                                          pCode as CodeModel, 
                                          pcTempTableName as char,
                                          pcBeforeTableName as char, 
                                          pcOriginalTempTableName as char, 
                                          pcOriginalBeforeTableName as char,
                                          pErrorTracker as ErrorTracker):
                                              
         define variable cLong         as character no-undo.
         define variable i             as integer no-undo.
         define variable cLine         as character no-undo.
         define variable lComment      as logical no-undo init ?.
         define variable lForeachadded as logical no-undo.
         
         cLong = replace(pCode:Code,pcOriginalTempTableName,pcTempTableName).
         if pcBeforeTableName  > "" and pcOriginalBeforeTableName > "" then
             cLong = replace(cLong,pcOriginalBeforeTableName,pcBeforeTableName).
         // end will be added during serialization */
        
         cLong = replace(cLong,"end procedure.","").
         if index(cLong,"isAdd") > 0 or index(cLong,"isCopy") > 0 then
            cLong = replaceIsAddAndCopy(pMethodname,cLong,pcTempTableName,pErrorTracker).
         if AddForEach(pMethodname) then
         do: 
             do i = 1 to num-entries(cLong,StringConstant:LF):
                cLine = entry(i,cLong,StringConstant:LF).
                
                if cLine > "" then
                do:
                    // set comment flag if first encountered line is comment
                    if lComment = ? then 
                    do:
                       if trim(cLine) begins "/*" then 
                          lComment = true.
                       else 
                          lComment = false.   
                    end.
                    if lComment then
                    do:
                       if trim(cLine) matches "~*/" then
                         lComment = false.
                    end.
                    else do:
                       if not lforeachadded then
                       do:
                          entry(i,cLong,StringConstant:LF) = (if opsys = "unix" then StringConstant:LF else StringConstant:CRLF)  
                                                           + subst("    for each &1:",pcTempTableName)
                                                           + (if opsys = "unix" then StringConstant:LF else StringConstant:CRLF). 
                          lforEachadded  = true.  
                          i = i + 2.
                          
                       end.
                       entry(i,cLong,StringConstant:LF) = "        " + cLine.
                   end.    
                end.
             end.
             cLong = right-trim(cLong,(if opsys = "unix" then StringConstant:LF else StringConstant:CRLF)).
             cLong = cLong              
                    + (if opsys = "unix" then StringConstant:LF else StringConstant:CRLF)  
                    + "    end. //for each"
                    + (if opsys = "unix" then StringConstant:LF else StringConstant:CRLF).
                     
         end.    
         // TODO Convert RETURN "some error" to throw - check for quotes and replace with undo, throw
         return cLong.              
    end method.    
    
    method protected longchar ReplaceIsAddAndCopy(pMethodname as char, pcLong as longchar,pcTablename as char,pErrorTracker as ErrorTracker):
        define variable iAdd      as integer   no-undo.
        define variable iCopy     as integer   no-undo.
        define variable iFound    as integer   no-undo.
        define variable cReplace  as character no-undo.
        define variable iSpace    as integer   no-undo.
        define variable cChunk    as longchar  no-undo.
        define variable cWord     as character no-undo.
        define variable iLength   as integer   no-undo.
        define variable iState    as integer   no-undo.
        define variable lDone     as logical   no-undo.
         
        iFound = 1.
        
        cReplace = subst("row-state(&1) = row-created",pcTablename).
        bigloop:
        do while true:
            iAdd = index(pcLong,"isAdd",iFound).
            iCopy = index(pcLong,"isCopy",iFound).
            
            if iAdd > 0 then iFound = iAdd.
            if iCopy > 0 and (iCopy < iAdd or iAdd = 0) then iFound = iCopy.
            
            if iAdd > 0 or iCopy > 0 then 
            do:
                iState = 0.
                lDone = false.
                iLength = 0.
                cChunk = substr(pcLong,iFound).
                do iSpace = 1 to num-entries(cChunk,StringConstant:SPACE):
                    cWord = entry(iSpace,cChunk,StringConstant:SPACE).
                    if cWord <> StringConstant:LF and cWord <> StringConstant:CRLF then
                    do:
                       iLength = iLength + (if iSpace = 1 then 0 else 1). 
                       if cWord begins "isAdd()" then 
                       do:
                            iState = iState + 1.
                            iLength = iLength + length("isAdd()").
                            if iState = 3 then
                               lDone = true.
                            else if cWord begins "isAdd())" then
                               lDone = true.
                               
                            if cword matches "*" + StringConstant:Lf or cword matches "*" + StringConstant:crLf then
                            do:
                               iLength = iLength + 1.
                            end.       
                       end.
                       else if cWord begins "isCopy()" then 
                       do:
                            iState = iState + 1.
                            iLength = iLength + length("isCopy()").
                            if iState = 3 then
                               lDone = true.
                            else if cWord begins "isCopy())" then
                               lDone = true.   
                               
                            if cword matches "*" + StringConstant:Lf or cword matches "*" + StringConstant:crLf then
                            do:
                               iLength = iLength + 1.
                            end.       
                      
                       end.
                       else if cWord = "or" or cWord = "or" + StringConstant:Lf or cword = "or" + StringConstant:crLf  then 
                       do:
                           if iState = 1 then
                           do: 
                               iState = 2.
                               iLength = iLength + length(cword).
                               if cword matches "*" + StringConstant:Lf or cword matches "*" + StringConstant:crLf then
                               do:
                                  iLength = iLength + 1.
                               end.       
                           end.    
                           else lDone = true.
                             
                       end.
                       else if cWord <> "" then 
                       do:
                           iLength = iLength - 1. // keep the last counted space
                           lDone = true.
                       end.
                    end.
                       
                    if lDone then
                    do: 
                        if iState = 1 or iState = 3 then
                        do:
                            substr(pcLong,iFound,iLength) = cReplace.
                            iFound = iFound + length(cReplace).
                        end.    
                        else do:
                            pErrorTracker:AddError(ErrorTrackerEnum:ISAddOrCopyReplace,pMethodName).
                            // not sure if this can happen, but just in case bail to avoid endless loop 
                            if iLength = 0 then leave bigloop.
                            iFound = iFound + iLength.
                        end.        
                        leave. 
                    end.       
                end.
            end.
            else 
                leave.
         end.
         return pcLong.
    end method.     
    
end class.