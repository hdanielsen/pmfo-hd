 
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
using Pmfo.Tools.Gui.Model.ISdoModel from propath.
using Pmfo.Tools.Gui.Model.AdoModel from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Tools.Gui.Model.ProcedureModel from propath.
using Pmfo.Tools.Gui.Model.FieldModel from propath.


block-level on error undo, throw.

class Pmfo.Tools.AppBuilder.CodeConverter: 
    define variable DataSourceMethodMap as character no-undo
        init "~
preTransactionValidate,PreTransactionValidate,~
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
       
    define variable DataSourceRowObjUpdMethodMap as character no-undo
        init "beginTransactionValidate,BeginTransactionValidate".
       
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
    
    method protected logical UseForEach(pcMethodname as character):
       return pcMethodname = "ValidateTable". 
    end.    
    
    method protected logical IsBusinessEntityMethod(pcMethodname as character):
       return pcMethodname = "ValidateTable". 
    end.       
    
    method public final BusinessEntityModel CreateBusinessEntity (pcName as char, 
                                                            pSDO as ISdoModel, 
                                                            pDLp as DlpModel, 
                                                            pcResource as char, 
                                                            pcTempTableName as char, 
                                                            pcBeforeName as char, 
                                                            pcDefInclude as char,
                                                            pErrorTracker as ErrorTracker):
        return CreateBusinessEntity(pcName,pSDO,pDLP,pcResource,pcTempTableName, pcBeforeName,Array:ToArray(pcDefInclude),pErrorTracker).
    end method.
    
    method public final BusinessEntityModel CreateBusinessEntity (pcName as char, 
                                                            pSDO as ISdoModel, 
                                                            pDlp as DlpModel, 
                                                            pcResource as char, 
                                                            pcTempTableName as char, 
                                                            pcBeforeName as char,
                                                            pcDefIncludes as char extent,
                                                            pErrorTracker as ErrorTracker):
        define variable oBusinessEntity as BusinessEntityModel no-undo.
        define variable i as integer no-undo.
        define variable oDLpProcedureIterator as IIterator no-undo.
        define variable oFieldIterator as IIterator no-undo.
        define variable oDLpProcedure as CodeModel  no-undo.
        define variable cFieldName as character no-undo.
        define variable cFields as character no-undo.
        define variable oField as FieldModel  no-undo.
        define variable oMethod       as MethodModel no-undo.
          
        oBusinessEntity = new BusinessEntityModel(pcName,BusinessEntityTypeString).
        oBusinessEntity:Resource = pcResource.
        
        do i = 1 to extent(pcDefIncludes):
            oBusinessEntity:AddInclude(pcDefIncludes[i]).
        end.    
        
        if valid-object(pSdo) and type-of(pSdo,SdoModel)then 
        do:
            ConvertSdoHooks(oBusinessEntity,cast(pSDo,SdoModel),BusinesEntityMethodMap,pcTempTableName,pErrorTracker).
        end.    
        if valid-object(pDlp) then 
        do:
            ConvertDLpHooks(oBusinessEntity,pDlp,BusinesEntityMethodMap,pcTempTableName,pcBeforeName,pErrorTracker).
            if valid-object(pSdo) then
            do:
                oFieldIterator = pSdo:Fields:Iterator().
                do while oFieldIterator:HasNext():
                    oField = cast(oFieldIterator:Next(),FieldModel).
                    cFields = StringUtil:AppendEntry(cFields,oField:name).
                end. 
            end.
            oDLpProcedureIterator = pDLp:Procedures:Iterator().
            do while oDLpProcedureIterator:HasNext():
                oDLpProcedure = cast(oDLpProcedureIterator:Next(),CodeModel).
                if oDlpProcedure:name matches "*validate" then
                do: 
                    cFieldName = replace(oDlpProcedure:name,"validate","").
                    if lookup(cFieldname,cFields) > 0 then 
                    do:
                        define variable cParam as character no-undo.
                        oMethod = new MethodModel(oDLpProcedure:name).
                        oMethod:code = ConvertFieldValidation(oBusinessEntity,oDLpProcedure:name,oDLpProcedure,pcTempTableName,"","b_" + pcBeforeName,"",pErrorTracker, output cPAram).
                        oMethod:Parameters:Add(new ParameterModel(cParam,OpenEdge.Core.IOModeEnum:Input,DataTypeEnum:Character)).
                        oBusinessEntity:Methods:Add(oMethod).
                    end.  
                end.
              
            end.    
        end.
        return oBusinessEntity.
    end method.
    
    method public final DataSourceModel CreateDataSource (pcName as char, 
                                                          pSDO as ISdoModel, 
                                                          pDlp as DlpModel, 
                                                          pDbTable as char,
                                                          pcTempTable as char, 
                                                          pcBeforeName as char,
                                                          pcDefInclude as char ,
                                                          pErrorTracker as ErrorTracker):
        
       return CreateDataSource(pcName,pSDO,pDLP,pDbTable,pcTempTable, pcBeforeName,Array:ToArray(pcDefInclude),pErrorTracker).
                                                              
    end method.                                                          
    
    method public final DataSourceModel CreateDataSource (pcName as char, 
                                                          pSDO as ISdoModel, 
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
        define variable oMethod       as MethodModel no-undo.
        define variable cBufferRefs as character  no-undo.
        oDataSource = new DataSourceModel(pcName,DataSourceTypeString).
        
        do i = 1 to extent(pcDefIncludes):
           oDataSource:AddInclude(pcDefIncludes[i]).
        end.    
        
        if valid-object(pSdo) then 
        do:
            if type-of(pSdo,Sdomodel) then 
                oDataSource:FieldMapping = cast(pSdo,SdoModel):GetAssignListNoRowObject().
            else  
                oDataSource:FieldMapping = pSdo:AssignList.
            oDataSource:BaseQuery    = FormatQuery(pSDO:BaseQuery).
            
           
            extent(oDataSource:BufferNames) = num-entries(pSdo:QueryTables).
            do i = 1 to extent(oDataSource:BufferNames):
                 oDataSource:BufferNames[i] = entry(i,pSdo:QueryTables).
                 cBufferRefs = StringUtil:AppendEntry(cBufferRefs,subst("buffer &1:handle",oDataSource:BufferNames[i])). 
            end.
            if extent(oDataSource:BufferNames) > 1 then 
            do:
                oMethod = new MethodModel("GetBaseQuery").
                oMethod:code = '    return ~n      ' + quoter(replace(oDataSource:BaseQuery,StringConstant:LF,StringConstant:LF + "     ")) + '.'. 
                
                oMethod:DataType = OpenEdge.Core.DataTypeEnum:character.
                oDataSource:Methods:Add(oMethod).
            
                oMethod = new MethodModel("GetBufferHandles").
                oMethod:Parameters:Add(new ParameterModel("pRequest",OpenEdge.Core.IOModeEnum:Input,"IGetDataTableRequest")).
                oMethod:Parameters:Add(new ParameterModel("pParentRequest",OpenEdge.Core.IOModeEnum:Input,"IGetDataTableRequest")).
                oDataSource:AddUsing("Pmfo.Util.Array").
                oDataSource:AddUsing("Pmfo.Core.BusinessLogic.IGetDataTableRequest").
                
                oMethod:code = subst('    return Array:ToArray(&1).',cBufferRefs). 
                
                oMethod:DataType = OpenEdge.Core.DataTypeEnum:handleArray.
                oDataSource:Methods:Add(oMethod).
            end.    
            // TODO fiox field mapping for SDO
            if oDataSource:FieldMapping > "" and type-of(pSdo,AdoModel) then 
            do:
                oMethod = new MethodModel("GetFieldMap").
                oMethod:DataType = OpenEdge.Core.DataTypeEnum:character.
                
                oMethod:code = '    define variable cFieldMap as character no-undo.~n~n'
                             + '    cFieldmap = super:GetFieldMap().~n' 
                             + subst('    cFieldMap = StringUtil:AppendEntry(cFieldMap,"&1").~n',oDataSource:FieldMapping) 
                             + '    return cFieldMap.~n' 
                             .
                oDataSource:AddUsing("Pmfo.Util.StringUtil").
                oDataSource:Methods:Add(oMethod).
            end.    
            
            if type-of(pSdo,Sdomodel) then 
                lPrepareSave = ConvertSdoHooks(oDataSource,cast(pSdo,SdoModel),DataSourceMethodMap,pcTempTable,pErrorTracker).
            else if type-of(pSdo,AdoModel) and valid-object (pDlp)then
            do:
                define variable lPrepare2 as logical no-undo.
                // rowobjupd
                lPrepare2 = ConvertSdoHooks(oDataSource,pDlp,DataSourceRowObjUpdMethodMap,pcTempTable,pErrorTracker). 
                
                if not lPreparesave then
                    lPreparesave = lprepare2.    
            end.    
            // todo move into setter of methods?? 
            if lPrepareSave then 
                oDataSource:OverridePrepareSaveBuffer = true.
            if type-of(pSdo,Sdomodel) then 
               lPrepareAfter = ConvertDataCalculate(oDataSource:Methods, cast(pSDo,SdoModel), pcTempTable,pErrorTracker).
            else if type-of(pSdo,Adomodel) then
               lPrepareAfter = ConvertCalculateFunctions(oDataSource:Methods, pDLP, pcTempTable,pErrorTracker).

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
    
     // Get the first curly brace in the string from start to end  
    method public void ParseInclude(
        input pcCode as longchar,
        output pcName as character,
        output pcParameters as character extent,  
        output piStart as integer,
        output piLength as integer
                               ) :
        /*------------------------------------------------------------------------------
          Purpose: Utility function that extracts a parenthesis from a string
                   ignoring nested parenthesises               
        Parameters: pcString = string that might have a parenthesis
        Returns the first parenthsis with content.  
        ------------------------------------------------------------------------------*/
        define variable iChr           as integer   no-undo.
        define variable iState         as integer   no-undo.
        define variable cQuote         as character no-undo.         
        define variable cChr           as character no-undo.   
        define variable cWord          as character no-undo.   
        define variable iWord          as integer no-undo.
        define variable iSingleQuote   as integer no-undo.  
        define variable iDoubleQuote   as integer no-undo.
        define variable cContent       as character no-undo.
        define variable cMaskedContent as character no-undo.
        define variable cQuotedTokens  as character extent no-undo.
        define variable iMaskCount     as integer no-undo.
       
        piStart = index(pcCode,"~{").
        if piStart > 0 then 
        do:
            charloop:
            do iChr = piStart to length(pcCode):
                cChr = substring(pcCode,iChr,1).
                
                if cChr = "~}" then 
                do:
                    iState = istate - 1.
                end.
                else if cChr = "~{" then
                do:
                    iState = istate + 1.  
                end.  
                if iState = 0 then
                do:
                    cContent = substring(pcCode,piStart,ichr - (piStart - 1)).
                    cmaskedContent = StringUtil:MaskQuotes (cContent,chr(1),output cQuotedTokens).
                    cMaskedContent = substr(cMaskedContent,2,length(cMaskedContent) - 2).
                    do iWord = 1 to num-entries(cMaskedContent,""): 
                        cWord = entry(iWord,cMaskedContent,"").
                        if cWord > "" then
                        do:
                            if iWord = 1 then 
                                pcName = cword.
                            else do:
                                if cWord begins chr(1) then  
                                do:
                                     iMaskCount = iMaskCount + 1.
                                     cWord = StringUtil:UnquoteValue(cQuotedTokens[iMaskCount]).
                                      
                                     // twice sonce [arams mau have double quotes inside singe quotes 
                                     cWord = StringUtil:UnquoteValue(cWord).
                                end.         
                                extent(pcParameters) = (if extent(pcParameters) = ? then 0 else extent(pcParameters)) + 1.
                                if cWord = "?" then 
                                    pcParameters[extent(pcParameters)] = ?.
                                else     
                                    pcParameters[extent(pcParameters)] = cword.
                            end.
                        end.
                    end.
                    piLength = ichr - (piStart - 1).
                    leave charloop.
                end.   
            end. 
            if istate > 0 then 
                undo, throw new IllegalArgumentError(subst("'&1' is missing a rught curly brace",pcCode)).   
            if istate < 0 then 
                undo, throw new IllegalArgumentError(subst("'&1' is missing a left curly brace",pcCode)).   
        end.
        
    end method. 
    
     // Get the first curly brace in the string from start to end  
    method public logical ParseMessage(
        input pcCode as longchar,
        output pcMessage as character,
        output pcParameters as character extent,  
        output piStart as integer,
        output piLength as integer
                               ) :
        /*------------------------------------------------------------------------------
          Purpose: Utility function that extracts a parenthesis from a string
                   ignoring nested parenthesises               
        Parameters: pcString = string that might have a parenthesis
        Returns the first parenthsis with content.  
        ------------------------------------------------------------------------------*/
        define variable iChr           as integer   no-undo.
        define variable iState         as integer   no-undo.
        define variable cQuote         as character no-undo.         
        define variable cChr           as character no-undo.   
        define variable cWord          as character no-undo.   
        define variable iWord          as integer no-undo.
        define variable iSingleQuote   as integer no-undo.  
        define variable iDoubleQuote   as integer no-undo.
        define variable cContent       as character no-undo.
        define variable cMaskedContent as character no-undo.
        define variable cQuotedTokens  as character extent no-undo.
        define variable iMaskCount     as integer no-undo.
        define variable cPrev          as character no-undo.
        piStart = index(pcCode,"message").
        if piStart > 0 then 
        do:
            if piStart > 2 then
            do: 
                cPrev = substr(pccode,pistart - 1,1).
                if cPrev <> ""  
                or cPrev <> StringConstant:LF
                or cPrev <> StringConstant:CR then 
                   return false.
            
            end.
            
            
            cContent = substring(pcCode,piStart,ichr - (piStart - 1)).
                    cmaskedContent = StringUtil:MaskQuotes (cContent,chr(1),output cQuotedTokens).
                    cMaskedContent = substr(cMaskedContent,2,length(cMaskedContent) - 2).
                    do iWord = 1 to num-entries(cMaskedContent,""): 
                        cWord = entry(iWord,cMaskedContent,"").
                        if cWord > "" then
                        do:
                            if iWord = 1 then 
                                pcMessage = cword.
                            else do:
                                if cWord begins chr(1) then  
                                do:
                                     iMaskCount = iMaskCount + 1.
                                     cWord = StringUtil:UnquoteValue(cQuotedTokens[iMaskCount]).
                                      
                                     // twice sonce [arams mau have double quotes inside singe quotes 
                                     cWord = StringUtil:UnquoteValue(cWord).
                                end.         
                                extent(pcParameters) = (if extent(pcParameters) = ? then 0 else extent(pcParameters)) + 1.
                                if cWord = "?" then 
                                    pcParameters[extent(pcParameters)] = ?.
                                else     
                                    pcParameters[extent(pcParameters)] = cword.
                            end.
                        end.
                    end.
                    piLength = ichr - (piStart - 1).
            
          
        end.
        
    end method. 
    
    method protected character FormatQuery(pQuery as character):
        do while index(pQuery,StringConstant:SPACE + StringConstant:SPACE) > 0:
            pQuery = replace(pQuery,StringConstant:SPACE + StringConstant:SPACE,StringConstant:SPACE).
        end.    
        return lc(pQuery).
    end method.    
                                                                                                                               
    method protected logical ConvertSdoHooks(oModel as ClassModel, 
                                         pSdoModel as AppBuilderModel, 
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
    
    method protected logical ConvertCalculateFunctions(
        pMethods as ICollection, 
        pModel as DLpModel, 
        pcTempTableName as char,
        pErrorTracker as ErrorTracker):
        define variable oIter        as IIterator no-undo.  
        define variable oCode        as CodeModel no-undo.
        define variable oMethod      as MethodModel no-undo.
        define variable lMethodAdded as logical no-undo.
        define variable cFieldName   as character no-undo.
        define variable cNewCode     as character // longchar ? 
                                        no-undo.
        define variable cBody        as character //longchar  ?
                                        no-undo.
        define variable cBuffers      as character extent no-undo.
        define variable cFoundBuffers as character extent no-undo.
        define variable iBuffer      as integer no-undo.
        define variable cBufferstatements as character no-undo.   
        extent(cBuffers) = ?.                              
        oIter = pModel:Functions:Iterator().                      
        do while oIter:HasNext():
            oCode = cast(oIter:Next(),CodeModel).
            if oCode:name begins "Calculate" then
            do:
                cFieldname = substr(oCode:name,10).
                cNewCode = ConvertCalculateFunction(pModel, oCode, pcTempTableName, cFieldname, pErrorTracker, output cFoundBuffers).
                if extent(cBuffers) <> ? and extent(cFoundBuffers) <> ? then
                    cBuffers = Array:MergeArrays(cBuffers,cFoundBuffers).   
                else if extent(cFoundBuffers) <> ? then
                    cBuffers = cFOundbuffers.
                    
                if cNewCode > "" then
                do: 
                   cBody = StringUtil:AppendEntry(cBody,cNewCode,StringConstant:LF).
                end.   
                   
            end. 
        end.
        if cBody gt "" then 
        do: 
            oMethod = new MethodModel("AfterRow").
            cBody = "    super:AfterRow(input dataset-handle phDataSet by-reference)." + StringConstant:LF + StringConstant:LF + cBody.
            
            do iBuffer = 1 to extent(cBuffers):
                // todo check for duplicates
                cBufferStatements = StringUtil:AppendEntry(cBufferStatements,cBuffers[iBuffer],StringConstant:LF).
            end.     
            if cBufferstatements > "" then cBody = cBufferstatements + StringConstant:LF +  StringConstant:LF +  cBody.
            oMethod:code = cBody.
            pMethods:Add(oMethod).
            oMethod:Parameters:Add(new ParameterModel("phDataset",IOModeEnum:Input,"dataset-handle")).
            lMethodAdded = true.
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
    
    method protected longchar ConvertCalculateFunction
                                         (pModel as DLpModel,
                                          pCode as CodeModel, 
                                          pcTempTableName as char,
                                          pcfieldname as character,
                                          pErrorTracker as ErrorTracker,
                                          output pcBuffers as character extent ):
         define variable cLong         as character no-undo.
         define variable cLine         as character no-undo.
         define variable i     as integer no-undo.
         define variable cWord as character no-undo.
         define variable iWord as integer no-undo.
         define variable iState as integer no-undo.
         define variable cVariable as character no-undo.
         define variable cdatatype as character no-undo. 
         define variable cObjfield as character no-undo. 
         define variable cReturnField as character no-undo.
         define variable lComment as logical no-undo.
         define variable cComment as character no-undo.
         define variable iComment as integer no-undo.
         define variable cReplaces as character extent no-undo. 
         define variable cKeyfield as character no-undo.
         define variable ckeyhandle as character no-undo.
         define variable cReturn    as character no-undo.
         define variable iReturn    as integer no-undo.
         define variable lReturnfound as logical no-undo.
        // message "convert function for field name" pcfieldname
       //  view-as alert-box.
         readline:
         do i = 1 to num-entries(pCode:code,StringConstant:LF): 
             cLine = string(entry(i,pCode:code,StringConstant:LF)).
             
             if cLine > "" then
             do:
                 if trim(cline) begins "(" and (trim(cline) matches "(*):" or trim(cline) matches "(*) :")  then 
                     next readline.  
                 
                 if trim(cline) begins "end function" then 
                     next readline.
                  
                 if left-trim(cline) begins "/*" then
                 do: 
                     lComment =true.
                     iComment = 1.
                     
                     cLong = cLong + StringConstant:LF + cline.           
                       
                 end.      
                 if lComment then 
                 do:
                     if iComment > 1 then 
                         cLong = cLong + StringConstant:LF + cline.           
                     iComment = iComment + 1. 
                     // only look for the slash
                     if right-trim(cline) matches "*/" then
                         lComment = false.        
                     next readline.    
                 end.
                 
                 if left-trim(cLine) begins "DEF" then 
                 do:
                     if left-trim(cLine) matches "DEF*BUFFER*FOR*" then
                     do:
                         extent(pcBuffers) = (if extent(pcBuffers) =? then 0 else extent(pcBuffers)) + 1.
                         pcBuffers[extent(pcBuffers)] = cline.
                     end. 
                     
                     iState  = 0.
                     cVariable = "".
                     do iword = 1 to num-entries(cLine,""):
                         cword = entry(iWord,cline,"").
                         if iState = 0 and cWord begins "def" then 
                             iState = 1.
                         else if iState = 1 and cWord begins "var" then 
                             iState = 2.
                         else if iState = 2 then
                         do: 
                             iState = 3.
                             cVariable = cword.
                         end.     
                         else if iState = 3 and cWord = "as" then
                             iState = 4.
                         else if iState = 4 then
                         do:
                             cDatatype = cword. 
                             iState = 5.
                             leave.
                         end.
                     end.  
                     if iState = 5 then 
                     do:
                         if cDataType <> "handle" then 
                         do:
                             if cVariable matches("*_obj") then 
                                 cObjfield  = cVariable.
                             else do:
                                 cReturnfield = cVariable.
                             end.         
                         end.
                      
                         next Readline.
                     end.
                 end.
                 else do:
                     if cLine matches "*dynamic-function*rowobject*" then 
                         next readline.
                     
                     if cLine matches "*hRowobject:buffer-field*" then
                     do: 
                         extent(cReplaces) = ?.
                         StringUtil:MaskQuotes(cline, "@",output creplaces).
                         cKeyField = substr(cReplaces[1],2,length(cReplaces[1]) - 2).
                         if cObjField = "" then // TODO always do r this here
                             cObjField = entry(1,left-trim(cLine),"").
         //                message "keyfield" Ckeyfield 
           //              "from line"  cLine
             //            view-as alert-box.
                         next readline.
                     end.    
                     
                     // TODO handle in separate loop and check start and IF ELSE 
                     // to manage cases where there is a RETURN at end after en IF RETURN
                     iReturn = index(cline,"return ").
                     if iReturn > 0 then
                     do: 
                         define variable istart as integer no-undo.
                         define variable iend    as integer no-undo.
                         define variable cDummy as character no-undo.
                         
                         if cReturnField > ""  
                        // left-trim(substr(cLine,iReturn + length("return") + 1))  skip
                        // left-trim(substr(cLine,iReturn + length("return") + 1)) begins cReturnField 
                         
                       //  view-as alert-box.
                         and left-trim(substr(cLine,iReturn + length("return") + 1)) begins cReturnField then 
                         do:
                             lReturnfound = true.
                             next readline.   
                         end.     
                         
                       //  cReturn = entry(1,left-trim(substr(left-trim(substr(cLine,iReturn)),length("Return") + 1)),"").
                        
                        // if cReturn = cReturnfield or cReturn = cReturnfield + "." then
                          //   next readline. 
                         else do:
                             // hack to get rid of bottom return blank - see TODO above
                             if lReturnFound 
                             and index(cLine,"ELSE ") = 0
                             and (left-trim(substr(cLine,iReturn + length("return") + 1)) begins '""' 
                             or  left-trim(substr(cLine,iReturn + length("return") + 1)) begins '" "') then
                             do: 
                                 cline = subst("  // Commented by code generator. Assumed to wipe our the real assignment.~n//&1",
                                              replace(cline,"return",subst("&1.&2 = ",pcTemptablename,pcFieldname))
                                              ). 
                             end.
                             else
                                 cline = replace(cline,"return",subst("&1.&2 = ",pcTemptablename,pcFieldname)).
                         end.
                         lReturnfound = true.                      
                          
                     end.
                     
                     else do:
                         define variable lReplaced as logical no-undo.
                         define variable lDirectWhere as logical no-undo.
                         lDirectwhere  = false.
                         lReplaced = false.
                         
                         if cLine matches "*_obj:buffer-value*" then
                         do: 
                             ckeyhandle = entry(1,left-trim(cLine),"").
                             cKeyhandle = substr(cKeyHandle,2,length(cKeyhandle) - 1).
                             next readline.
                         end.  
                         else if cLine matches subst("*where*_obj*&1:buffer-value*",cObjfield) then
                         do:
                             cLine  = replace(cLine,subst("&1:buffer-value",cObjField),subst("&1.&2",pcTemptablename,cKEyfield)).         
                             lREplaced = true. 
                         end.
                         else do:
                      //   message
                        // "matched obj buffer-value"   cLine matches "*_obj:buffer-value*"
                        //  "keyhandle:" cKeyhandle "keyfield:" ckeyfield "obj:" cObjfield skip 
                        //  cline 
                       //  view-as alert-box.
                         
                         
                             if (cKEyhandle > "" or lDirectWhere) and cKEyfield > "" then
                             do:  
                                 if index(cLine,cObjfield) > 0 then
                                 do:
                                    define variable iPos as integer no-undo.
                                    iPos = index(cLine," " + cObjfield).
                                    
                                    substr(cLine,iPos + 1,length(cObjfield)) = subst("&1.&2",pcTemptablename,cKEyfield).
                             //       message
                               //         "after replace of objfield" cObjfield "keyfield" cKeyfield skip                  
                                 //       cLine
                                   // view-as alert-box.
                                    lReplAceD = tRue.
                                 end.
                             end.
                         end.
                         if lReplaced = false and cVariable > "" then
                         do:
                             
                             cLine = replace(cLine,cVariable,subst("&1.&2",pcTemptablename,pcFieldname)).
                         end.  
                     end.     
                     
                     cLong = cLong + StringConstant:LF + cline.           
                 end.
             end.              
         end.
         
         
         //cLong = subst("        &1.&2 =  'aa'",pcTemptablename,pcFieldname).
     //    cLong = replace(pCode:Code,"RowObject.",pcTempTableName + ".").
         // end will be added during serialization */
      //   cLong = replace(cLong,"end procedure.","").
         
         // TODO Convert RETURN "some error" to throw - check for quotes and replace with undo, throw
         return cLong + StringConstant:LF.              
    end method.    
    
    method protected final logical ConvertHooks(
                                          pClassModel as ClassModel,  
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
                
                oMethod:code = ConvertHook(pClassModel,cMappedName,oCode,pcTempTableName,pcBeforeTableName,pcOriginalTempTableName,pcOrignalBeforeTableName,pErrorTracker).
                // beginTransactionValidate may have both - TODO clean up - should not be handled here 
                if type-of(pModel,DlpModel) and oCode:name = "beginTransactionValidate" and pcOriginalTempTableName = "RowObjUpd" then
                do:         
                    oMethod:code = replace(oMethod:code,subst("b_&1",cast(pModel,DlpModel):DataLogicTable),pcTempTableName).
                end.
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
    
    method protected logical ApplicationReplaceInclude(
        input pcCode as longchar,
        input pClassModel as ClassModel,
        input pcTempTableName as char,
        input pcBeforeTableName as char, 
     
        input pcIncludeName as character, 
        input pcParameters as character extent,
        input piStart as integer, 
        input piLength as integer,
        output piPosition as integer,
        output pcAssignVariable as character,
        output pcnewcode as longchar 
    ):
         return false.
    end method.
    
    method protected longchar ReplaceInclude(
        input pClassModel as ClassModel,
        input pcMethodname as character, 
        input pcTempTableName as char,
        input pcBeforeTableName as char, 
        input pccode as longchar,
        input pcIncludeName as character, 
        input pcParameters as character extent,
        input piStart as integer, 
        input piLength as integer,
        output piPosition as integer,
        output pcAssignVariable as character
    ):
        define variable cReplace            as character no-undo.
        define variable iAppNewPos as integer no-undo.
        define variable cAppVar    as character no-undo.
        define variable cappLong   as longchar no-undo.
        case pcIncludeName: 
            // has no purpose 
            when "get" then 
            do:
                if pcParameters[1] = "AsDivision" then
                do: 
                    substring(pcCode,piStart,piLength) = "".
                    pcAssignVariable =  pcParameters[2].
                    piPosition = piStart + 1.
                    return pcCode.
                 //   piPosition = piStart + piLength.
                end.
                else do:
                    if ApplicationReplaceInclude(
                         pcCode, 
                         pClassModel, 
                         pcTempTableName, 
                         pcBeforeTableName, 
                         pcIncludeName,  
                         pcParameters, 
                         piStart, 
                         piLength, 
                         output iAppNewPos,
                         output cAppVar,
                         output cApplong) then
                     do:
                         if iAppNewPos > 0 then 
                         do: 
                             pcAssignVariable = cAppvar.
                             piPosition = iaPPnewPos.
                             return cApplong.
                         end.
                         else do:
                             piPosition = piStart + 1.
                             return pccode.
                         end.       
                     end.
                     else  
                          message Subst("Include ~{get &1 in new method &2 must be removed manually", pcParameters[1],pcMethodname)
                          view-as alert-box.
                               
                end.
            end.
            when "getAppservice.i" then 
            do:
                substring(pcCode,piStart,piLength) = "".
               // piPosition = piStart + piLength.
            end.
            when "aferrortxt.i" or when "af/sup2/aferrortxt.i" then
            do:
                FindStatementStart(input pcCode, input-output piStart, input-output piLength, output pcAssignVariable).
                cReplace = GetErrorCall(pClassModel, 
                                       if pcMethodname  matches "*delete*" 
                                       then pcBeforeTableName
                                       else pcTempTableName, 
                                       pcParameters).
              // if type-of(pClassModel,BusinessEntityModel) then 
                //   cReplace = subst("BufferError(buffer &1:handle,'&2').",pcTempTableName,pcParameters[2]).
             //  else    
               //    cReplace = subst("BufferError('&1').",pcParameters[2]).
                        
                substr(pcCode,piStart,piLength + 1) = cReplace.
                piPosition = pistart + length(cReplace).
                return pcCode.
            end. //  when "aferrortxt.i" or when "af/sup2/aferrortxt.i" then
            otherwise do:
                if ApplicationReplaceInclude(
                         pcCode, 
                         pClassModel, 
                         pcTempTableName, 
                         pcBeforeTableName, 
                         pcIncludeName,  
                         pcParameters, 
                         piStart, 
                         piLength, 
                         output iAppNewPos,
                         output cAppVar,
                         output cApplong) then
                     do:
                         // allow suppress of error for cases handled separately 
                         // old subclass may have special code to remvoe some of thede
                         if iAppNewPos > 0 then 
                         do: 
                             pcAssignVariable = cAppvar.
                             piPosition = iaPPnewPos.
                             return cApplong.
                         end. 
                         else do:
                             piPosition = piStart + 1.
                             return pcCode.
                         end.      
                     end.
                     else  
                          message Subst("Include &1 in new method &2 must be removed manually", pcIncludeName,pcMethodname)
                          view-as alert-box.
                    
            end.    
        end.     
        piPosition = pistart + piLength.
        return pccode. 
    end method.    

    method protected character GetErrorCall(input poClassModel as ClassModel,pcTempTableName as character,input pcParameters as character extent): 
        define variable cCall as character no-undo.
        define variable i as integer no-undo.
        if pcParameters[1] = ? then 
        do:
            if type-of(poClassModel,BusinessEntityModel) then 
                return subst("BufferError(buffer &1:handle,'&2','&3').",
                             pcTempTableName,
                             pcParameters[2],
                             if extent(pcParameters) > 3 
                             then if pcParameters[4] = ? then "" 
                                  else pcParameters[4]
                                  else ""
                             ).
            else DO:    
                 
                return subst("BufferError(&1,'&2').",
                             // hack need to manage this in the paramyaer extraction
                             if index(pcParameters[2]," ") > 0  then quoter(pcParameters[2],"'") else pcParameters[2],
                             
                             if extent(pcParameters) > 3 
                             then if pcParameters[4] = ? then "" 
                                  else pcParameters[4]
                                  else ""
                             
                             ).
            
           
             END.  
        end.
        else do:
            if type-of(poClassModel,BusinessEntityModel) then 
                cCall = subst("BufferError(buffer &1:handle,'&2',&3",pcTempTableName,pcParameters[1],pcParameters[2]).
            else    
                cCall = subst("BufferError('&1',&2",pcParameters[1],pcParameters[2]).
            // param 3 is table - should not be needed since we always know    
            do i = 4 to extent(pcParameters):
                if i = 4 then 
                    cCall = cCall + "," + if pcParameters[i] = ? then "''" else quoter(pcParameters[i]).
                else
                    cCall = cCall + "," + quoter(pcParameters[i]).
            end.
            return subst("&1).",cCall). 
        end.    
    end.
    
    method protected longchar ConvertHook(pClassModel as ClassModel,
                                          pMethodName as char, /* for override error log */
                                          pCode as CodeModel, 
                                          pcTempTableName as char,
                                          pcBeforeTableName as char, 
                                          pcOriginalTempTableName as char, 
                                          pcOriginalBeforeTableName as char,
                                          pErrorTracker as ErrorTracker):
                                              
         define variable cLong         as character no-undo.
      
         cLong = replace(pCode:Code,pcOriginalTempTableName,pcTempTableName).
         if pcBeforeTableName  > "" and pcOriginalBeforeTableName > "" then
             cLong = replace(cLong,pcOriginalBeforeTableName,pcBeforeTableName).
         // end will be added during serialization */
        
         cLong = replace(cLong,"end procedure.","").
         if index(cLong,"isAdd") > 0 or index(cLong,"isCopy") > 0 then
            cLong = replaceIsAddAndCopy(pMethodname,cLong,pcTempTableName,pErrorTracker).
         if index(cLong,"isCreate") > 0 then
            cLong = replaceIsCreate(pMethodname,cLong,pcTempTableName,pErrorTracker).
         
         if index(cLong,".RowMod") > 0 then
            cLong = replaceRowMod(pMethodname,cLong,pcTempTableName,pErrorTracker).
         
         if index(cLong,"~{") > 0 then 
             cLong = ReplaceIncludes(pCLassmodel,pMethodname, pcTempTableName, pcBeforeTableName,cLong, pErrorTracker).
         
         if index(cLong,"dynamic-function") > 0 then 
             cLong = ReplaceDynamicFunctions(pCLassmodel,pMethodname, cLong, pErrorTracker).
             
         if UseForEach(pMethodname) then
             cLong = AddForEach(cLong, pcTempTableName,pcOriginalTempTableName).
         // TODO Convert RETURN "some error" to throw - check for quotes and replace with undo, throw
         return cLong.    
         catch e as Progress.Lang.Error :
             undo, throw new ApplicationError(subst("Failed to parse &1 in &2 due to error: &3.",pMethodName,pCode:name,e:GetMessage(1))).    
         end catch.          
    end method.    
    
    method protected longchar ConvertFieldValidation(
                                          pClassModel as ClassModel,
                                          pMethodName as char, /* for override error log */
                                          pCode as CodeModel, 
                                          pcTempTableName as char,
                                          pcBeforeTableName as char, 
                                          pcOriginalTempTableName as char, 
                                          pcOriginalBeforeTableName as char,
                                          pErrorTracker as ErrorTracker,
                                          output pcParameter as character):
                                              
         define variable cLong   as character no-undo.
         define variable cLong2   as character no-undo.
         define variable cLine   as character no-undo.
         define variable i as integer no-undo.
         
         define variable lBlank as logical no-undo.
         define variable lLastBlank as logical no-undo.
         cLong = replace(pCode:Code,pcOriginalTempTableName,pcTempTableName).
         if pcBeforeTableName  > "" and pcOriginalBeforeTableName > "" then
             cLong = replace(cLong,pcOriginalBeforeTableName,pcBeforeTableName).
         // end will be added during serialization */
        
         cLong = replace(cLong,"end procedure.","").
        // if index(cLong,"isAdd") > 0 or index(cLong,"isCopy") > 0 then
          //  cLong = replaceIsAddAndCopy(pMethodname,cLong,pcTempTableName,pErrorTracker).
        // if index(cLong,"isCreate") > 0 then
          //  cLong = replaceIsCreate(pMethodname,cLong,pcTempTableName,pErrorTracker).
         
        // if index(cLong,".RowMod") > 0 then
          //  cLong = replaceRowMod(pMethodname,cLong,pcTempTableName,pErrorTracker).
         // temporary hack
         cLong = replace(cLong,"RETURN .","").
         cLong = replace(cLong,"RETURN.","").
         cLong = replace(cLong,"RETURN","").
         cLong = replace(cLong,"THEN","THEN = ").
         
         if index(cLong,"~{") > 0 then 
             cLong = ReplaceIncludes(pCLassmodel,pMethodname, pcTempTableName, pcBeforeTableName,cLong, pErrorTracker).
        
         lineloop:
         do i = 1 to num-entries(cLong,StringConstant:LF): 
             cLine = string(entry(i,cLong,StringConstant:LF)).
             
             if cLine > "" then
             do:
                 lBlank = false.
                 if left-trim(cLine) matches "DEF*PARAMETER*" then 
                 do: 
                     pcParameter = entry(4,trim(cLine)," ").
                     next lineloop. 
                 end.
                 
             end.
             else lBlank = true.
             if lBlank = false or lLastBlank = false then 
                 cLong2 = clong2 + cline + StringConstant:LF.
             lLastBLank = lBLank. 
         end.            
         return cLong2.              
    end method.  
    
    method protected longchar AddForEach(pcLong as longchar, pcTempTableName as character, pcOriginalTempTableName as char): 
                                          
        define variable i     as integer no-undo.
        define variable cLine as character no-undo.
        define variable lComment as logical no-undo init ?.
        define variable lDef as logical no-undo.
        define variable lForeachadded as logical no-undo.
        do i = 1 to num-entries(pcLong,StringConstant:LF):
            cLine = entry(i,pcLong,StringConstant:LF).
            if cLine > "" then
            do:
                // set comment flag if first encountered line is comment
                if lComment = ? then 
                do:
                   if left-trim(cLine) begins "/*" then 
                      lComment = true.
                   else 
                     lComment = false.
                   
                end.
                
                if lComment then
                do:
                   if index(cLine,pcOriginalTempTableName) > 0 then
                   do:
                       cLine = replace(cline,pcOriginalTempTableName,pcTemptablename). 
                       entry(i,pcLong,StringConstant:LF) = cLine.
                   end.   
                   // we know that adm2 rowobjectValidate has this inm the cmment
                   if index(cLine,"RowObject ") > 0 then
                   do:
                       cLine = replace(cline,"RowObject ",pcTemptablename + " "). 
                       entry(i,pcLong,StringConstant:LF) = cLine.
                   end.   
                   // we know that adm2 rowobjectValidate has this inm the cmment
                   if index(cLine,"Procedure ") > 0 then
                   do:
                       cLine = replace(cline,"Procedure ","Method "). 
                       entry(i,pcLong,StringConstant:LF) = cLine.
                   end.   
                    
                   if trim(cLine) matches "~*/" then
                       lComment = false.
                     
                end.
                else do:
                    //keep def as is if for eahc has not been added yet
                    if lforEachadded = false and lookup(entry(1,left-trim(cline),""),"def,defi,defin,define") > 0 
                    and right-trim(cline) matches "*~." then
                    do: 
                        //                                        
                    end. 
                    else do:
                        if lforeachadded = false then
                        do:
                            entry(i,pcLong,StringConstant:LF) = (if opsys = "unix" then StringConstant:LF else StringConstant:CRLF)  
                                                               + subst("    for each &1:",pcTempTableName)
                                                               + (if opsys = "unix" then StringConstant:LF else StringConstant:CRLF). 
                            lforEachadded  = true.  
                            i = i + 2.
                          
                        end.
                        entry(i,pcLong,StringConstant:LF) = "        " + cLine.
                    end.
                end.    
            end.
         end.
         pcLong = right-trim(pcLong,(if opsys = "unix" then StringConstant:LF else StringConstant:CRLF)).
         pcLong = pcLong              
                + (if opsys = "unix" then StringConstant:LF else StringConstant:CRLF)  
                + "    end. //for each"
                + (if opsys = "unix" then StringConstant:LF else StringConstant:CRLF).
                     
         return pcLong.
    end method.     
    
    method protected void FindStatementStart(
        input pcCode as longchar,
        input-output piStart as integer,
        input-output piLength as integer,
        output pcAssingedVariable as character):
        
        define variable iStatementStart as integer no-undo.
        define variable cPrev           as character no-undo.
        define variable cChar           as character no-undo.
        define variable iChar           as integer no-undo.
        define variable lEquals         as logical no-undo.
        define variable iLastPrintable  as integer no-undo.
        define variable cLastCharprint  as character no-undo. 
        define variable iComment        as integer no-undo.
        define variable lIgnoreNext     as logical no-undo.
        define variable lEqualsFound as logical no-undo.
        define variable cCharBeforeThen as character no-undo.
        
        // looping backwards is a rather crude way to do this.. can definitely be improved 
        do iChar = piStart to 1 by -1:
            if lIgnoreNext then 
            do:
                lIgnoreNext = false.
                next.
            end.    
            cChar = substr(pcCode,ichar,1).
            if iComment = 0 and ccHar = "/" then
            do:
                if ichar > 1 and substr(pcCode,ichar - 1,1) = "*" then
                do:
                   iComment = iComment + 1.
                   lIgnoreNext = true.
                end.    
            end. 
            if iComment <> 0 then 
            do:
                if cChar = "*" then
                do:  
                    if ichar > 1 and substr(pcCode,ichar - 1,1) = "/" then
                    do:
                       iComment = iComment - 1.
                       lIgnoreNext = true.
                    end.
                end.
            end.
            else do:   
                if lEqualsFound and cchar = "N" then 
                do:
                    if iChar > 4 and substr(pcCode,ichar - 3,4) = "THEN" then
                    do:
                        if iChar > 5 then
                            cCharBeforeThen = substr(pcCode,ichar - 4,1).
                        else 
                            cCharBeforeThen = "".    
                        
                        if cCharBeforeThen = "" 
                        or cCharBeforeThen = StringConstant:CR
                        or cCharBeforeThen = StringConstant:LF then
                        do: 
                            iStateMentStart = iChar + 1.
                            leave.
                        end.
                    end.
                end.    
                
                if cchar = ":" or cchar = "." then 
                do:
                    if cPrev = "" 
                    or cPrev = StringConstant:CR
                    or cPrev = StringConstant:LF then
                    do: 
                        iStateMentStart = iChar + 1.
                        leave.
                    end.                
                end.    
                
                
                if lEquals then
                do: 
                    if cChar = "" then 
                    do:
                        if pcAssingedVariable > "" then 
                            lEquals = false.
                    end.
                    if cChar > "" and lEquals then
                        pcAssingedVariable = cCHar + pcAssingedVariable.            
                end.
                else if cchar = "=" then 
                do:
                   lEquals = true.
                   lEqualsFound = true.
                end.
                if  cChar <> "" 
                and cChar <> StringConstant:CR
                and cChar <> StringConstant:LF 
                and cchar <> ":" 
                and cchar <> "."  then
                do:
                    iLastPrintable = iChar.
                    cLastCharprint = cChar.
                end.
                cPrev = CChar.
            end.
        end.
        if iStatementStart > 0 then
        do: 
            piLength = piLength + (piStart - iLastPrintable). 
            piStart = iLastPrintable. 
        end.
        else if iLastPrintable > 0 then
        do: 
            piLength = piLength + (piStart - iLastPrintable). 
            piStart = iLastPrintable. 
        end.      
    end method.         
    
    method protected integer FindStatementEnd(
        input pcCode as longchar,
        input piStart as integer):
        
        define variable cPrev           as character no-undo.
        define variable cChar           as character no-undo.
        define variable iChar           as integer no-undo.
        define variable cNext           as character no-undo.
        define variable cLastCharprint  as character no-undo. 
        define variable iComment        as integer no-undo.
        define variable lIgnoreNext     as logical no-undo.
        
        define variable iLength as int64 no-undo.
        iLength = length(pcCode).
        // looping backwards is a rather crude way to do this.. can definitely be improved 
        do iChar = piStart to ilength:
            if lIgnoreNext then 
            do:
                lIgnoreNext = false.
                next.
            end.    
            cChar = substr(pcCode,ichar,1).
            
            if iComment = 0 and ccHar = "/" then
            do:
                if ichar lt iLength and substr(pcCode,ichar + 1,1) = "*" then
                do:
                   iComment = iComment + 1.
                   lIgnoreNext = true.
                end.    
            end. 
            if iComment <> 0 then 
            do:
                if cChar = "*" then
                do:  
                    if ichar lt iLength and substr(pcCode,ichar + 1,1) = "/" then
                    do:
                       iComment = iComment - 1.
                       lIgnoreNext = true.
                    end.
                end.
            end.
            else if cChar = "." or CChar = ":" then 
            do:   
                if iChar lt iLength then 
                do:
                    cNext = substr(pcCode,ichar + 1,1).
                    if cNext = "" 
                    or cNext = StringConstant:CR
                    or cNext = StringConstant:LF then
                        return iChar.
                end.
                else 
                    return iChar. 
            end.
            cPrev = CChar.
        end.
        return ?.   
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
    
    method protected longchar ReplaceRowMod(pMethodname as char, pcLong as longchar,pcTablename as char,pErrorTracker as ErrorTracker):
        define variable iFound as integer no-undo.
        define variable iNewFound as integer no-undo.
        define variable ldone     as logical no-undo.
        define variable iLength as integer no-undo.
        define variable cChunk as longchar no-undo.
        define variable iSpace as integer no-undo.
        define variable cWord as character no-undo.
        define variable icando as integer no-undo.
        define variable cExpression as character no-undo.
        define variable cReplace as character no-undo.
        define variable coperator as character no-undo.
        define variable cRowmodvalue as character no-undo.
        define variable iExcept as integer no-undo.
        define variable lCommaBefore as integer no-undo.
        define variable iCommaAfter as integer no-undo.
        define variable ichar as integer no-undo.
        define variable cChar as character no-undo.
        define variable cPrev as character no-undo.
        define variable lBlankfound as logical no-undo.
        define variable iStatementend as integer no-undo.
        define variable iWordCount as integer no-undo.
        define variable iTo as integer no-undo.
        iFound = 1.
        
        bigloop:
        do while true:
            iNewFound = index(pcLong,".RowMod",iFound).
            icando = index(pcLong,"CAN-DO",iFound).
            
            iExcept = index(pcLong,"EXCEPT",iFound).
            coperator = "".
            cReplace = "".
             
            if iNewFound > 0 and iCando > 0 and iCando lt iNewFound and (iExcept = 0 or iExcept gt ICanDO) then 
            do:
                cExpression = StringUtil:ExtractParenthesis(substring(pcLong,iCando + 5)).
                cReplace = ConvertCanDoRowMod(cExpression,pcTablename).
                if cReplace > "" then
                do:
                    substr(pcLong,iCando,length(cExpression) + 6) = cReplace.
                    iFound = InewFound + 1.
                    next bigloop.
                end.             
            end.
            if iNewFound > 0 and iExcept > 0 and iExcept lt iNewFound  then 
            do:
               if iNewFound > 1 then
               do: 
                   // make sure this is excpet and not part of something else
                   cPrev = substr(pcLong,iNewFound - 1,1).
                   if cPrev <> ""  
                   or cPrev <> StringConstant:LF
                   or cPrev <> StringConstant:CR then
                   do:
                       iStatementend = FindStatementEnd(pcLong,iExcept).
                       
                       iWordCount = 0.
                       cChunk = substr(pcLong,iExcept,iStatementend - iExcept + 1).
                       iTO = index(cChunk," TO ").
                       if iTO = 0 then 
                          iTO = index(cChunk,subst(" TO&1",StringConstant:LF)).
                       if iTO = 0 then 
                          iTO = index(cChunk,subst(" TO&1",StringConstant:CR)).
                       
                       if iTO > 0 then 
                       do:
                           do iSpace = 2 to num-entries(cChunk,StringConstant:SPACE):
                               cWord = entry(iSpace,cChunk,StringConstant:SPACE).
                               if cWOrd = "TO" then
                                   leave.
                               if cWord > "" then 
                               do:
                                   if cWord <> ","
                                   and trim(entry(num-entries(cWord,"."),cWord,".")) <> "Rowmod" then
                                   do: 
                                       cReplace = cReplace 
                                             + (if iWordCount > 1 then ", " else "EXCEPT ")
                                             + cWord.  
                                       
                                       iWordCount = iWordCount + 1.
                                       
                                   end.      
                               end. 
                           end.
                           
                           substr(pcLong,iExcept,iTO) = cReplace + " ".
                              
                              
                           iFound = InewFound + 1.
                       end.
                       next bigloop.   
                   end. 
                end.
            end.
            if iNewFound > 0 then 
            do:
                lDone = false.
                cChunk = substr(pcLong,inewFound - length(pcTablename)).
                coperator = "".
                cRowModValue = "".
                wordloop: 
                do iSpace = 1 to num-entries(cChunk,StringConstant:SPACE):
                    cWord = entry(iSpace,cChunk,StringConstant:SPACE).
                    if lookup(cWord,"=,eq,<>,ne") > 0 then
                    do: 
                        cOperator = cWord.
                    end.
                    else if coperator > "" then 
                    do:   
                        if cWord begins "'" or cWord begins '"' then
                        do: 
                            cRowModValue = StringUtil:UnquoteValue(cWord).
                        end.       
                    end.
                  //  if cWord <> StringConstant:LF and cWord <> StringConstant:CRLF then
                 //   do:
                    iLength = iLength + (length(cWord) + 1).
                    iFound = iFound + iLength.
                    if cRowModValue > "" then
                    do: 
                        substr(pcLong,iNewFound - length(pcTableName),iLength - 1) 
                            = subst("row-state(&1) &2 &3",
                                     pcTableName,
                                     coperator,
                                     if cRowModValue = "C" or cRowModValue = "A"  then "row-created" else "row-modified").
                        leave wordloop.
                    end.                                                       
                end.
            end.
            
            else  
                leave.
                
         end.
         return pcLong.
        
        
    end method.
    
    method protected longchar ReplaceIscreate(pMethodname as char, pcLong as longchar,pcTablename as char,pErrorTracker as ErrorTracker):
        define variable iFound as integer no-undo.
        define variable iNewFound as integer no-undo.
        
        define variable cReplace as character no-undo.
         
        iFound = 1.
        
        cReplace = subst("row-state(&1) = row-created",pcTablename).
         
        bigloop:
        do while true:
            iNewFound = index(pcLong,"isCreate()",iFound).
            if iNewFound > 0 then 
            do:
                substr(pcLong,iNewFound,length("isCreate()")) = cReplace.
                iFound = inewFound + Length("isCreate()"). 
            end.
            
            else  
                leave.
                
         end.
         return pcLong.
        
        
        
    end method.     
    
    method protected longchar ReplaceDynamicFunctions(
        pClassModel as ClassModel,
        pcMethodName as char, /* for override error log */
        pcLong as longchar,
        pErrorTracker as ErrorTracker
       ):
           define variable iFound    as integer no-undo.
           define variable iNewFound as integer no-undo.
           define variable cFunction as character no-undo.
           define variable cFunctionExpression as character no-undo.
           define variable iRightParenthesis as integer no-undo.
           define variable cReplace as character no-undo.
           iFound = 1. 
          
           do while true:
               iNewFound = index(pcLong,"dynamic-function",iFound).
               if iNewFound > 0 then
               do:
                   cFunctionExpression = StringUtil:ExtractParenthesis(substr(pcLong,iNewFound),true).
                   if cFunctionExpression matches "*getPropertyList*IN*gshSessionManager*currentOrganisationname*" then  
                   do:
                       cReplace = "this-object:SessionManager:CompanyName".
                   end.
                           
                   
                   iRightParenthesis = index(pcLong,")",iNewFound).
                   if cReplace > "" then
                       substr(pcLong,iNewFound,(iRightParenthesis + 1) - iNewFound) = cReplace.
                   iFound = inewFound + iRightParenthesis.
                    
                //   message string(substr(pcLong,iNewFound,(iRightParenthesis + 1) - iNewFound))
                 //  view-as alert-box.
               end.     
               else 
                  leave.
            
          end.
          return pcLong.
    end method.
    
    method protected longchar ReplaceIncludes(
        pClassModel as ClassModel,
        pcMethodName as char, /* for override error log */
        pcTempTableName as char,
        pcBeforeTableName as char, 
        pcLong as longchar,
        pErrorTracker as ErrorTracker
     ):
        
        define variable iFound as integer no-undo.
        define variable iNewFound as integer no-undo.
        define variable cTokens as character extent no-undo.
        define variable cIncludeName as character no-undo.
        define variable iStart as integer no-undo.
        define variable iLength  as integer no-undo.
        define variable iReplacedPosition as integer no-undo.
        define variable cRemoveVariableList as character no-undo.
        define variable cRemoveVariable as character no-undo.
        iFound = 1.
        
        bigloop:
        do while true:
            iNewFound = index(pcLong,"~{",iFound).
             
            if iNewFound > 0 then 
            do:
                extent(ctokens) = ?.
                ParseInclude(input substring(pcLong,iNewFound),output cIncludeName, output cTokens, output iStart, output iLength).
               
                pcLong = ReplaceInclude(pClassModel,
                                        pcMethodname,
                                        pcTempTableName, 
                                        pcBeforeTableName,
                                        pcLong, 
                                        cIncludeName, 
                                        cTokens, 
                                        iNewFound, 
                                        iLength,
                                        output iReplacedPosition,
                                        output cRemoveVariable 
                                        ).
           
                iFound = iReplacedPosition.
                if cRemoveVariable > "" and lookup(cRemoveVariable,cRemoveVariableList) = 0 then
                    cRemoveVariableList = StringUtil:AppendEntry(cRemoveVariableList, cRemoveVariable). 
            end.
            else
                leave bigloop.
         end.
         
         if cRemoveVariableList > "" then 
             pcLong = RemoveVariables(input pClassModel,input pcLong, input cRemoveVariableList, input pcMethodName).
         
         
         return pcLong.
    end method.
    
    method protected longchar ReplaceMessages(
        pClassModel as ClassModel,
        pcMethodName as char, /* for override error log */
        pcTempTableName as char,
        pcBeforeTableName as char, 
        pcLong as longchar,
        pErrorTracker as ErrorTracker
     ):
        
        define variable iFound as integer no-undo.
        define variable iNewFound as integer no-undo.
        define variable cTokens as character extent no-undo.
        define variable cIncludeName as character no-undo.
        define variable iStart as integer no-undo.
        define variable iLength  as integer no-undo.
        define variable iReplacedPosition as integer no-undo.
        define variable cRemoveVariableList as character no-undo.
        define variable cRemoveVariable as character no-undo.
        iFound = 1.
      
        bigloop:
        do while true:
            iNewFound = index(pcLong,"message",iFound).
             
            if iNewFound > 0 then 
            do:
                extent(ctokens) = ?.
                
                ParseMessage(input substring(pcLong,iNewFound),output cIncludeName, output cTokens, output iStart, output iLength).
               
                pcLong = ReplaceInclude(pClassModel,
                                        pcMethodname,
                                        pcTempTableName, 
                                        pcBeforeTableName,
                                        pcLong, cIncludeName, 
                                        cTokens, 
                                        iNewFound, 
                                        iLength,
                                        output iReplacedPosition,
                                        output cRemoveVariable 
                                        ).
                iFound = iReplacedPosition.
                
                if cRemoveVariable > "" and lookup(cRemoveVariable,cRemoveVariableList) = 0 then
                    cRemoveVariableList = StringUtil:AppendEntry(cRemoveVariableList, cRemoveVariable). 
            end.
            else
                leave bigloop.
         end.
         if cRemoveVariableList > "" then 
             pcLong = RemoveVariables(input pClassModel,input pcLong, input cRemoveVariableList, input pcMethodName).
         
         return pcLong.
    end method.
    
    // this is a rudimentary prototype
    method protected logical RemoveVariableFromStatement(
        input pcVariablename  as character,
        input pcStatement     as character,
        output pcNewStatement as character 
    ):
        define variable cWord as character no-undo.
        define variable iWord as integer no-undo.
        define variable cNewStatement as character no-undo.
        define variable cMaskedStatement as character no-undo.
        define variable iState as integer no-undo.
        define variable lOk as logical no-undo.
        do iWord =  1 to num-entries(pcStatement,""):
            cWord = entry(iWord,pcStatement,"").
            
            if cWord > "" then 
            do:
                if iState = 0 then 
                do:
                    if cWord = pcVariablename then
                        iState = 1.
                    else 
                        pcNewStatement = pcNewstatement + " " + cWord.
                end.
                else
                do: 
                    if iState = 1 then 
                    do:
                        if lookup(cWord,"=,<>,>=,<=,begins,matches") > 0 then 
                            iState = 2.
                        else do:
                          
                            return false.
                        end.     
                    end.
                    else if iState = 2 then
                    do: 
                        iState = 3.
                        lok = true.
                    end.
                    else if iState = 3 then
                    do: 
                        if cWord = "AND" or cWOrd = "OR" then 
                        do:
                            iState = 0.
                            lOk = true.
                        end. 
                        else lok = false.     
                    end.        
                end.
                       
            end.
            else
                pcNewStatement = pcNewstatement + " ".
           
        end.  
        return lok.
    end. 
    
    method protected longchar RemoveVariables(
        input pcClass              as ClassModel,
        input pcLong               as longchar,
        input pcRemoveVariableList as character,
        input pcMethodname         as character
    ):
        define variable iVar as integer no-undo.
        
        define variable iFound as integer no-undo.
        define variable cNextChar as character no-undo.
        define variable cRemoveVariable as character no-undo.
        define variable iNewFound as integer no-undo.
        define variable pcAssingedVariable as character no-undo.
        define variable iLength as integer no-undo.
        define variable iStart as integer no-undo.
        define variable iStatementend as integer no-undo.
        define variable iCodeLength as integer no-undo.
        define variable cStatement as character no-undo.
        define variable cFirstWord as character no-undo.
        define variable cNewstatement as character no-undo.
        define variable iNewFound2 as integer no-undo.
      
        define variable cREplace as character no-undo.
        varloop:
        do iVar =  1 to num-entries(pcRemoveVariableList):   
             cRemoveVariable = entry(iVar,pcRemoveVariableList).
             iFound = 1.
        
             do while true:
                 iNewFound  = index(pcLong,subst(" &1",cRemoveVariable),iFound).
                 iNewFound2 = index(pcLong,subst("(&1",cRemoveVariable),iFound).
                 if iNewFound2 > 0 and (iNewFound = 0 or iNewFound2 lt iNewFound) then
                    iNewFound = iNewfound2.
                 
                 if iNewFound > 1 then
                 do:
                     cNextChar = substr(pcLong,iNewFound + length(cRemoveVariable) + 1,1).
                     
                     cStatement = "".
                     if cNextChar = "."
                     or (inewFound2 = iNewFound and cNextChar = ")")
                     or cNextChar = ""
                     or cNextChar = StringConstant:LF
                     or cNextChar = StringConstant:CR then
                     do: 
                         iStart = iNewFound.
                         iLength = length(cRemoveVariable).
                         do on error undo, throw:
                             FindStatementStart(pcLong, input-output iStart, input-output iLength, output pcAssingedVariable).
                             catch e as Progress.Lang.Error :
                                    message e:CallStack
                                    view-as alert-box. 
                                  undo, throw e.   
                             end catch.
                         end.                             
                         if cNextChar = "." then 
                         do:
                             iStatementend = iNewFound + length(cRemoveVariable) + 1.
                             cStatement = substr(pcLong,iStart,(iStatementend - iStart) + 1).
                             iFound = iStatementend - iStart.
                         end.    
                         else if iStart lt iNewFound then
                         do:
                             iStatementend = FindStatementEnd(pcLong,iNewFound + iLength). 
                             cStatement = substr(pcLong,iStart,(iStatementend - iStart) + 1).
                             iFound = iStatementend - iStart.
                             
                         end.
                         
                         if cStatement > "" then 
                         do: 
                              
                             cFirstWord = entry(1,trim(cStatement),"").
                             if cfirstWord = "return" 
                             or lookup(cFirstWord,"def,defi,defin,define") > 0 then
                             do: 
                                 // TODO remove CR or LF 
                                 substr(pcLong,iStart,(iStatementend - iStart) + 1) = "".
                                 iFound = iStart + 1.
                                 next.
                             end.
                             else if cRemoveVariable = "gshAppserver" and cfirstWord = "if" and cStatement matches "*valid-handle*" then
                             do:
                                 substr(pcLong,iStart,(iStatementend - iStart) + 1) = "".
                                 iFound = iStart + 1.
                                 next.

                             end.
                             else if cRemoveVariable = "gshAPpserver" and cfirstWord = "run" and cStatement matches subst("*on*server*&1*",cRemovevariable) then
                             do:
                                 define variable cProgname as character no-undo. 
                                 cProgname = entry(2,replace(cStatement,"  "," "),""). 
                                 cReplace = replace(cStatement," on ","").
                                 cReplace = trim(replace(cReplace,"server ","")).
                                 cReplace = trim(replace(cReplace,subst("&1",cRemoveVariable)," ")).
                                 cReplace = subst("undo, throw new NotImplementedError(~"API replacement for run &1~").",cProgname)
                                 
                                          + subst("~n~n      /******~n       &1~n    *****/",replace(cReplace,"~n","~n       ")).
                                          
                                 substr(pcLong,iStart,(iStatementend - iStart) + 1) = cReplace.
                                 iFound = iStart + length(cReplace).
                                 pcClass:AddUsing("Pmfo.core.Error.NotImplementedError").
                                 next.

                             end.
                             
                             else do:
                                 if RemoveVariableFromstatement(cRemoveVariable,cStatement,output cNewstatement) then  
                                 do:
                                     substr(pcLong,iStart,(iStatementend - iStart) + 1) = cnewStatement.
                                     iFound = iStart + length(cNewstatement) +  1.
                                     
                                     
                                 end.    
                                 else 
                                      message subst("Cannot remove reference to variable '&1' automatically. The statement below in the new '&2' method must be edited manually:~n~n&3",
                                                    cRemoveVariable,
                                                    pcMethodname,
                                                    cStatement
                                                    )
                                      view-as alert-box warning.
                                       
                             end.     
                         end.      
                     end.
                     iFound = iNewFound + 1.
                 end.
                 else 
                     leave. 
             end.
         end.
         return pcLong.
    end method.
    
    method protected longchar RemoveReturn(
        input pcClass              as ClassModel,
        input pcLong               as longchar,
        input pcMethodname         as character
    ):
         define variable iFound    as integer no-undo.
         define variable iNewFound as integer no-undo.
         define variable iStatementEnd as integer no-undo.
         define variable cReplace  as character no-undo.
         define variable cNextChar as character no-undo.
         
         iFound = 1. 
         do while true:
             iNewFound = index(pcLong," return",iFound).
             
             if iNewFound = 0 then 
                 iNewFound = index(pcLong,"return",iFound).
             else 
                 iNewFound = iNewFound + 1. // move to non-blank
             if iNewFound > 0 // one extra since we searhc for leading blank
             then
             do:
                 cNextChar = substr(pcLong,iNewFound + length("return"),1).
                 if cNextChar = "."
                 or cNextChar = ""
                 or cNextChar = StringConstant:LF
                 or cNextChar = StringConstant:CR then
                 do: 
                     iStatementend = FindStatementEnd(pcLong,iNewFound). 
                     iFound = (iStatementend - iNewFound) + 1.
                     substr(pcLong,iNewFound,iFound) = "".
                 end.
                 else 
                     iFound = iNewFound + length("return"). 
             end.     
             else 
                 leave.
            
          end.
          return pcLong.
    
    end method.
     
    method protected character ConvertCanDoRowMod(pcCandoExpression as character,pcTableName as character ):
        define variable ileftQuote as integer no-undo.
        define variable irightQuote as integer no-undo.
      
        define variable cCanDOFirstString as character no-undo.
        define variable iCandochar as integer no-undo.
        define variable lRowCreated as logical no-undo.
        define variable lRowModified as logical no-undo.
        if index(pcCanDoExpression,".RowMod") > 0 then
        do:
            iLeftquote = index(pcCanDoExpression,'"').
            if iLeftquote > 0 then
                iRightquote = r-index(pcCanDoExpression,'"').
            else do:
                iLeftquote = index(pcCanDoExpression,"'").
                if iLeftquote > 0 then
                    iRightquote = r-index(pcCanDoExpression,"'").
            end.   
            if iLeftQuote > 0 and iRightQuote > iLeftQuote then
            do:
                cCanDOFirstString = substring(pcCanDoExpression,ileftQuote + 1,iRightQuote - 3).
                do iCandochar = 1 to num-entries(cCanDOFirstString):
                    case entry(iCandochar,cCanDOFirstString):
                        when "C" or when "A" then 
                            lRowCreated = true. 
                        when "U" then
                            lRowModified = true.    
                    end case.               
                end.    
                if lRowCreated and lRowModified then 
                    return subst("row-state(&1) gt 0",pcTablename).
                if lRowCreated then
                    return subst("row-state(&1) eq row-created",pcTablename).
                if lRowModified then
                    return subst("row-state(&1) eq row-modified",pcTablename).
           end.     
        end.
    end method.    
end class.