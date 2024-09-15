 
 /*------------------------------------------------------------------------
    File        : BufferUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Apr 18 08:47:10 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Common.FieldValue from propath.
using Pmfo.Core.Common.IFieldValue from propath.
using Pmfo.Core.Common.IKey from propath.
using Pmfo.Core.Common.Key from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.NumberUtil from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Error from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
 
class Pmfo.Util.BufferUtil: 
     // cannot have field that start with _ since index-information throws STOP 
    define public  static  property SeqFieldName as character no-undo init "zz_seq" get. set.
   
    method public static character GetAlreadyExistsMessage (phDataset as handle, pError as Error):
        define variable cTable  as character no-undo.
        define variable cMessage as character no-undo.
        define variable hBuffer as handle no-undo.
        if pError:GetMessageNum(1) <> 132 then
            undo, throw new IllegalArgumentError("Error Message number is not 132").
        cMessage = pError:GetMessage(1).    
        cTable = entry(2,cMessage," ").
        hBuffer = phDataset:get-buffer-handle(cTable) no-error.
        if valid-handle(hBuffer) then
        do:
            entry(2,cMessage," ") = GetRowName(hBuffer,true).
            return FixAlreadyExistsMessage(hBuffer,cMessage).
        end.
        return pError:GetMessage(1). 
    end method.
    
    method private static character FixAlreadyExistsMessage(phBuffer as handle,pcMessage as character):
        define variable iValuePos as integer no-undo.
        define variable i as integer no-undo.
        define variable j as integer no-undo.
        define variable iNumkeys as integer no-undo.
        define variable iWord as integer no-undo.
        define variable cMessage as character no-undo.
        define variable cFieldValueString as character no-undo.
        define variable cMaskValues as character no-undo.
        define variable cFieldValues as character extent no-undo.
        define variable cUniqueIndexes as character extent no-undo.
        define variable cIndexMatches as character no-undo.
        define variable iIndex as integer no-undo.
        define variable cPublicFieldNames as character no-undo.
        define variable lCharValue as logical no-undo.
        define variable lCharType as logical no-undo.
        cMessage = left-trim(replace(pcMessage,"(132)",""),"*"). 
        iValuePos = index(cMessage," already exists with").
        cFieldValueString = right-trim(substr(cMessage,iValuePos + length(" already exists with") + 2),". ").
        cFieldValues = StringUtil:GetWords(cFieldValueString).
        define variable hFld as handle no-undo.
        cUniqueIndexes = GetUniqueIndexes(phBuffer,false).
        do i = 1 to extent(cUniqueIndexes):
            if num-entries(cUniqueIndexes[i]) = extent(cFieldValues) then
                cIndexMatches = cindexMatches
                              + (if CindexMatches = "" then "" else ",")  
                              + string(i).     
        end.
        if num-entries(cIndexMatches) > 1 then
        do:
            IndexLoop:
            do j = 1 to num-entries(cIndexMatches):
                do i = 1 to extent(cFieldValues):
                     
                    hFld = phBuffer:buffer-field(entry(i,cUniqueIndexes[integer(entry(j,cIndexMatches))])).
                    lCharType = hFld:data-type = "character" or hFld:data-type = "longchar".
                    lCharValue = cFieldValues[i] begins "'" or cFieldValues[i] begins '"'.
                    if lCharType <> lCharValue then 
                        next IndexLoop.
                end.
                if iIndex = 0 then 
                    iIndex = integer(entry(j,cIndexMatches)).
                else // more than one match just give up.. 
                    leave indexloop.
            end. 
        end.
        else if num-entries(cIndexMatches) = 1 then
            iIndex = integer(cIndexMatches).
         
        if iIndex > 0 then
        do: 
            cPublicFieldNames = BufferUtil:GetPublicFieldNames(phBuffer, cUniqueIndexes[iIndex]).
            cMessage = entry(1,left-trim(cMessage),"") + " already exists with".
             
            do i = 1 to extent(cFieldValues):
                cMessage = cMessage 
                         +  (if i = 1 then " " else if i = extent(cFieldValues) then " and " else ", ")
                         + subst(" &1: &2",entry(i,cPublicFieldNames),cFieldValues[i]) .
            end.    
        end.
        
 
        return cMessage.
        catch e as Progress.Lang.Error :
            // if anything fails here just return the parameter
            return pcMessage.    
        end catch.
        
    end method.
    
    method public static logical MarkRowState(phbuffer as handle,pState as integer):
        define variable rRowid       as rowid no-undo.
        define variable rBeforeRowid as rowid no-undo.
        define variable lOk          as logical no-undo.
        define variable hBefore      as handle no-undo.
        
        if not phBuffer:avail then 
            undo, throw new IllegalArgumentError("Buffer not available").
        
        if phBuffer:row-state = row-created then
        do:
             rRowid = phBuffer:rowid.
             hBefore = phBuffer:before-buffer.
             if valid-handle(hBefore) then 
             do:
                 rBeforeRowid = phBuffer:before-rowid.
                 hBefore:find-by-rowid(phBuffer:before-rowid).
                 hBefore:accept-row-changes ().
             end. 
             else
                 phBuffer:accept-row-changes().    
             phbuffer:find-by-rowid(rRowid).
        end.     
        lOk = phbuffer:mark-row-state(pState). 
        if lok and valid-handle(hBefore) and hBefore:avail = false then
        do: 
            // @TODO test for all states - is it needed and does it work for modified? 
            // this was added to support convert create to delete 
            // for action - the no-error was added as a precaution 
            // but is probably not necessary 
            // ALSO consider deleting the after record in this case
            hBefore:find-by-rowid(rBeforeRowid) no-error.
            if hBefore:avail and pState = row-deleted then
                phbuffer:buffer-delete no-error. 
        end.    
        return lok.
    end method.
    
    // for internal use by BusinessEntoity:GetKeys for default only TODO - move to singleton so it harder to use
    //  
    method public static character GetInternalKeys (phBuffer as handle):
        return GetInternalKeys(phBuffer,"",false).
    end method.
    
    method private static character GetInternalKeys (phBuffer as handle,pcJoinedFields as char,plPublic as logical):
        define variable hDummy as handle extent no-undo.
        return GetInternalKeys(phBuffer, pcJoinedFields, plPublic, output hDummy).
    end method.     
     
    method private static character GetInternalKeys (phBuffer as handle,pcJoinedFields as char,plPublic as logical, output phfields as handle extent):
        define variable i            as integer   no-undo.
        define variable j            as integer   no-undo. 
        define variable cIndexFields as character no-undo.
        define variable cIndex       as character no-undo.
        define variable iMin         as integer   no-undo.
        define variable iMaxUnknown  as integer   no-undo.
        define variable iNumFields   as integer   no-undo.
        define variable cFldDlm      as character no-undo.
        define variable cKey         as character no-undo.
        define variable cKey2        as character no-undo.
        
        define variable cfield       as character no-undo. 
        define variable cFieldname   as character no-undo.
        define variable cDefault     as character no-undo.
        define variable hField       as handle    no-undo.
        define variable lHasUnknown  as logical   no-undo.
        define variable hFields      as handle    extent no-undo.
        define variable hKeys        as handle    extent no-undo.
        define variable hKeys2       as handle    extent no-undo.
        
        if not valid-handle(phBuffer) or phBuffer:type <> "Buffer" then 
             undo, throw new IllegalArgumentError("Buffer parameter is not valid or not a Buffer").  
        indexloop:
        do while true on error undo, throw:                           
            i = i + 1.                          
            cIndex = phBuffer:index-information(i).
            // no more indexes 
            if cIndex = ? then leave.
            
            // unique flag is on 
            if entry(2,cindex) = "1" then do:
                assign 
                    cFldDlm = ""
                    inumFields = 0
                    cIndexFields = ""
                    lHasUnknown  = false
                    extent(hFields) = ?.
               // loop through fields skip the asc/desc flag    
               do j = 5 to num-entries(cIndex) by 2:
                   cField = entry(j,cindex).
                 
                   hField = phBuffer:buffer-field(cField).
                   if plPublic 
                   // only check public and serialize-hidden on the last entry of a join
                   and (pcJoinedFields = "" or (j + 1 = num-entries(cIndex))) 
                   and hField:serialize-hidden then
                       next indexloop.
                   /* if ? value default - only pick if no other and more than one field
                      Unknown indicates maeans that this is not always unique
                   */   
                   // If this is the only field then trust schema that has this as the first index  
                   if hField:default-string = ? and num-entries(cIndex) > 6 then
                      lHasUnknown = true.
                      
                   cFieldName = if plPublic then hField:serialize-name else hField:name.
                   if lookup(cFieldName,pcJoinedFields) > 0 then
                      next.
                   
                   cIndexFields = cIndexFields 
                                + cFldDLm 
                                + cFieldName. 
                   cFldDlm = ",".
                   iNumFields = inumFields + 1.
                   extent(hfields) = (if extent(hFields) = ? then 0 else extent(hFields)) + 1.
                   hFields[extent(hfields)] = hfield.              
               end.    
               
               if lHasUnknown then
               do:
                   /* keep track of the one with most fields if any value is unknown
                      this seems safer as it in theory is more likely that the key is unique the more fields there are 
                     (the cases that need this are many-to-many relations where the key need to have unknwown 
                      init to avoid FILL error when the values comes from two tables)   
                     this also ensures the first similar index is picked 
                   */
                   if iNumFields gt iMaxUnknown or iMaxUnknown = 0 then
                   do:
                       cKey2          = cIndexFields.
                       iMaxUnknown    = iNumFields.
                       extent(hKeys2) = ?.
                       hKeys2         = hFields.
                   end.
               end.    
               // keep track of the one with fewest fields 
               // this also ensures the first similar index is picked
               else if iNumFields lt iMin or iMin = 0 then
               do:
                   cKey          = cIndexFields.
                   iMin          = iNumFields.
                   extent(hKeys) = ?.
                   hKeys         = hFields.
               end.
            end. 
        end.             
        if cKey > "" then 
        do:
            phFields = hKeys.
            return cKey. 
        end.            
        else do:
            phFields = hKeys2.
            return cKey2.
        end.
    end method.    
    
    // returns the buffer handle of a public child entity/table qualifier as used in filter or expand parts/metalTypes
    method public static handle GetBufferFromUrlQualifier(phParent as handle,pcChildpath as character ):
        define variable i         as integer no-undo.
        define variable cSegments as character extent no-undo.
        define variable cSubpath  as character no-undo.
        define variable hBuffer as handle no-undo.
        
        cSegments = Array:Split(pcChildpath, "/").
        do i = 1 to phParent:num-child-relations:
            if phParent:get-child-relation(i):child-buffer:serialize-name = cSegments[1] then
            do:
                if extent(cSegments) = 1 then
                do:
                    return phParent:get-child-relation(i):child-buffer.
                end.
                else do:
                    cSubPath = pcChildpath.
                    entry(1,cSubPath,"/") = "".
                    cSubPath = left-trim(cSubPath,"/").
                    hBuffer = GetBufferFromUrlQualifier(phParent:get-child-relation(i):child-buffer,cSubPath).
                    if valid-handle(hBuffer) then 
                    do: 
                        return hBuffer.
                    end.        
                end. 
            end. 
        end.
    end method.  
    
    method public static character GetChildrenAsPaths(phBuffer as handle):
        return GetChildrenAsPaths("",phBuffer).
    end method.
    
    // returns the buffer handle of a public child entity/table qualifier as used in filter or expand parts/metalTypes
    method private static character GetChildrenAsPaths(pcParentPath as character,phBuffer as handle):
        define variable i            as integer   no-undo.
        define variable hChildBuffer as handle    no-undo.
        define variable hRelation    as handle    no-undo.
        define variable cAllPaths    as character no-undo.
        define variable cPath        as character no-undo.
        define variable cChildPaths  as character no-undo.
        do i = 1 to phBuffer:num-child-relations:
            hRelation = phBuffer:get-child-relation(i).
            if hRelation:reposition = false then
            do:
                hChildbuffer = hRelation:child-buffer.
                cPath = left-trim(subst("&1/&2",pcParentPath,hChildbuffer:serialize-name),"/").
                cChildPaths = GetChildrenAsPaths(cPath,hChildbuffer).
                cPath = StringUtil:AppendEntry(cPath,cChildPaths).
                cAllPaths = StringUtil:AppendEntry(cAllPaths,cPath).
            end. 
        end.
        return cAllPaths.
    end method.  
    
    // returns the public parnt qualifiers as used in filter or expand parts/metalTypes
    method public static character GetQualifiedBufferName(phBuffer as handle):
        define variable cParentPath as character no-undo.  
        define variable cPath as character no-undo.
        cPath = phBuffer:serialize-name.
        if valid-handle(phBuffer:parent-relation) then
        do:
            cParentPath = GetQualifiedBufferName(phbuffer:parent-relation:parent-buffer).
            if cParentPath > "" then 
                cPath = cParentPath + "/"  + cPAth. 
        end.
        return cPath.
    end method.  
    
    
    // returns a where expression to find children from parent
    method public static character GetJoinExpression(phRelation as handle):
        return GetJoinExpression(phRelation,false).
    end method.
    
    // returns a where expression to find children from parent
    method public static character GetJoinExpression(phRelation as handle, plUseBeforename as logical):
        define variable hChildBuffer  as handle    no-undo.
        define variable hParentBuffer as handle    no-undo.
        define variable cValue        as character no-undo.
        define variable i             as integer   no-undo.
        define variable cExpression   as character no-undo.
        define variable cChildname    as character no-undo.
        
        hParentBuffer = phRelation:parent-buffer.
         
        if not hParentBuffer:avail then
        do:
            undo, throw new IllegalStateError(subst("Parent '&1' not available in GetJoinExpression",hParentBuffer:name)).
        end.
        if not plUseBeforename then 
            cChildName = phRelation:child-buffer:name. 
        else    
            cChildName = phRelation:child-buffer:before-buffer:name. 
               
        do i = 1 to num-entries(phRelation:relation-fields) by 2:
            cValue = hParentBuffer:buffer-field(entry(i,phRelation:relation-fields)):buffer-value.
            cExpression = cExpression 
                   + (if cExpression = "" then "" else " and ")
                   + subst("&1.&2 = '&3'",cChildName,entry(i + 1,phRelation:relation-fields),cValue). 
                   .
        end. 
        return cExpression. 
    
    end method.
    
    method public static character GetParentJoinFields(phRelation as handle):
        return GetParentJoinFields(phRelation,false). 
    end method.
    
    method public static character GetParentJoinFields(phRelation as handle, plPublic as logical):
        define variable cParentJoin as character no-undo.
        define variable cDummy      as character no-undo.
        SplitJoinFields(phRelation, false,output cParentJoin, output cDummy).
        return cParentJoin. 
    end method.
    
    method public static character GetChildJoinFields(phRelation as handle):
        return GetChildJoinFields(phRelation,false). 
    end method.
    
    method public static character GetChildJoinFields(phRelation as handle, plPublic as logical):
        define variable cDummy     as character no-undo.
        define variable cChildJoin as character no-undo.
        SplitJoinFields(phRelation, plPublic,output cDummy, output cChildJoin).
        return cChildJoin. 
    end method.
 
    method public static void SplitJoinFields(phRelation as handle, output pParentFields as character, output pChildFields as character):
        SplitJoinFields(phRelation, false,output pParentFields, output pChildFields). 
    end method.
    
    method public static void SplitJoinFields(phRelation as handle, plPublic as logical,output pParentFields as character, output pChildFields as character):
        define variable iKey as integer no-undo.
        define variable hFld as handle  no-undo.
        define variable cDlm as character no-undo.
        
        // as of current we  are hiding the foreignkey for recursive on the operationhandler
        do iKey = 1 to num-entries(phRelation:relation-fields) by 2:
            assign
                hFld = phRelation:parent-buffer:buffer-field(entry(iKey,phRelation:relation-fields))
                pParentFields = pParentFields
                            + cDlm 
                            + (if plPublic then hFld:serialize-name else hFld:name)
                hFld = phRelation:child-buffer:buffer-field(entry(iKey + 1,phRelation:relation-fields))
                pChildFields = pChildFields
                            + cDlm 
                            + (if plPublic then hFld:serialize-name else hFld:name)
                cDlm        = ","
              .
             
        end.
    end.    
    
     
    // returns a where expression to find the parent from the child
    method public static character GetJoinParentExpression(phRelation as handle):
        define variable hChildBuffer  as handle    no-undo.
        define variable hParentBuffer as handle    no-undo.
        define variable cValue        as handle no-undo.
        define variable i             as integer   no-undo.
        define variable cExpression   as character no-undo.
        define variable cParentName   as character no-undo.
        
        if not valid-handle(phRelation) then
        do:
            undo, throw new IllegalArgumentError("Invalid relation").
        end.
        
        hChildBuffer = phRelation:child-buffer.
         
        if not hChildBuffer:avail then
        do:
            undo, throw new IllegalStateError(subst("Child '&1' not available in GetJoinParentExpression",hChildBuffer:name)).
        end.
        
        cParentName = phRelation:parent-buffer:name. 
                
        do i = 1 to num-entries(phRelation:relation-fields) by 2:
            cValue = hChildBuffer:buffer-field(entry(i + 1,phRelation:relation-fields)):buffer-value.
            cExpression = cExpression 
                   + (if cExpression = "" then "" else " and ")
                   + subst("&1.&2 = '&3'",cParentname,entry(i,phRelation:relation-fields),cValue). 
                   .
        end. 
        return cExpression. 
    
    end method.
    
    method public static character extent GetFieldsInIndex(phBuffer as handle, pcname as char, plPublic as logical):
        define variable cIndex as character no-undo.
        define variable i as integer no-undo.
        define variable iNumFields as integer no-undo.
        define variable j as integer no-undo.
        define variable cField as character no-undo.
        define variable hField as handle no-undo.
        define variable cFieldname as character no-undo.
        define variable cFieldArray as character extent no-undo.
        indexloop:
        do while true on error undo, throw:                           
            i = i + 1.                          
            
            cIndex = phBuffer:index-information(i).
            // no more indexes 
            if cIndex = ? then leave.
            
            if entry(1,cIndex) = pcname then
            do:
                extent(cFieldArray) = integer((num-entries(cIndex) - 4) / 2).
                   // loop through fields skip the asc/desc flag    
                do j = 5 to num-entries(cIndex) by 2:
                    cField = entry(j,cindex).
                    hField = phBuffer:buffer-field(cField).
                    iNumFields = inumFields + 1.
                    cFieldName = if plPublic then hField:serialize-name else hField:name.
                    cFieldArray[iNumFields] = cFieldName. 
                end.    
            end.
        end. 
        return cFieldArray.       
    end method.
      
    method public static character extent GetUniqueIndexes(phBuffer as handle, plPublic as logical):
        define variable cIndex as character no-undo.
        define variable i as integer no-undo.
        define variable cFldDlm as character no-undo.
        define variable iNumFields as integer no-undo.
        define variable cIndexFields as character no-undo.
        define variable j as integer no-undo.
        define variable cField as character no-undo.
        define variable hField as handle no-undo.
        define variable cFieldname as character no-undo.
        define variable cIndexDlm as character no-undo.
        define variable cIndexArray as character extent no-undo.
        
        indexloop:
        do while true on error undo, throw:                           
            i = i + 1.                          
            cIndex = phBuffer:index-information(i).
            // no more indexes 
            if cIndex = ? then leave.
           
            // unique flag is on 
            if entry(2,cindex) = "1" then 
            do:
                assign 
                    cFldDlm = ""
                    inumFields = 0.
                    cIndexFields = cIndexFields + cIndexDlm.   
               // loop through fields skip the asc/desc flag    
               do j = 5 to num-entries(cIndex) by 2:
                   cField = entry(j,cindex).
                   hField = phBuffer:buffer-field(cField).
                   cFieldName = if plPublic then hField:serialize-name else hField:name.
                   cIndexFields = cIndexFields 
                                + cFldDLm 
                                + cFieldName. 
                   cFldDlm = ",".
                   iNumFields = inumFields + 1.
               end.    
               cIndexDlm = ":".    
            end. 
        end.        
        if cindexfields> "" then
        do:
            extent(cIndexArray) = num-entries(cIndexFields,cIndexDlm).
            do i = 1 to num-entries(cIndexFields,cIndexDlm).
                cIndexArray[i] = entry(i,cIndexFields,cIndexDlm). 
            end.                          
        end.
        return cIndexArray. 
    end method.
    
    method public static character GetFirstFieldInIndexes(phBuffer as handle):
        return GetFirstFieldInIndexes(phBuffer,true).
    end method.    
    
    // return a comma spearated list of field names that are leading components in an index 
    method public static character GetFirstFieldInIndexes(phBuffer as handle, plPublic as logical):
        define variable cDummy as character no-undo.
        return GetFirstFieldInIndexes(phBuffer, plPublic, output cdummy). 
    end method.
    
    method public static character GetIndexNames(phBuffer as handle):
        define variable cIndex as character no-undo.
        define variable i as integer no-undo.
        define variable cIndexnames as character no-undo.
        define variable cFldDlm as character no-undo.
        indexloop:
        do while true on error undo, throw:     
            i = i + 1.                      
            cIndex = phBuffer:index-information(i).
            // no more indexes 
            if cIndex = ? then leave.
            cIndexNames = cIndexNames + cFldDlm + entry(1,cindex).
            cFldDlm = ",".
             
        end.        
        return cIndexNames. 
    end method.
    
    method public static character GetFirstFieldInIndexes(phBuffer as handle, plPublic as logical, output pIndexnames as character):
        define variable cIndex as character no-undo.
        define variable cFldDlm as character no-undo.
        define variable cFields as character no-undo.
        define variable cField as character no-undo.
        define variable hField as handle no-undo.
        define variable cFieldname as character no-undo.
        define variable i as integer no-undo.
        indexloop:
        do while true on error undo, throw:     
            i = i + 1.                      
            cIndex = phBuffer:index-information(i).
            // no more indexes 
            if cIndex = ? then leave.
           
            // word index is not on 
            if entry(4,cindex) = "0" then 
            do:
                assign 
                    cField     = entry(5,cindex)
                    hField = phBuffer:buffer-field(cField)
                    cFieldName = (if plPublic then hField:serialize-name else hField:name)
                    .
                if lookup(cFieldName,cfields) = 0 then
                do:     
                    cFields    = cFields + cFldDlm + cFieldname.
                    pIndexnames = pIndexnames + cFldDlm + entry(1,cindex).
                    cFldDlm = ",".
                end.    
                    
            end. 
        end.        
        return cFields. 
    end method.
    
     // return a comma spearated list of field names that are leading components in an index 
    method public static character extent GetFieldsInUniqueIndexes(phBuffer as handle, plPublic as logical):
        define variable cDummy as character no-undo.
        return GetFieldsInUniqueIndexes(phBuffer, plPublic, output cdummy). 
    end method.
    
     // return a comma spearated list of field names that are leading components in an index 
    method public static character extent GetFieldsInUniqueIndexes(phBuffer as handle, plPublic as logical,output pIndexnames as character):
        define variable cIndex      as character no-undo.
        define variable cField      as character no-undo.
        define variable hField      as handle no-undo.
        define variable cFieldname  as character no-undo.
        define variable i           as integer no-undo.
        define variable j           as integer no-undo.
        define variable cFieldList  as character   no-undo.
        define variable cindexFieldList  as character   no-undo.
        define variable cDlm        as character no-undo.
        define variable cIndexDlm    as character no-undo.
        define variable cIndexNameDlm    as character no-undo.
        indexloop:
        do while true on error undo, throw:     
            i = i + 1.                      
            cIndex = phBuffer:index-information(i).
            // no more indexes 
            if cIndex = ? then leave.
            // entry 2 is uniqe    
            
            if entry(2,cindex) = "1" then 
            do :
                cFieldList = "".    
                cDlm = "".
                 
                   // loop through fields skip the asc/desc flag    
                do j = 5 to num-entries(cIndex) by 2:
                    assign
                        cField     = entry(j,cindex) 
                        hField     = phBuffer:buffer-field(cField)
                        cFieldName = if plPublic then hField:serialize-name else hField:name
                        cFieldList = cFieldList 
                                   + cDlm 
                                   + cFieldName
                        cdlm = ","
                        . 
                 
                end.    
                assign 
                    cIndexFieldList = cIndexFieldList
                                    + cIndexDlm 
                                    + cFieldList
                    cIndexDlm       = ";"
                    pIndexnames = pIndexnames + cIndexNameDlm + entry(1,cindex)
                    cIndexNameDlm   = ",".
                .
            end. 
                                
        end.        
        if cindexFieldList > "" then
            return Array:Split(cIndexFieldList,cIndexDlm).
        else do: 
            define variable cEmptyArray  as character extent no-undo.
            return cEmptyArray.
        end.    
    end method.
    
/*    method public static JsonObject GetJsonKey(phBuffer as handle):                                                          */
/*        define variable cKeys           as character no-undo.                                                                */
/*        define variable iField          as integer no-undo.                                                                  */
/*        define variable cFld            as character no-undo.                                                                */
/*        define variable hFld            as handle no-undo.                                                                   */
/*                                                                                                                             */
/*        define variable oJson           as JsonObject no-undo.                                                               */
/*                                                                                                                             */
/*        cKeys = BufferUtil:GetKeys(phBuffer).                                                                                */
/*        if ckeys = "" then                                                                                                   */
/*            undo, throw new IllegalOperationError(subst("Cannot generate Json key for &1 with no unique key",phBuffer:name)).*/
/*        oJson = new JsonObject().                                                                                            */
/*        do iField = 1 to num-entries(cKeys).                                                                                 */
/*            assign                                                                                                           */
/*                cfld = entry(iField,cKeys)                                                                                   */
/*                hFld = phBuffer:buffer-field(cfld).                                                                          */
/*            oJson:Add(hFld:serialize-name,hFld:buffer-value).                                                                */
/*       end.                                                                                                                  */
/*                                                                                                                             */
/*       return oJson.                                                                                                         */
/*    end method.                                                                                                              */
     
/*    method public static character GetUrlRowKey(phBuffer as handle):     */
/*        return GetStringRowKey(phBuffer, "Url").                         */
/*    end method.                                                          */
/*                                                                         */
/*    method public static character GetUrlQueryRowKey(phBuffer as handle):*/
/*        return GetStringRowKey(phBuffer, "UrlQuery").                    */
/*    end method.                                                          */
/*                                                                         */
/*    method public static character GetLabeledKey(phBuffer as handle):    */
/*        return GetStringRowKey(phBuffer, "Text").                        */
/*    end method.                                                          */
    
    /* generate a row key for url usage
       @parameter buffer the buffer - must be avail
       @parameter Queryoption - false if use in Url (comma separator and "=" as expression)
                                true  if use in Url query (" and " separator and " eq " in expression) 
          
    */
    
/*    method private static character GetStringRowKey(phBuffer as handle, pcQueryOption as character):                                     */
/*        define variable cKeys           as character no-undo.                                                                            */
/*        define variable iField          as integer no-undo.                                                                              */
/*        define variable cFld            as character no-undo.                                                                            */
/*        define variable hFld            as handle no-undo.                                                                               */
/*        define variable cValue          as character no-undo.                                                                            */
/*        define variable lHasSpaceOrPlus as logical no-undo.                                                                              */
/*        define variable cRowKey         as character no-undo.                                                                            */
/*        define variable cExpressionSep  as character no-undo.                                                                            */
/*        define variable cPredicateSep   as character no-undo.                                                                            */
/*        define variable lUseLabel      as logical no-undo.                                                                               */
/*        if pcQueryOption = "UrlQuery" then                                                                                               */
/*        do:                                                                                                                              */
/*           assign                                                                                                                        */
/*              cExpressionSep = " eq "                                                                                                    */
/*              cPredicateSep  = " and ".                                                                                                  */
/*        end.                                                                                                                             */
/*        else if pcQueryOption = "Url" then                                                                                               */
/*        do:                                                                                                                              */
/*           assign                                                                                                                        */
/*              cExpressionSep = "="                                                                                                       */
/*              cPredicateSep  = ",".                                                                                                      */
/*        end.                                                                                                                             */
/*        else if pcQueryOption = "text" then                                                                                              */
/*        do:                                                                                                                              */
/*           assign                                                                                                                        */
/*              cPredicateSep = " and ".                                                                                                   */
/*              lUseLabel = true.                                                                                                          */
/*        end.                                                                                                                             */
/*                                                                                                                                         */
/*        cKeys = BufferUtil:GetKeys(phBuffer).                                                                                            */
/*        if ckeys = "" then                                                                                                               */
/*            undo, throw new IllegalOperationError(subst("Cannot generate &1 key for &2 with no unique key",pcQueryOption,phBuffer:name)).*/
/*                                                                                                                                         */
/*        do iField = 1 to num-entries(cKeys).                                                                                             */
/*            assign                                                                                                                       */
/*                cfld = entry(iField,cKeys)                                                                                               */
/*                hFld = phBuffer:buffer-field(cfld).                                                                                      */
/*            if hFld:data-type begins "date" then                                                                                         */
/*            do:                                                                                                                          */
/*                 cValue = quoter(iso-date(hFld:buffer-value),"'").                                                                       */
/*            end.                                                                                                                         */
/*            else if (hfld:data-type = "character" or hFld:data-type = "longchar") then                                                   */
/*            do:                                                                                                                          */
/*                assign                                                                                                                   */
/*                    cValue = hFld:buffer-value                                                                                           */
/*                    lHasSpaceOrPlus = index(cValue,StringConstant:SPACE) > 0 or index(cValue,"+") > 0                                    */
/*                    cValue      = quoter(cValue,"'")                                                                                     */
/*                    .                                                                                                                    */
/*           end.                                                                                                                          */
/*           else                                                                                                                          */
/*               cValue = hFld:buffer-value.                                                                                               */
/*                                                                                                                                         */
/*           /* if hasspaceorplus use field name in expression so we can safely decode bad encoders like certain REST testing tools...     */
/*              some REST tools will encode space as +, but not if there already is a +                                                    */
/*              this is indeterministic since a value with + and no space could origin from both space and +                               */
/*           */                                                                                                                            */
/*           if num-entries(cKeys) = 1 and lHasSpaceOrPlus = false and lUseLabel = false then                                              */
/*           do:                                                                                                                           */
/*               cRowKey = cValue.                                                                                                         */
/*           end.                                                                                                                          */
/*           else if luselabel = false then                                                                                                */
/*           do:                                                                                                                           */
/*               cRowKey = cRowKey                                                                                                         */
/*                           + (if cRowKey = "" then "" else cPredicateSep)                                                                */
/*                           + subst("&1&2&3",                                                                                             */
/*                                   hFld:serialize-name,                                                                                  */
/*                                   cExpressionSep,                                                                                       */
/*                                   cValue                                                                                                */
/*                                  ).                                                                                                     */
/*           end.                                                                                                                          */
/*           else do:                                                                                                                      */
/*               cRowKey = cRowKey                                                                                                         */
/*                           + (if cRowKey = "" then "" else cPredicateSep)                                                                */
/*                           + subst("&1: &2",                                                                                             */
/*                                   GetFieldLabel(hFld),                                                                                  */
/*                                   cValue                                                                                                */
/*                                  ).                                                                                                     */
/*           end.                                                                                                                          */
/*                                                                                                                                         */
/*       end.                                                                                                                              */
/*       return cRowKey.                                                                                                                   */
/*    end method.                                                                                                                          */
/*                                                                                                                                         */
    method static public character SingularizeSerializeName(pcName as char, plUpperCaseFirst as logical):
        define variable cName as character no-undo. 
        cName = SingularizeSerializeName(pcName).
        if plUpperCaseFirst then
            return caps(substring(cName,1,1)) + substring(cName,2). 
        else 
            return cName. 
    end method.
    
    method static public character SingularizeSerializeName(pcName as char):
        define variable iLength as integer no-undo.
        
        if (pcName > "") = false  then undo, throw new IllegalArgumentError("Name cannot be blank or null").
        
        if length(pcName) > 3 then
        do:
             
            iLength = length(pcname). 
            if pcName ="dies" then
                pcName = "die". 
            if pcName matches "*ies" then
            do:
                pcName = substr(pcName,1,iLength - 3) + "y".
            end.
            else if pcName matches "*xes" then
            do:
                pcName = substr(pcName,1,iLength - 2).
            end.
            else if pcName matches "*tches" then
            do:
                pcName = substr(pcName,1,iLength - 2).
            end.
            else if pcName matches "*shes" then
            do:
                pcName = substr(pcName,1,iLength - 2).
            end.
            else if pcName matches "*addresses" or pcName matches "*processes" or pcName matches "*flatnesses" or pcName matches "*statuses"then
            do:
                pcName = substr(pcName,1,iLength - 2).
            end.
            else if pcName matches "*s" then
            do:
                pcName = substr(pcName,1,iLength - 1).
            end.    
        end.
        // ?? not sure if this ever happens
        else if pcname matches "*s" then
        do:
            pcName = substr(pcName,1,length(pcname) - 1).
        end.
        
        return pcName.  
    end method.    
    
    // name of row for single row requests error messages etc
    method static public character GetRowName(phHandle as handle, plUpperCaseFirst as logical):
        define variable cName as character no-undo. 
        cName = GetRowName(phHandle).
        if plUppercasefirst then 
            return StringUtil:UpperCaseFirst(cName).
        else 
            return cName.       
    end method.  
    
     // name of row for single row requests error messages etc
    method static public character GetCurrentRowAsText(phHandle as handle):
        define variable oQueryentry as IQueryEntry no-undo.
        
        case phHandle:type:
            when "buffer" then do:
                if valid-handle(phHandle:after-buffer) then 
                    phHandle = phHandle:after-buffer.
            end. 
            when "temp-table" then do:
                if valid-handle(phHandle:after-table) then 
                    phHandle = phHandle:after-table.
            end.
            otherwise 
                undo, throw new IllegalArgumentError(subst("Handle of type &1",phHandle:type)). 
        end case.
        if not phHandle:avail then 
            undo, throw new IllegalOperationError(subst("Buffer &1 is not available",phHandle:name )).
        
        oQueryentry = cast(QueryUtil:GetCurrentRowKeyEntry(phHandle),IQueryEntry). 
        return subst("&1 where &2",
                      GetRowName(phHandle, true), // uppercase first
                      oQueryentry:ToString(phHandle, 
                      true, // public
                      true) //trim pouter parenthesis if applicable
                 ).
                
    end method.      
    
    method static public character GetRowName(phHandle as handle):
    
        case phHandle:type:
            when "buffer" then do:
                if valid-handle(phHandle:after-buffer) then 
                    phHandle = phHandle:after-buffer.
            end. 
            when "temp-table" then do:
                if valid-handle(phHandle:after-table) then 
                    phHandle = phHandle:after-table.
            end.
            otherwise 
                undo, throw new IllegalArgumentError(subst("Handle of type &1",phHandle:type)). 
        end case.
        
        return SingularizeSerializeName(phHandle:serialize-name).
              
    end method.    
    
    method static public character GetHiddenFields(phBuffer as handle): 
        return GetFields(phBuffer,true).
    end.
    
    method static public character GetVisibleFields(phBuffer as handle): 
        return GetFields(phBuffer,false).
    end.
    
    method static private character GetFields(phBuffer as handle): 
        return GetFields(phBuffer,?).
    end.
    
    //  set visible fields (really hiding all other fields)  
    method static public void SetVisibleFields(phBuffer as handle, pcFieldList as character): 
        define variable i as integer no-undo.
        define variable cList as character no-undo.
        define variable cDlm  as character no-undo.
        define variable hFld as handle no-undo.  
        define variable hParentRelation as handle no-undo.
        define variable iJoinfield as integer no-undo.
        do i = 1 to phBuffer:num-fields:
            hfld = phbuffer:buffer-field(i). 
            if lookup(hfld:serialize-name,pcfieldlist) = 0 then
            do:
                if hfld:serialize-hidden = false then 
                    hfld:serialize-hidden = true. 
            end.
            else do:
                if valid-handle(phbuffer:parent-relation) and phbuffer:parent-relation:foreign-key-hidden then 
                do:
                    do iJoinField = 2 to num-entries(phbuffer:parent-relation:relation-fields) by 2:
                        if hFld:name = entry(iJoinfield,phbuffer:parent-relation:relation-fields) then 
                        do:
                            phbuffer:parent-relation:foreign-key-hidden = false.
                            leave.
                        end.
                    end. 
                end.     
                pcfieldlist = StringUtil:RemoveEntryFromList(hfld:serialize-name, pcfieldlist).
            end.
                       
        end.    
        if pcFieldList > "" then 
            undo, throw new IllegalArgumentError(subst("'&1' &2 not &3valid field reference&4 for buffer &5",
                                                       pcFieldList, 
                                                       if num-entries(pcFieldList) > 1 then "are" else "is",
                                                       if num-entries(pcFieldList) > 1 then "" else "a ",
                                                       if num-entries(pcFieldList) > 1 then "s" else "",
                                                       phBuffer:name  
                                                       )   
                                                    ).
    end.
    
    method static private character GetFields(phBuffer as handle,plhidden as logical): 
        define variable i as integer no-undo.
        define variable cList as character no-undo.
        define variable cDlm  as character no-undo.
        define variable hFld as handle no-undo.  
        do i = 1 to phBuffer:num-fields:
            hfld = phbuffer:buffer-field(i). 
            if hfld:serialize-hidden = plHidden or plHidden = ? then
            do:
               assign 
                   cList = cList + cDlm + hFld:name
                   cdlm = ",".
            end.       
        end.    
        return cList.
    end.
    
    // return a comma separated paired list with fully qualified name for the db
    // to use by DataSource GetFieldMap when there are joins. 
    // Progress Datasource default mapping will map to the last matching table -  
    method static public character GetFieldMap(phBuffer as handle,phDbBuffer as handle): 
        return GetFieldMap(phBuffer, phDbBuffer, "").
    end.
    
    method static public character GetFieldMap(phBuffer as handle,phDbBuffer as handle, pcExcludeDbFieldNames as character): 
        define variable i as integer no-undo.
        define variable cList as character no-undo.
        define variable cDlm  as character no-undo.
        define variable hFld as handle no-undo.  
        define variable hDbFld as handle no-undo.
          
        if pcExcludeDbFieldNames = ? then 
            pcExcludeDbFieldNames = "".
        
        if phBuffer:dbname <> "PROGRESST" then
            undo, throw new IllegalArgumentError("The first buffer parameter must reference a temp-table").
        
        do i = 1 to phBuffer:num-fields:
            hfld = phbuffer:buffer-field(i). 
            hDbFld = phDbBuffer:buffer-field(hFld:name) no-error.
            
            if valid-handle(hDbFld) and hDbFld:extent = 0 and (pcExcludeDbFieldNames = "" or lookup(hDbFld:name,pcExcludeDbFieldNames) = 0) then 
            do:
                cList = cList
                      + cDlm 
                      + subst("&1,&2.&3",hFld:name,hDbFld:buffer-name,hDbFld:name)
                      .
                cDlm = ",".      
            end.                   
        end.    
        return cList.
    end.
    
    method public static character GetFieldLabel(pfld as handle):
        if pFld:label <> pFld:name then 
            return pFld:label.
        else 
            return StringUtil:CamelToWords(pFld:serialize-name).  
    end method.    
    
    method public static character GetFieldValueForMessage(pfld as handle):
        if pFld:data-type = "character" or pFld:data-type begins "date" then 
            return quoter(pFld:buffer-value,"'").
        else 
            return pFld:buffer-value.  
    end method.    
    
    // get a serializename of field name in buffer - handles extent reference   
    method public static character GetFieldSerializeName(phBuffer as handle,pcfieldname as char):
        define variable iArrayPos as integer   no-undo.
        define variable cField    as character no-undo.
        define variable hField    as handle    no-undo.
        define variable iBracket  as integer no-undo.
        
        iArrayPos = index(pcFieldname,"[").  
        if iArrayPos > 0 then
        do:
            cField   = substring(pcFieldname,1,iArrayPos - 1).
            iBracket = integer(right-trim(substring(pcFieldname,iArrayPos + 1),"]")).
        end.
        else 
            cField = pcFieldname.
             
        hfield = phBuffer:buffer-field(cField) no-error.
        if not valid-handle(hField) then 
            undo, throw new IllegalArgumentError(subst("Field '&1' does not exist in &2 ",pcFieldName,phBuffer:name)).
        
        if iBracket > 0 then
        do:
           if iBracket > hField:extent then 
               undo, throw new IllegalArgumentError(subst("Extent reference &1 in Field '&2' is not valid",iBracket,pcFieldName,phBuffer:name)).
           // JSON array is 0 based - subtract 1 
           return subst("&1[&2]",hField:serialize-name,iBracket - 1).
        end.
        else 
            return hField:serialize-name.          
    end method.     
    
    method public static void HideCalculatedFields(phBuffer as handle):
        define variable i as integer no-undo.
        if not valid-handle(phBuffer) then
           undo, throw new IllegalArgumentError("Invalid handle").
        if phBuffer:type <> "buffer" then
           undo, throw new IllegalArgumentError(subst("Handle of Type '&1'",phBuffer:type)).
        do i = 1 to phBuffer:num-fields: 
            if index(phBuffer:buffer-field(i):serialize-name,"@") > 0 then 
               phBuffer:buffer-field(i):serialize-hidden = true.
        end.    
         
    end method.    
    
    /***
    Return a list of changes for a buffer by comparing before and after 
    Both buffers must be available - It is not the job of this to synchronize 
    NOTE - It is NOT the job of this to synchronize the two buffers 
    **/
    method static public character GetChanges(phBuffer as handle):
        return GetChanges(phBuffer,"").
    end method. 
     
    /***
    Return a list of changes for a buffer by comparing beofre and after 
    Both buffers must be available 
    NOTE - It is NOT the job of this to synchronize the two buffers 
    **/
    method static public character GetChanges(phBuffer as handle, pcExcept as character):
        define variable hBefore as handle no-undo.
        define variable hDummy as handle extent no-undo.
        define variable hDummy2 as handle extent no-undo.
        
        hBefore = AssertAndGetBeforeBuffer(phBuffer).        
        return CompareFields(phbuffer,hBefore,pcExcept,output hDummy, output hDummy2).  
    end method.
    
     /** used to append text to show differences when comparing data with a list of Ids that has trhe same values together
       I.E Parts 123, 345, 4567 has metal type 6 ()
      hard coded line break delimiter for now - add overload if needed  
      @param - diff text - the current value to append to 
      @param - buffer    - the buffer to use as label for keys  
      @param - idlist    - concatenated list of key values (so currently only fore single key) 
      @param - valueText - The value for all the records with the id. Label included and typcially both a key and description  
     **/
    method public static character AppendDifferences(pcDiffText as character, phBuffer as handle, pcIdList as character,pcValueText as character) : 
        return StringUtil:AppendEntry(pcDiffText,
                                      subst("&1 &2 &3 &4", 
                                            subst("&1:",
                                                  if num-entries(pcIdList) > 1 
                                                  then StringUtil:UpperCaseFirst(phBuffer:serialize-name)
                                                  else GetRowName(phbuffer,true)
                                            ),
                                            StringUtil:GetMessageFromEntries(pcIdList),
                                            if num-entries(pcIdList) > 1 then "have" else "has",
                                            // add period, but ensure only one
                                            subst("&1.",right-trim(pcValueText,"."))
                                           ),
                                      "~n"       
                                     ) .
    end method.  
    
    method static public handle AssertAndGetBeforebuffer(phbuffer as handle):
        define variable hBefore as handle no-undo.
 
        if not phBuffer:avail then 
            undo, throw new IllegalOperationError(subst("Buffer &1 is not available",phBuffer:name )).
        
        hBefore = phbuffer:before-buffer.
        if not valid-handle(hBefore) then 
            undo, throw new IllegalArgumentError(subst("Buffer &1 has no before image buffer",phBuffer:name )).
        
        if not hBefore:avail then 
            undo, throw new IllegalOperationError(subst("Before Buffer &1 is not available",hBefore:name )).
        
        if hBefore:rowid <> phBuffer:before-rowid then 
            undo, throw new IllegalOperationError(subst("Before buffer &1 and after Buffer &2 is not in synch",hBefore:name,phbuffer:name)).
 
        return hBefore. 
    end method.    
    
    method static public character CompareFields(phBuffer1 as handle,phBuffer2 as handle, pcExcept as character, output phDataFields as handle extent, output phSourceFields as handle extent ): 
        return CompareFields(phBuffer1,phBuffer2, pcExcept,"",output phDataFields, output phSourcefields).
    end method.
    
    method static public character CompareFields(phBuffer1 as handle,phBuffer2 as handle,  pcExcept as character):
         define variable hDummy as handle extent no-undo.
         define variable hDummy2 as handle extent no-undo.
         return CompareFields(phBuffer1, phBuffer2, pcExcept, "", output hDummy, output hDummy2).
    end method.
    
    method static public character CompareFields(phBuffer1 as handle,phBuffer2 as handle,  pcExcept as character, pcOnlyTheseFields as character):
         define variable hDummy as handle extent no-undo.
         define variable hDummy2 as handle extent no-undo.
         return CompareFields(phBuffer1, phBuffer2, pcExcept, pcOnlyTheseFields, output hDummy, output hDummy2).
    end method.
    
    
    method static public character CompareFields(phBuffer1 as handle,phBuffer2 as handle, pcExcept as character, pcOnlyTheseFields as character,output phDataFields as handle extent, output phSourceFields as handle extent): 
        define variable i as integer no-undo.
        define variable cList as character no-undo.
        define variable cDlm  as character no-undo.
        define variable hFld1 as handle no-undo.  
        define variable hFld2 as handle no-undo.
        define variable lChanged as logical no-undo.  
        define variable lCompareDatasetAndDb as logical no-undo.
        define variable IsTT1   as logical no-undo.
        define variable iLookup as integer no-undo.
        define variable cDbColumn as character no-undo.
        define variable hAfterBuffer as handle no-undo.
        define variable cDataFieldHandles as character no-undo.
        define variable cSourceFieldHandles as character no-undo.
        define variable iArrayPos as integer no-undo.
        define variable i2Extent as integer no-undo.
        // allow ? in fieldname lists
        if pcExcept = ? then 
            pcExcept = "".
        if pcOnlyTheseFields = ? then
            pcOnlyTheseFields  = "".
           
        
        lCompareDatasetAndDb = valid-handle(phBuffer1:dataset) and phBuffer2:dbname <> "PROGRESST".
         
        if lCompareDatasetAndDb = false and valid-handle(phBuffer1:dataset) = false then
            undo, throw new IllegalArgumentError("The first buffer parameter must reference the temp-table when comparing a dataset buffer with a database buffer").
        
    
        fieldloop:
        do i = 1 to phBuffer1:num-fields:
            hfld1 = phbuffer1:buffer-field(i).
            hFld2 = ?.
            lChanged = false. 
            
            if lookup(hfld1:name,pcExcept) = 0 
            and (pcOnlyTheseFields = "" or lookup(hfld1:name,pcOnlyTheseFields) > 0) then
            do: 
                i2Extent = 0.
                // if the first buffer is a dataset buffer  and the second is db buffer then find the db field name
                if lCompareDatasetAndDb then 
                do:
                    // data-source-complete-map is only valid for after buffer
                    hAfterBuffer = if valid-handle(phBuffer1:after-buffer) then phBuffer1:after-buffer else phBuffer1.
                    cDbColumn = StringUtil:GetMappedName(subst("&1.&2",hAfterBuffer:name,hfld1:name), hAfterBuffer:data-source-complete-map).
                    if cDbColumn > "" and cDbColumn begins phBuffer2:name + "." then
                    do:
                        iArrayPos = index(cDbColumn,"[").  
                        if iArrayPos > 0 then
                        do:
                            i2Extent = integer(right-trim(substring(cDbColumn,iArrayPos + 1),"]")).
                            cDbColumn = substring(cDbColumn,1,iArrayPos - 1).
                        end.
                        hFld2 = phBuffer2:buffer-field(entry(2,cDbColumn,".")) no-error.   
                    end.    
                end.
                else    
                    hFld2 = phBuffer2:buffer-field(hFld1:name) no-error. 
                
                if valid-handle(hFld2) then
                do: 
                    case hFld1:data-type:
                        when "character" then 
                        do:
                            lChanged = not compare(hFld1:buffer-value,"eq",hfld2:buffer-value(i2Extent),"case-sensitive").                         
                        end.
                        when "clob" then 
                        do:     
                            lChanged = StringUtil:CompareClobValues(hFld1, "eq", hFld2, "case-sensitive"). 
                        end.
                        otherwise do:                     
                            lChanged = hFld1:buffer-value <> hfld2:buffer-value(i2Extent).
                        end.    
                    end case.   
                    if lChanged then
                    do:
                        assign
                            cDataFieldHandles   = cDataFieldHandles + cDlm + string(hfld1) 
                            cSourceFieldHandles = cSourceFieldHandles + cDlm + string(hfld2) 
                            cList = cList + cDlm + hFld1:name
                            cdlm = ",".
                      
                    end.        
                end.
            end.  
        end.  
        if cDataFieldHandles > "" then 
        do:
            phDataFields   = Array:SplitToHandle(cDataFieldHandles).
            phSourceFields = Array:SplitToHandle(cSourceFieldHandles).
        end.   
        return cList.
    end.
  
    method static public character GetRowStateStringPassive(piRowState as integer):
        case piRowstate:
            when row-created then 
                return "create".
            when row-modified then 
                return "modify".
            when row-deleted  then 
                return "delete".
            otherwise
                return "".     
        end.           
    end method.
    
    method static public character GetRowStateStringPassive(piRowState as integer, plUpperCaseFirst as logical):
        define variable cState as character no-undo.
        cState = GetRowStateStringPassive(piRowState).
        if plUpperCaseFirst then 
            return StringUtil:UpperCaseFirst(cState).
        else 
            return cState.       
    end method.
    
    method static public character GetRowStateStringPast(piRowState as integer):
         case piRowstate:
            when row-created then 
                return "created".
            when row-modified then 
                return "updated".
            when row-deleted  then 
                return "deleted".
            otherwise
                return "unmodified".     
        end.                 
    end method.
    
    /*
    Returns a comma separated list of serialize-name,name pairs of field buffers
    Fields that have same name is not added to list 
    Use StringUtil:GetUseName to resolve the name
    */
    method static public character GetPublicToNameFieldPairs(phBuffer as handle):
        define variable iFld         as integer          no-undo.
        define variable hFld         as handle           no-undo.
        define variable cStringMap   as character        no-undo.
        define variable cDlm         as character        no-undo. 
        define variable hAfterBuffer as handle no-undo.
        // serialize-names are not set in before image
        if valid-handle(phBuffer:after-buffer) then 
            hAfterBuffer = phBuffer:after-buffer.
        else
            hAfterBuffer = phBuffer.
            
        do iFld = 1 to hAfterBuffer:num-fields:
            hFld = hAfterBuffer:buffer-field(iFld).
           
            if hFld:serialize-name <> hFld:name then
            do:
                assign
                    cStringMap = cStringMap + cDlm + hFld:serialize-name + "," + hFld:name
                    cDlm = ",".
            end.         
        end.
        return cStringMap.         
    end method.
    
    method static public character GetPublicFieldNames(phBuffer as handle,pcFieldList as character):
        define variable iFld       as integer          no-undo.
        define variable hFld       as handle           no-undo.
        define variable cNames     as character        no-undo.
        define variable cDlm       as character        no-undo. 
       
        do iFld = 1 to num-entries(pcFieldList):
            hFld = phBuffer:buffer-field(entry(iFld,pcfieldList)) no-error.
            if not valid-handle(hFld) then
                undo, throw new IllegalArgumentError(subst("Field '&1' does not exist in buffer '&2'",entry(iFld,pcfieldList),phbuffer:name)).
            assign
                cNames = cNames + cDlm + hFld:serialize-name 
                cDlm = ",".
                     
        end.
        return cNames.         
    end method.
    
    method static public handle GetFieldByPublicName(phBuffer as handle,pcField as character):
        define variable iFld       as integer          no-undo.
        define variable hFld       as handle           no-undo.
       
        do iFld = 1 to phBuffer:num-fields:
            hFld = phBuffer:buffer-field(ifld) no-error.
            if hFld:serialize-hidden = false and hFld:serialize-name = pcField then 
                return hFld.
        end.
        return ?.    
    end. 
    
    method static public character GetFieldLabels(phBuffer as handle,pcFieldList as character):
        define variable iFld       as integer          no-undo.
        define variable hFld       as handle           no-undo.
        define variable cNames     as character        no-undo.
        define variable cDlm       as character        no-undo. 
       
        do iFld = 1 to num-entries(pcFieldList):
            hFld = phBuffer:buffer-field(entry(iFld,pcfieldList)) no-error.
            if not valid-handle(hFld) then
                undo, throw new IllegalArgumentError(subst("Field '&1' does not exist in buffer '&2'",entry(iFld,pcfieldList),phbuffer:name)).
            assign
                cNames = cNames + cDlm + GetFieldLabel(hfld). 
                cDlm = ",".
                     
        end.
        return cNames.         
    end method.
    
    method static public character AssertAndConvertFromPublicValue(phBuffer as handle,pcField as char, pcValue as char, pcFilterForError as character):
        return AssertAndConvertFromPublicValue(phBuffer, pcField, pcValue). 
        catch e1 as IllegalArgumentError :
            undo, throw new IllegalArgumentError(subst("&1. The filter '&2' is invalid.",right-trim(e1:GetStrippedMessage(),"."),pcFilterForError)).     
        end catch.
        
    end method.     
    
    method static public character AssertAndConvertFromPublicValue(phBuffer as handle,pcField as char, pcValue as char):
    
    // Convert a json string value to internal format
    // This is used for filter parsing of quoted strings from web (or any public presentation layer that uses json formats)  
       
        define variable cInvalidFieldTmpl     as character init "'&1' is not a valid property name in '&2'" no-undo.
        define variable cMissingQuotesMsgTmpl as character init "Invalid value &1 for property '&2'. &3 values must be single quoted." no-undo.
        define variable cBadQuotesMsgTmpl     as character init "Invalid value &1 for property '&2'. &3 values cannot be quoted." no-undo.
        
        define variable cValue                as character no-undo.
        define variable hField                as handle no-undo.
        define variable lHasQuotes            as logical no-undo.
        define variable iTest                 as integer no-undo.
        define variable i64Test               as int64 no-undo.
        define variable dTest                 as decimal no-undo.
        define variable lUNsupported          as logical no-undo.
        define variable lLogical              as logical no-undo.
        do on error undo, throw:
            hField = phBuffer:buffer-field(pcField).
            catch e as Progress.Lang.Error :
                undo, throw new IllegalArgumentError(subst(cInvalidFieldTmpl,
                                                          pcField,
                                                          BufferUtil:GetRowName(phBuffer)
                                                          )
                                                    ).      
            end catch.
        end.
         
        cValue = pcValue. // keep pcValue for erropr messages 
        // if ? just return the value (the fieldValueparser conceets from 'null' to ?)
        if cValue <> ? then
        do: 
            cValue = StringUtil:UnquoteValue(cValue, output lHasQuotes). // if we need to know the quote there is an overload woth output character
            if cValue = "null" and lHasQuotes = false then
            do: 
               cValue = ?.
            end.    
            else do:    
                // this case block is for check of quotes - we use another case for assign of values below
                case hField:data-type:
                    when "character" then 
                    do:
                        if lHasQuotes = false then
                            undo, throw new IllegalArgumentError(subst(cMissingQuotesMsgTmpl,
                                                                      pcValue,
                                                                      hField:serialize-name,
                                                                      "String"
                                                                )
                                                           ).
                    end.
                    when "date" or when "datetime" or when "datetime-tz" then
                    do:                                                            
                        if lHasQuotes = false then 
                            undo, throw new IllegalArgumentError(subst(cMissingQuotesMsgTmpl,
                                                                      pcValue,
                                                                      hField:serialize-name,
                                                                      "Date"
                                                                     )
                                                                ).
                    end.
    /*                when "logical" then                                                     */
    /*                do:                                                                     */
    /*                    if lHasQuotes then                                                  */
    /*                        undo, throw new IllegalArgumentError(subst(cBadQuotesMsgTmpl,   */
    /*                                                                  pcValue,              */
    /*                                                                  hField:serialize-name,*/
    /*                                                                  "Boolean"             */
    /*                                                                 )                      */
    /*                                                            ).                          */
    /*                end.                                                                    */
                    otherwise do:                                          
                        if lHasQuotes then  
                            lUNsupported = true.
    /*                        // temporary -  as of current we convert this to warning since there*/
    /*                        // are so many cases with this problem                              */
    /*                        undo, throw new UnsupportedOperationError(subst(cBadQuotesMsgTmpl,  */
    /*                                                                   pcValue,                 */
    /*                                                                   hField:serialize-name,   */
    /*                                                                   "Number"                 */
    /*                                                                   )                        */
    /*                                                            ).                              */
                                                        
                    end.
                end case.  
               
               // use another case for values since the cases are differentr from the quote check above (we amy also add parameter to skip quote chack in future) 
                case hField:data-type:
                   // ABL is unsafe (misguided kindness) and rounds decimal values without error 
                    when "integer"  or when "int64" then 
                    do:
                       
                       do on error undo, throw:
                           case hField:data-type:
                               when "integer" then
                                   iTest = integer(cValue).
                               when "int64" then 
                                   i64Test = int64(cValue).
                           end.      
                           catch e as Progress.Lang.Error :
                               undo, throw new illegalArgumentError(subst("Invalid value &1 for property '&2'. The value is not a valid ABL '&3' data-type value.",
                                                                          cValue, 
                                                                          hField:serialize-name,
                                                                          hField:data-type 
                                                                         )
                                                                   ,e).     
                           end catch.
                       end.     
                       
                       // be strict - if we forgive .0 in data then this can later cause production errors
                       if NumberUtil:NumDecimals(decimal(cValue)) > 0 then // a less strict variation - if decimal(cValue) <> int64(cValue) then 
                            undo, throw new IllegalArgumentError(subst("Invalid value &1 for property '&2'. The property is bascked by ABL data type '&3 and cannot have decimals",
                                                                       cValue, 
                                                                       hField:serialize-name,
                                                                       hField:data-type
                                                                      )
                                                                 ).
                    end.
                    when "decimal" then 
                    do:
                       
                       do on error undo, throw:
                           dTest = decimal(cValue).
                           catch e as Progress.Lang.Error :
                               undo, throw new illegalArgumentError(subst("Invalid value &1 for property '&2'. The value is not a valid ABL &3 data-type value.",
                                                                          cValue, 
                                                                          hField:serialize-name,
                                                                          hField:data-type 
                                                                         )
                                                                   ).     
                           end catch.
                        end.     
                    end.
                    when "datetime"  or when "datetime-tz" then
                    do: 
                        // replace the date part of the value 
                        substr(cValue,1,10) = StringUtil:StringDateFromIsoDate(substr(cValue,1,10)).
                    end.       
                    when "date" then
                    do:
                        cValue = StringUtil:StringDateFromIsoDate(cValue).
                    end.    
                    when "logical" then 
                    do:
                        // logical is too wide as i t also handled 1 and 0  only allow true false yes and no 
                        if lookup(cValue,"true,false,yes,no") = 0 then 
                            undo, throw new illegalArgumentError(subst("Invalid value &1 for property '&2'. The value is not a valid ABL &3 data-type value.",
                                                                       cValue, 
                                                                       hField:serialize-name,
                                                                       hField:data-type 
                                                                      )
                                                                ).     
                             
                    end.    
                end case.
                if lUnsupported then 
                   // temporary -  as of current we convert this to warning since there 
                            // are so many cases with this problem
                    undo, throw new UnsupportedOperationError(subst(cBadQuotesMsgTmpl,
                                                                    pcValue,
                                                                    hField:serialize-name,
                                                                    "Number"
                                                                    )
                                                                ).              
                
            end.
        end. // cValue <> ?
        return cValue.
        
        catch e as Progress.Lang.Error :
            undo, throw e.    
        end catch.
    end method.
end class.