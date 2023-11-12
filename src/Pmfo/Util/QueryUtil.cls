 
 /*------------------------------------------------------------------------
    File        : QueryUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 31 10:58:54 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.BusinessLogic.SortOrderEnum from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.Parser.KeyValueParser from propath.
using Pmfo.Core.BusinessLogic.QueryDefinition from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DateUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
 


class Pmfo.Util.QueryUtil: 
    define private static property LockList as character init "NO-LOCK,SHARE-LOCK,EXCLUSIVE-LOCK,SHARE,EXCLUSIVE,SHARE-L,SHARE-LO,SHARE-LOC,EXCLUSIVE-L,EXCLUISVE-LO,EXCLUSIVE-LOC" no-undo get.
    define private static property RecordphraseList as character init "BY,BREAK,INDEXED-REPOSITION,MAX-ROWS" no-undo get.
    // convenience utitlity to convert datetime predicate to a group of date and time 
    // the DataTypeEnum does not serve our purpose since we don't want to separate array and non array    
    method public static IQueryEntry CreateDateAndTimeGroup (pPredicate as QueryPredicate,pDatatype as character, pdateField as char, ptimeField as char):
        define variable oGroup     as QueryGroup  no-undo.
        define variable oGroupFrom as IQueryEntry no-undo.
        define variable oGroupTo   as IQueryEntry no-undo.
        define variable dDate      as date        no-undo.
        define variable iTime      as integer     no-undo.
        define variable dDates     as date        no-undo extent.
        define variable iTimes     as integer     no-undo extent.
        define variable i          as integer     no-undo.
        
        case pDataType:
            when "datetime" then do:
                if extent(pPredicate:StringValues) = ? then
                    DateUtil:GetDateAndTime(datetime(pPredicate:StringValue),output ddate, output iTime).
                else do:
                    extent(dDates) = extent(pPredicate:StringValues).
                    extent(iTimes) = extent(pPredicate:StringValues).
                    do i = 1 to extent(dDates):                  
                        DateUtil:GetDateAndTime(datetime(pPredicate:StringValues[i]),output ddate, output iTime).
                        dDates[i] = dDate.
                        itimes[i] = itime.
                    end.
                end.    
            end.    
            when "datetime-tz" then do:
                if extent(pPredicate:StringValues) = ? then
                    DateUtil:GetDateAndTime(datetime-tz(pPredicate:StringValue),output ddate, output iTime).
                else do:
                    extent(dDates) = extent(pPredicate:StringValues).
                    extent(iTimes) = extent(pPredicate:StringValues).
                    do i = 1 to extent(dDates):                  
                        DateUtil:GetDateAndTime(datetime-tz(pPredicate:StringValues[i]),output ddate, output iTime).
                        dDates[i] = dDate.
                        itimes[i] = itime.
                    end.
                end.     
            end.    
            otherwise
                undo, throw new IllegalArgumentError(subst("Data type parameter '&1' must be 'datetime' or 'datetime-tz'",pDataType)).
              
        end.
        
        case pPredicate:Operator:
            when QueryOperatorEnum:InList then
            do:
                oGroup = new QueryGroup( 
                              cast(Array:ToArray(new QueryPredicate(pdateField,pPredicate:Operator,Array:ToStringArray(ddates)),
                                                 new QueryPredicate(ptimeField,pPredicate:Operator,Array:ToStringArray(itimes))
                                   ),
                                   QueryPredicate)
                             ).
            end. 
            when QueryOperatorEnum:Eq or when QueryOperatorEnum:Ne then 
            do:
                oGroup = new QueryGroup( 
                                     cast(Array:ToArray(new QueryPredicate(pdateField,pPredicate:Operator,string(ddate)),
                                                        new QueryPredicate(ptimeField,pPredicate:Operator,string(itime))
                                         ),
                                        QueryPredicate)
                                 ).
            end.
            when QueryOperatorEnum:Ge or when QueryOperatorEnum:Gt then 
            do:
                 
                 oGroup = new QueryGroup( 
                                     cast(Array:ToArray(new QueryPredicate(pdateField,QueryOperatorEnum:Eq,string(ddate)),
                                                        new QueryPredicate(ptimeField,pPredicate:Operator,string(itime))
                                         ),
                                        QueryPredicate)
                                 ).
                 oGroup = new QueryGroup(          
                             cast(Array:ToArray(oGroup,
                                                new QueryPredicate(JoinEnum:Or,pdateField,QueryOperatorEnum:Ge,string(ddate))
                                         ),
                                        IQueryEntry)
                                      ).
            end.
            when QueryOperatorEnum:InRange then 
            do:
                oGroupFrom =  CreateDateAndTimeGroup(new QueryPredicate(pPredicate:FieldName,QueryOperatorEnum:Ge,pPredicate:StringValues[1]),pDataType,pDateField,pTimeField).  
                oGroupTo   =  CreateDateAndTimeGroup(new QueryPredicate(pPredicate:FieldName,QueryOperatorEnum:Le,pPredicate:StringValues[2]),pDataType,pDateField,pTimeField).
                oGroup = new QueryGroup(          
                                        cast(Array:ToArray(oGroupFrom,oGroupTo),IQueryEntry)
                                        ).
            end.
        end case.         
        oGroup:Join = pPredicate:Join.
        return oGroup.
    end method.    
    
    method public static Pmfo.Core.BusinessLogic.IQueryEntry CreateQueryEntry(input phBuffer as handle,pcFieldList as character):
        define variable i as integer no-undo.
        define variable hField as handle no-undo.
        define variable oGroup as QueryGroup no-undo.
        Assert:NotNull(phBuffer,"Buffer").
        Assert:Equals(phBuffer:type,"Buffer").
        
        if phBuffer:avail = false then 
            undo, throw new IllegalStateError(subst("Buffer &1 is available",phBuffer:name)).    
        
        do i = 1 to num-entries(pcFieldList):
            hField = phBuffer:buffer-field(entry(i,pcFieldList)) no-error.
            if not valid-handle(hField) then 
                undo, throw new IllegalArgumentError(subst("Field &1 isd not defined in Buffer &2",entry(i,pcFieldList),phBuffer:name)).    
            
            if num-entries(pcFieldList) = 1 then 
                return new QueryPredicate(hField:name,QueryOperatorEnum:Eq,hField:buffer-value). 
            else if i = 1 then 
                oGroup = new QueryGroup().     
            oGroup:AddEntry(hField:name,QueryOperatorEnum:Eq,hField:buffer-value).
            
        end.        
        return oGroup. 
    end method.
    
    method public static character TrimQuery(pcQuery as char):
        if pcQuery begins "for " then 
            return trim(substr(pcQuery,5)).  
        if pcQuery begins "preselect " then 
            return trim(substr(pcQuery,11)). 
        if pcQuery begins StringConstant:SPACE then
            undo, throw new IllegalArgumentError("Query passed to TrimQuery(character) cannot start with blank").
        
        return pcQuery .
    end method. 
    
    method public static character ReplaceTableName(pcQuery as char, pcOldname as char, pcNewName as char):
        if pcNewName = "" then
        do:
            // TODO reserach : some of these renames makes no sense with blank rename
            // maybe they are not applicable in any case... 
            pcQuery = replace(pcQuery," " + pcOldname + " "," ").
            pcQuery = replace(pcQuery," " + pcOldname + "."," ").
            pcQuery = replace(pcQuery,"(" + pcOldname + " ","(" + " ").
            pcQuery = replace(pcQuery,"( )","").
            pcQuery = replace(pcQuery,"(" + pcOldname + ".","(" + " ").
            pcQuery = replace(pcQuery,"=" + pcOldname + ".","=" + " ").
            // if the table somehow is at the end 
            if pcQuery matches "*" + pcOldname then 
                pcQuery = substr(pcQuery,1,length(pcQuery) - length(pcOldname)).
        end.
        else 
        do:
            pcQuery = replace(pcQuery," " + pcOldname + " "," " + pcNewName + " ").
            pcQuery = replace(pcQuery," " + pcOldname + "."," " + pcNewName + ".").
            pcQuery = replace(pcQuery,"(" + pcOldname + " ","(" + pcNewName + " ").
            pcQuery = replace(pcQuery,"(" + pcOldname + ".","(" + pcNewName + ".").
            pcQuery = replace(pcQuery,"=" + pcOldname + ".","=" + pcNewName + ".").
            // if the table somehow is at the end 
            if pcQuery matches "*" + pcOldname then 
                pcQuery = substr(pcQuery,1,length(pcQuery) - length(pcOldname)) + pcNewName.
        end.
             
        return pcQuery .
    end method. 
    
    method public static IGetDataRequest GetFindRequest(pcTable as char,input pExpression as character) :
        define variable oGetRequest as IGetDataRequest no-undo.
        define variable oGetTableRequest as GetDataTableRequest no-undo.
        oGetTableRequest = new GetDataTableRequest(pcTable).
        oGetTableRequest:QueryDefinition = new QueryDefinition(GetQueryPredicate(pExpression)).
        return new GetDataRequest(cast(Array:ToArray(oGetTableRequest),IGetDataTableRequest)).
    end method.
    
    method public static IGetDataRequest GetDataRequest(pcTable as char,pcField as char, poOperator as QueryOperatorEnum, piValue as integer) :
        return GetDataRequest(pcTable,pcField,poOperator,string(piValue)).
    end method.
    
    
    method public static IGetDataRequest GetDataRequest(pcTable as char,pcField as char, poOperator as QueryOperatorEnum, pcValue as char) :
        define variable oGetRequest as IGetDataRequest no-undo.
        define variable oGetTableRequest as GetDataTableRequest no-undo.
        oGetTableRequest = new GetDataTableRequest(pcTable).
        oGetTableRequest:QueryDefinition = new QueryDefinition(new QueryPredicate(pcField,poOperator,pcValue)).
        return new GetDataRequest(cast(Array:ToArray(oGetTableRequest),IGetDataTableRequest)).
    end method.
    
    method public static IGetDataRequest GetDataRequest(pcTable as char) :
        define variable oGetRequest as IGetDataRequest no-undo.
        define variable oGetTableRequest as GetDataTableRequest no-undo.
        oGetTableRequest = new GetDataTableRequest(pcTable).
        return new GetDataRequest(cast(Array:ToArray(oGetTableRequest),IGetDataTableRequest)).
    end method.
    
    method public static IQueryEntry GetCurrentRowKeyEntry(phBuffer as handle) :
        return GetCurrentRowKeyEntry(phBuffer,false). 
    end method.
    
    method public static IQueryEntry GetCurrentRowKeyEntry(phBuffer as handle,plUseChildKeys as logical) :
        return GetCurrentRowKeyEntry(phBuffer,plUseChildKeys,false). 
    end method.
    
    method public static IQueryEntry GetCurrentRowKeyEntry(phBuffer as handle,plUseChildKeys as logical, plPublic as logical) :
        define variable cKeys as character no-undo.
        define variable i as integer no-undo.
        define variable hFld as handle no-undo.
        define variable cValue as character no-undo.
        define variable oQueryPredicates as IQueryPredicate extent no-undo.
        // if before buffer use after-buffer key (before-buffer only has rowstate index)
        
        if plUseChildKeys then
        do:
            if plPublic then
                cKeys = ServiceInfo:GetPublicChildKeys(if valid-object(phBuffer:after-buffer) then phBuffer:after-buffer else phBuffer).
            else    
                cKeys = ServiceInfo:GetChildKeys(if valid-object(phBuffer:after-buffer) then phBuffer:after-buffer else phBuffer).
        end.
        else do:
            if plPublic then
                cKeys = ServiceInfo:GetPublicKeys(if valid-object(phBuffer:after-buffer) then phBuffer:after-buffer else phBuffer).
            else    
                cKeys = ServiceInfo:GetKeys(if valid-object(phBuffer:after-buffer) then phBuffer:after-buffer else phBuffer).
        end.    
        // for a one-to-one relation this may be valid  
        if cKeys > "" then
        do: 
            if plPublic then 
                return GetCurrentValuesFromPublicNamesEntry(phBuffer, cKeys).
            else    
                return GetCurrentValuesEntry(phBuffer,cKeys).  
        end.
        return ?.    
    end method.
    
    method public static IQueryEntry GetCurrentValuesEntry(phBuffer as handle,pcFields as character) :
        define variable oQueryPredicates as IQueryPredicate extent no-undo.
        define variable i                as integer no-undo.
        define variable hFld             as handle no-undo.
        define variable cValue           as character no-undo.
        extent(oQueryPredicates) = num-entries(pcFields).
        do i = 1 to extent(oQueryPredicates):
            hFld = phBuffer:buffer-field(entry(i,pcFields)).
            cValue = hFld:buffer-value.
            oQueryPredicates[i] = new QueryPredicate(entry(i,pcFields),QueryOperatorEnum:Eq,cValue).
        end.
        if extent(oQueryPredicates) = 1 then
            return oQueryPredicates[1].
        else 
            return new QueryGroup(oQueryPredicates).
    end method.     
    
    method public static IQueryEntry GetCurrentValuesFromPublicNamesEntry(phBuffer as handle,pcFields as character) :
        define variable oQueryPredicates as IQueryPredicate extent no-undo.
        define variable i                as integer no-undo.
        define variable hFld             as handle no-undo.
        define variable cField           as character no-undo.
        define variable cValue           as character no-undo.
        define variable cFieldMap        as character no-undo.
        extent(oQueryPredicates) = num-entries(pcFields).
        cFieldMap = BufferUtil:GetPublicToNameFieldPairs(phBuffer).
        do i = 1 to extent(oQueryPredicates):
            cField = StringUtil:GetUseName(entry(i,pcFields), cFieldMap).
            hFld = phBuffer:buffer-field(cField).
            cValue = hFld:buffer-value.
            oQueryPredicates[i] = new QueryPredicate(entry(i,pcFields),QueryOperatorEnum:Eq,cValue).
        end.
        if extent(oQueryPredicates) = 1 then
            return oQueryPredicates[1].
        else 
            return new QueryGroup(oQueryPredicates).
    end method.   
    
    method public static IGetDataTableRequest GetFindCurrentRowRequest(phBuffer as handle) :
        // default to use internal keys
        return GetFindCurrentRowRequest(phbuffer,false).  
    end method.      
    
    method public static IGetDataTableRequest GetFindCurrentRowRequest(phBuffer as handle, plPublic as logical) :
        define variable oGetTableRequest as GetDataTableRequest no-undo.
        define variable oQueryEntry      as IQueryEntry no-undo.
        oGetTableRequest = new GetDataTableRequest(phBuffer:table-handle:name).
        oGetTableRequest:FindUnique = true.
        oQueryEntry = GetCurrentRowKeyEntry(phBuffer,false,plPublic).
        if valid-object(oQueryEntry) then
        do:
            oGetTableRequest:QueryDefinition = new QueryDefinition(oQueryEntry).
            return oGetTableRequest.
        end.
        else 
            undo, throw new IllegalOperationError(subst("Cannot build find request for &1 with no key",phBuffer:name)).    
    end method.
    
    method public static IGetDataTableRequest GetFindCurrentValuesRequest(phBuffer as handle, pcFieldNames as character) :
        define variable oGetTableRequest as GetDataTableRequest no-undo.
        define variable oQueryEntry      as IQueryEntry no-undo.
        oGetTableRequest = new GetDataTableRequest(phBuffer:table-handle:name).
        oGetTableRequest:FindUnique = true.
        oQueryEntry = GetCurrentValuesEntry(phBuffer,pcFieldNames).
        if valid-object(oQueryEntry) then
        do:
            oGetTableRequest:QueryDefinition = new QueryDefinition(oQueryEntry).
            return oGetTableRequest.
        end.
        else 
            undo, throw new IllegalOperationError(subst("Cannot build find request for &1 with fields &2",phBuffer:name,StringUtil:GetMessageFromEntries(pcFieldNames))).
    end method.
    
    method public static IQueryEntry GetKeyEntry (pcString as character, phBuffer as handle) :
        define variable oKeyParser as KeyValueParser no-undo.
        oKeyParser = new KeyValueParser().
        return oKeyParser:GetKeyEntry(pcString,phbuffer,false).
    end method.  
    
    method public static IQueryEntry GetKeyEntry (pcKeyValueMap as IStringStringMap, phBuffer as handle) :
        define variable oKeyParser as KeyValueParser no-undo.
        oKeyParser = new KeyValueParser().
        return oKeyParser:GetKeyEntry(pcKeyValueMap,phbuffer,false).
    end method.  
    
    method public static IQueryEntry GetKeyEntry (pcString as character, phBuffer as handle, pHasSerializename as logical) :
        define variable oKeyParser as KeyValueParser no-undo.
        oKeyParser = new KeyValueParser().
        return oKeyParser:GetKeyEntry(pcString,phbuffer,pHasSerializename).
    end method.  
    
    method public static IQueryEntry GetKeyEntry (pcKeyValueMap as IStringStringMap, phBuffer as handle, pHasSerializename as logical) :
        define variable oKeyParser as KeyValueParser no-undo.
        oKeyParser = new KeyValueParser().
        return oKeyParser:GetKeyEntry(pcKeyValueMap,phbuffer,pHasSerializename).
    end method.  
    
    
/*    method public static IQueryEntry GetQueryEntryFromKeyExpression(pExpression as char,pFieldMap as character):                 */
/*        return GetQueryEntryFromExpression(pExpression,pFieldMap," and ").                                                       */
/*    end method.                                                                                                                  */
/*                                                                                                                                 */
/*    method private static IQueryEntry GetQueryEntryFromExpression(pExpression as char,pFieldMap as character,pDelimiter as char):*/
/*        define variable cWorkList         as character no-undo.                                                                  */
/*        define variable iPos              as integer   no-undo.                                                                  */
/*        define variable iStart            as integer no-undo.                                                                    */
/*        define variable cExpression       as character no-undo.                                                                  */
/*        define variable oCollection       as ICollection no-undo.                                                                */
/*        define variable iSpaces           as integer no-undo.                                                                    */
/*        define variable iDlmLength        as integer no-undo.                                                                    */
/*                                                                                                                                 */
/*        assign                                                                                                                   */
/*            iDlmLength  = length(left-trim(pDelimiter))                                                                          */
/*            iSpaces     = length(pDelimiter) - iDlmLength                                                                        */
/*            cWorkList   = StringUtil:MaskQuotes(pExpression,"@")                                                                 */
/*            iStart      = 1                                                                                                      */
/*            iPos        = index(cWorkList,pDelimiter) + iSpaces                                                                  */
/*            oCollection = new Collection()                                                                                       */
/*            .                                                                                                                    */
/*        if index(cWorkList," or ") > 0 then                                                                                      */
/*            undo, throw new UnsupportedOperationError("Key expression with OR operator").                                        */
/*        if index(cWorkList,"(") > 0 or index(cWorkList,")") > 0 then                                                             */
/*            undo, throw new UnsupportedOperationError("Key expression with parenthesis").                                        */
/*                                                                                                                                 */
/*        do while iPos > 0 + iSpaces :                                                                                            */
/*           cExpression = substr(pExpression,istart,iPos - iStart).                                                               */
/*           oCollection:Add(GetQueryPredicate(cExpression,pFieldMap)).                                                            */
/*           assign                                                                                                                */
/*               iStart = iPos + iDlmLength                                                                                        */
/*               iPos = index(cWorkList,pDelimiter,iStart) + iSpaces                                                               */
/*               .                                                                                                                 */
/*        end.                                                                                                                     */
/*        if istart > 1 then // append the rest                                                                                    */
/*            oCollection:Add(GetQueryPredicate(substr(pExpression,istart),pFieldMap)).                                            */
/*        else                                                                                                                     */
/*            oCollection:Add(GetQueryPredicate(pExpression,pFieldMap)).                                                           */
/*                                                                                                                                 */
/*        return new QueryGroup(cast(oCollection:ToArray(),IQueryEntry)).                                                          */
/*                                                                                                                                 */
/*    end method.                                                                                                                  */
    
    method public static IQueryPredicate GetQueryPredicate (input pExpression as character):
        return GetQueryPredicate(pExpression,"").
    end method.     
    
    method public static IQueryPredicate GetQueryPredicate (input pExpression as character,pFieldMap as char):
        define variable cWork           as character         no-undo.
        define variable iStart          as integer           no-undo.
        define variable iEnd            as integer           no-undo.
        define variable cFieldName      as character         no-undo.
        define variable oOperator       as QueryOperatorEnum no-undo.
        define variable cStringOperator as character no-undo.
        define variable cStringValue    as character no-undo.
        assign
            pExpression = trim(pExpression)
            cWork       = StringUtil:MaskQuotes(pExpression,"@")
            iStart      = index(cWork,"@")
            iEnd        = r-index(cWork,"@")
            .
        
        do while index(cWork,StringConstant:SPACE + StringConstant:SPACE) > 0:
            cWork = replace(cWork,StringConstant:SPACE + StringConstant:SPACE,StringConstant:SPACE).
        end.    
        
        if num-entries(cWork,StringConstant:SPACE) <> 3 then 
            undo, throw new IllegalArgumentError(subst("Invalid expression '&1'",pExpression)).
        
        assign
            cFieldName      = entry(1,cWork,StringConstant:SPACE)
            cStringOperator = entry(2,cWork,StringConstant:SPACE)
            oOperator       = GetOperatorEnum(entry(2,cWork,StringConstant:SPACE))
            .
        if not valid-object(oOperator) then 
            undo, throw new IllegalArgumentError(subst("Operator '&1' in expression '&2'",cStringOperator,pExpression)).    
        if iStart > 0 then    
            cStringValue = substr(pExpression,iStart + 1,iEnd - 1 - iStart).
        else 
            cStringValue = entry(3,cWork,StringConstant:SPACE).
        
        if pFieldMap > "" then
            cFieldName = StringUtil:GetUseName(cFieldName, pFieldMap). 
                 
        return new QueryPredicate(cFieldName,oOperator,cStringValue).   
    end method.
    
    method public static IQuerySortEntry extent GetQuerySort(pExpression as character):
        return GetQuerySort(pExpression,"").  
    end.         
    
    method public static character GetQuerySortAsString(pQuerySort as IQuerySortEntry extent):
        define variable i           as integer no-undo.  
        define variable cExpression as character no-undo.
        do i = 1 to extent(pQuerySort):
            cExpression = cExpression 
                        + (if cExpression = "" then "" else " ")
                        + pQuerySort[i]:ToString().
        end.
        return cExpression.    
    end method.    
    
    method public static IQuerySortEntry extent GetQuerySort(pExpression as character,pFieldMap as character):
        define variable i                 as integer     no-undo.
        define variable cExpression       as character   no-undo.
        define variable cField            as character   no-undo.       
        define variable oCollection       as ICollection no-undo.
        define variable iCount            as integer     no-undo.
        define variable cWord             as character   no-undo.
        define variable oSortEntries      as IQuerySortEntry extent no-undo.
        define variable oSortDirection    as SortOrderEnum no-undo.
        define variable iNumWords         as integer no-undo.
        define variable cMaskedExpression as character no-undo.
        define variable iByPos            as integer no-undo.
        define variable iIndexRPos        as integer no-undo.
        
        if pExpression > "" then
        do:
            assign
                pExpression = trim(pExpression)
                oCollection = new Collection()
                .
             
            cMaskedExpression = StringUtil:MaskQuotes(pExpression,"@").
            iByPos = index(" " + cMaskedExpression + " ":U," BY ":U).
            if iByPos > 0 then
            do:
                cMaskedExpression  = trim(substr(pExpression,iByPos)).
                iIndexRPos = StringUtil:FindAnyWord(cMaskedExpression,"indexed-reposition").
                if iIndexRPos > 0 then 
                do:
                    substring(cMaskedExpression,iIndexRPos,length("indexed-reposition") + 1) = "".
                end.
                /* Remove extra blanks (this may be used to compare new and old 
                   sort expression so it need to be consistent)*/
                do while index(cExpression,'  ':U) > 0:
                    cMaskedExpression = replace(cExpression,'  ':U,' ':U).
                end.
                do while index(pExpression,StringConstant:SPACE + StringConstant:SPACE) > 0:
                    cMaskedExpression = replace(cMaskedExpression,StringConstant:SPACE + StringConstant:SPACE,StringConstant:SPACE).
                end.
               
                iNumWords = num-entries(cMaskedExpression,StringConstant:SPACE).
                
                do i = 1 to iNumWords:
                    cWord = entry(i,cMaskedExpression,StringConstant:SPACE).
                    if cWord = "by" then 
                    do:
                        assign
                            iCount = 1
                            cField = ""
                            oSortDirection = ?.
                    end.
                    else do:
                        iCount = iCount + 1.
                        if iCount = 2 then
                        do:
                           cField = cWord.
                        end.
                        else if iCount = 3 then do:
                            case cWord:
                                when "desc" or 
                                when "desce" or
                                when "descen" or
                                when "descend" or
                                when "descend" or
                                when "descendi" or
                                when "descendin" or
                                when "descending" then
                                    oSortDirection = SortOrderEnum:Descending.
                                otherwise 
                                    undo, throw new IllegalArgumentError(subst("Invalid sort expression &1",pExpression)).
                            end case.     
                        end.
                        else 
                            undo, throw new IllegalArgumentError(subst("Invalid sort expression &1",pExpression)).
                    end.
                    if (i < iNumWords and entry(i + 1,cMaskedExpression,StringConstant:SPACE) = "by")
                    or  i = iNumWords then 
                    do: 
                        if cField > "" then
                        do:
                            if pFieldMap > "" then
                                cField = StringUtil:GetUseName(cField, pFieldMap).  
                            if valid-object(oSortDirection) then     
                                oCollection:Add(new QuerySortEntry(cField,oSortDirection)).
                            else
                                oCollection:Add(new QuerySortEntry(cField)).
                        end.   
                        
                    end.    
                end.
                
                return cast(oCollection:ToArray(),IQuerySortEntry).
            end.
        end.
        return oSortEntries.  // no extents  
    end.         
    
    // replaces commas with 'and' operator while allowing commas in quoted value   
    method public static IQuerySortEntry extent GetQuerySortFromDelimitedExpression(plist as char):
        return GetQuerySortFromDelimitedExpression(pList,"").
    end method.
    
    
    // replaces commas with 'and' operator while allowing commas in quoted value   
    method public static IQuerySortEntry extent GetQuerySortFromDelimitedExpression(plist as char,pFieldMap as character):
        define variable i              as integer   no-undo.
        define variable cExpression    as character no-undo.
        define variable oSortDirection as SortOrderEnum no-undo.
        define variable iEntries       as integer   no-undo.
        define variable cField         as character no-undo.       
        define variable cTTField   as character no-undo.       
        define variable oCollection    as ICollection no-undo.
        
        assign
            pList       = trim(pList)
            oCollection = new Collection()
            .
        do while index(pList,StringConstant:SPACE + StringConstant:SPACE) > 0:
            pList = replace(pList,StringConstant:SPACE + StringConstant:SPACE,StringConstant:SPACE).
        end. 
        do i = 1 to num-entries(pList):
            assign
                cExpression = entry(i,pList)
                iEntries    = num-entries(cExpression,StringConstant:SPACE)
                .
            if iEntries = 1 then
                assign
                    cField         = cExpression
                    oSortDirection = SortOrderEnum:Ascending
                    .
            else if iEntries = 2 then
                assign
                    cField          = entry(1,cExpression,StringConstant:SPACE)
                    oSortDirection  = if entry(2,cExpression,StringConstant:SPACE) = "desc" then SortOrderEnum:Descending else SortOrderEnum:Ascending
                    .
                    
             if pFieldMap > "" then
                 cField = StringUtil:GetUseName(cField, pFieldMap).
             oCollection:Add(new QuerySortEntry(cField,oSortDirection)).
        end.
        return cast(oCollection:ToArray(),IQuerySortEntry).            
    end method.
         
    method public static QueryOperatorEnum GetOperatorEnum(pOperator as char):
        case pOperator:
            when "=" or
            when "eq" then 
                return QueryOperatorEnum:Eq.
            when "<>" or
            when "ne" then 
                return QueryOperatorEnum:Ne.
            when ">" or
            when "gt" then 
                return QueryOperatorEnum:Gt.
            when ">=" or
            when "ge" then 
                return QueryOperatorEnum:Ge.
            when "<" or
            when "lt" then 
                return QueryOperatorEnum:Lt.
            when "<=" or
            when "le" then 
                return QueryOperatorEnum:Le.
            when "begins" or
            when "startswith" then 
                return QueryOperatorEnum:Begins.
            when "matches" then
                return QueryOperatorEnum:Matches.
        end.    
        return ?.
    end method.    
    
    method public static character GetOperatorText(pOperator as char):
        case pOperator:
            when "=" or
            when "eq" then 
                return "equals".
            when "<>" or
            when "ne" then 
                return "not equals".
            when ">" or
            when "gt" then 
                return "greater than".
            when ">=" or
            when "ge" then 
                return "greater equals".
            when "<" or
            when "lt" then 
                return "less than".
            when "<=" or
            when "le" then 
                return "less equals".
            when "begins" or
            when "startswith" then 
                return "begins".
            when "matches" then
                return pOperator.
        end.    
        return pOperator.
    end method.    
    
    method public static character JoinString(pJoin as JoinEnum, pFirst as logical):
        define variable cValue as character no-undo.
        Assert:NotNull(pJoin,"Join").
        
        case pJoin:
            when JoinEnum:And or
            when JoinEnum:Not or
            when JoinEnum:Or then
                cValue = pJoin:ToString ().
            when JoinEnum:AndNot then
                cValue = "and not".
            when JoinEnum:OrNot then
                cValue = "or not".
            when JoinEnum:None then
                cValue = "".
        end case.
        if pFirst then 
        do:
            if cValue begins "and" then
               cValue = trim(replace(cValue,"and","")).
            if cValue begins "or" then
               cValue = trim(replace(cValue,"or","")).   
        end.  
        else if cValue = "" then
            cValue = "and". 
        return cValue.                  
    end method.
    
    method public static character RemoveSort(pcQueryString as character):
        return SetSort(pcQueryString,"").
    end method.
    
    method public static character RemoveQueryOptions(pcQueryString as character):
        define variable cWord     as character no-undo.
        define variable cTmpQuery     as character no-undo.
        define variable iPos          as integer no-undo.
        
        pcQueryString = RemoveSort(pcQueryString).
        pcQueryString = pcQueryString + StringConstant:SPACE.
        // add space so we can find word at end - trimmed before return 
        cTmpQuery = StringUtil:MaskQuotes(pcQueryString,"@").
        iPos = StringUtil:FindAnyWord(cTmpQuery,LockList,output cWord).
        if iPos > 0 then 
        do:
            // keep in synch
            // remove 1 extra since there is space before and after
            substring(cTmpQuery,iPos,length(cWord) + 1) = "".
            substring(pcQueryString,iPos,length(cWord) + 1) = "".
        end.    
        iPos = StringUtil:FindAnyWord(cTmpQuery,"indexed-reposition").
        if iPos > 0 then 
        do:
            // keep in synch
            substring(cTmpQuery,iPos,length("indexed-reposition") + 1) = "".
            substring(pcQueryString,iPos,length("indexed-reposition") + 1) = "".
        end.
        iPos = StringUtil:FindAnyWord(cTmpQuery,"no-prefetch").
        if iPos > 0 then 
        do:
            // keep in synch
            substring(cTmpQuery,iPos,length("no-prefetch") + 1) = "".
            substring(pcQueryString,iPos,length("no-prefetch") + 1) = "".
        end.
        iPos = StringUtil:FindAnyWord(cTmpQuery,"OUTER-JOIN").
        if iPos > 0 then
        do:  
            undo, throw new UnsupportedOperationError(subst("Removal of query options in query with '&1'. Use RemoveOuterJoin instead.","OUTER-JOIN")).
        end. 
            
        // NOTE: Getting this error from the framework likely indicates bad/conflcting use of BaseQuery  
        // The options should rather be added to the query later. 
        // Consider adding utility functions to add these if necessary 
        iPos = StringUtil:FindAnyWord(cTmpQuery,"MAX-ROWS,LEFT,OF,USE-INDEX,TENANT-WHERE",output cWord).
        if iPos > 0 then
        do:  
            undo, throw new UnsupportedOperationError(subst("Removal of query options in query with '&1'",cWord)).
        end.
        return trim(pcQueryString).
    end method.
    
    // set sort in query expression - removes existing sort if present
    method public static character SetSort(pcQueryString as character,pcSort as char):
        define variable iByPos      as integer     no-undo.
        define variable iBreakPos      as integer     no-undo.
        define variable cExpression as character   no-undo.
        define variable cTmpQuery   as character   no-undo.
        define variable iIndexpos   as integer     no-undo.
        define variable iMaxrowspos as integer     no-undo.
        define variable iKeepPos    as integer     no-undo.
        define variable iLength     as integer     no-undo.
        /* We mask quoted strings to ensure the following BY keyword lookup
           only finds stuff in the expression(in lack of parsing) */ 
        if pcsort > "" and (left-trim(pcSort) begins "by ") = false then
            pcSort = "by " + pcSort.    
        assign
            cTmpQuery   = StringUtil:MaskQuotes(pcQueryString,"@")
            iByPos      = index(cTmpQuery + StringConstant:SPACE," by ":U)
            iBreakPos      = index(cTmpQuery + StringConstant:SPACE," break ":U)
            iIndexpos   = index(cTmpQuery + StringConstant:SPACE," indexed-reposition ":U)
            iMaxRowspos = index(cTmpQuery + StringConstant:SPACE," max-rows ":U)
            iLength     = length(pcQueryString).
            
        if iIndexpos > 0 and iMaxrowspos > 0 then
           iKeepPos  = min(iMaxRowspos,iIndexpos).
        else if iIndexpos > 0 then    
           iKeepPos = iIndexpos.
        else if iMaxrowspos > 0 then
            iKeepPos = iMaxRowsPos.
        
        if iBreakpos > 0 then 
           iByPos = iBreakPos.
            
        if iKeepPos > 0 and iByPos > 0 then 
        do:
                                                                // left-trim to remove right space if blank 
            substr(pcQueryString,iByPos + 1,iKeeppos - iBypos) = left-trim(pcSort + StringConstant:SPACE).
        end.
        else if iKeepPos > 0 then 
        do:
            // keeppos + 1 since left space is part of index
            substr(pcQueryString,iKeeppos + 1,0) = left-trim(pcSort + StringConstant:SPACE).
        end.
        else if iByPos > 0 then
        do:
                                                                // right-trim to remove left space if blank
            pcQueryString = substr(pcQueryString,1,iByPos - 1) + right-trim(StringConstant:SPACE + pcSort).
        end.
        else if pcSort > "" then
        do:   
            pcQueryString = pcQueryString + StringConstant:SPACE + pcSort.
        end.    
        return pcQueryString.  
    end method.
   
    method public static character InsertQueryExpressions(pWhere as  char, pExpressions as char extent):
        define variable cTableWheres   as character extent no-undo.
        define variable i              as integer no-undo.
        define variable cNewExpression as character no-undo.
        define variable cDLm           as character no-undo.
        
        cTableWheres = SplitQuery(pWhere).
        if extent(cTableWheres) <> extent(pExpressions) then 
            undo, throw new IllegalArgumentError(subst("The expression has &1 extents, but the query '&2' references &3 buffers",extent(pExpressions),pWhere,extent(cTableWheres))).
       
        do i = 1 to extent(cTableWheres):
            cTableWheres[i] = InsertQueryExpression(cTableWheres[i],pExpressions[i]).
            cNewExpression = cNewExpression + cDlm + cTableWheres[i].
            cDlm = ", ". 
        end. 
        return cNewExpression.
    end method.
    
    method public static character RemoveOuterJoin(pcTable as char, pcQueryString as  char):
        define variable cTableWheres   as character extent no-undo.
        define variable i              as integer no-undo.
        define variable cBuffer        as character no-undo.
        define variable cTmpQuery      as character no-undo.
        define variable iPos           as integer no-undo.
        cTableWheres = SplitQuery(pcQueryString).
        cTmpQuery = StringUtil:MaskQuotes(pcQueryString, "@").
        do i = 1 to extent(cTableWheres):
            cBuffer = WhereClauseBuffer(cTableWheres[i]).
            if cBuffer = pcTable then 
            do:
                iPos = StringUtil:FindAnyWord(cTmpQuery,"outer-join").
                if iPos > 0 then 
                do:
                    substring(pcQueryString,iPos,length("outer-join") + 1) = "".
                    return pcQueryString.
                end.
            end.
        end. 
        return pcQueryString.
    end method.
    
    
    method public static character InsertQueryExpression(pcTable as char, pcWhere as  char, pcExpression as char):
        define variable cTableWheres   as character extent no-undo.
        define variable i              as integer no-undo.
        define variable cNewExpression as character no-undo.
        define variable cDLm           as character no-undo.
        define variable cBuffer        as character no-undo.
        define variable lFound         as logical no-undo.
        cTableWheres = SplitQuery(pcWhere).
        do i = 1 to extent(cTableWheres):
            if not lFound then 
            do:
                cBuffer = WhereClauseBuffer(cTableWheres[i]).
                if cBuffer = pcTable then 
                do:
                    cTableWheres[i] = InsertQueryExpression(cTableWheres[i],pcExpression).
                    lFound = true.
                end.
            end.
            cNewExpression = cNewExpression + cDlm + cTableWheres[i].
            cDlm = ", ". 
        end. 
        return cNewExpression.
    end method.
    
    method public static character SetMaxRows(pcWhere as  char, piMax as integer):
        define variable cTmpQuery  as character no-undo.
        define variable iMaxRowPos as integer   no-undo.
        define variable cWhere1    as character no-undo.
        define variable cWhere2    as character no-undo.
        define variable iSplit     as integer   no-undo.
        define variable iSpace     as integer no-undo.
        
        cTmpQuery   = StringUtil:MaskQuotes(pcWhere,'@').
        iMaxRowPos  = StringUtil:MinNotZeroPosition(cTmpQuery,"MAX-ROWS").
        if iMaxRowPos > 0 then
        do:
            if piMax = 0 then
            do:
                assign
                    iSplit = iMaxRowPos + length("max-rows")
                    cWhere1 = substr(pcWhere,1,iMaxRowPos - 2)
                    cWhere2 = left-trim(substr(pcWhere,iSplit)) + StringConstant:SPACE
                    iSpace  = index(cWhere2,StringConstant:SPACE)
                    cWhere2 = substr(cWhere2,iSpace) 
                    .
             
                return subst("&1&2",cWhere1,cWhere2).
            end.
            else do:      
                assign
                    iSplit = iMaxRowPos + length("max-rows")
                    cWhere1 = substr(pcWhere,1,iSplit - 1)
                    cWhere2 = left-trim(substr(pcWhere,iSplit)) + StringConstant:SPACE
                    iSpace  = index(cWhere2,StringConstant:SPACE)
                    cWhere2 = substr(cWhere2,iSpace) 
                    .
                return subst("&1 &2&3",cWhere1,piMax,cWhere2).
            end.          
        end.
        else if piMax > 0 then 
            return right-trim(pcWhere) + subst(" max-rows &1",piMax).          
        else
            return pcWhere.    
    end method.    
    
    method public static character InsertQueryExpression(pcWhere as  char, pcExpression as char):
        define variable iInsertPosition as integer   no-undo.
        define variable lHasWhereClause as logical   no-undo.
        define variable iEndWhereClause as integer   no-undo.
        
        if pcExpression > "" then
        do: 
            iInsertPosition = GetInsertPosition(pcWhere, output lHasWhereClause, output iEndWhereClause).
            if lHasWhereClause then
            do:  
                // Add or insert END parenthesis for existing where clause 
                if iEndWhereClause > 0 then
                    substring(pcWhere,iEndWhereClause - 1,0) = ")". 
                else 
                    pcWhere  = pcWhere + ")".
                // insert the new one before the old with "and ("       
                substring(pcWhere,iInsertPosition,0) = pcExpression + " and (". 
            end.
            else do:
                pcExpression = " where " + pcExpression.  
                if iInsertPosition < length(pcWhere) then
                    substring(pcWhere,iInsertPosition,0) = pcExpression.
                else
                    pcWhere = pcWhere + pcExpression.
            end.
        end. 
        return pcWhere.
    end method.
    
    method public static character xInsertQueryExpression(pcWhere as  char, pcExpression as char):
        /*------------------------------------------------------------------------------
         Purpose:     Inserts an expression into ONE buffer's where-clause.
         Parameters:  
              pcWhere      - Complete where clause with or without the FOR keyword,
                             but without any comma before or after.
              pcExpression - New expression OR OF phrase (Existing OF phrase is replaced)
         Notes:     - The new expression is embedded in parenthesis, but no parentheses
                      are placed around the existing one.  
                    - Lock keywords must be unabbreviated or without -lock (i.e. SHARE
                      or EXCLUSIVE.)   
                    - Cannot hsve comments.              
        ------------------------------------------------------------------------------*/  
        define variable cTable              as character no-undo.  
        define variable cWhereOrAnd         as character no-undo.  
        define variable iTablePos           as integer   no-undo.
        define variable iWherePos           as integer   no-undo.
        define variable cTmpQuery           as character no-undo.
        define variable iInsertPos          as integer   no-undo.
        define variable lHasWhere           as logical   no-undo.
        define variable cNewExpression      as character no-undo.
        define variable iEndRecordPhrase    as integer no-undo.
        define variable iLockEndPosition    as integer no-undo.
        define variable iLockStartPosition  as integer no-undo.
        define variable iEndWhereClause     as integer no-undo.
        define variable iRecordPhraseOption as integer no-undo.
        define variable cRecordPhraseOptions as character init "OUTER-JOIN,LEFT,OF,USE-INDEX,NO-PREFETCH,TENANT-WHERE" no-undo.
        if pcExpression > "" then
        do:
            
            assign
                /* Mask quoted strings to ensure the following table and keyword lookup
                   only finds stuff in the expression(in lack of parsing) */ 
                cTmpQuery   = StringUtil:MaskQuotes(pcWhere,'@')
                iWherePos   = index(cTmpQuery," where ")
                // lazy coders pay the prize 
                . 
                                 
            if iWherePos > 0 then
            do:
                 iInsertPos = iWherepos + 7. 
                 iEndRecordPhrase = StringUtil:MinNotZeroPosition(cTmpQuery,Pmfo.Util.QueryUtil:RecordphraseList).
                 iLockStartPosition = StringUtil:FindAnyWord(cTmpQuery,LockList).
                 iRecordPhraseOption = StringUtil:MinNotZeroPosition(cTmpQuery,cRecordPhraseOptions,iInsertpos).
                  
                 if iLockStartPosition < iWherePos then
                 do: 
                     if iRecordPhraseOption > 0 then
                         iEndWhereClause = iRecordPhraseOption.
                     else 
                         iEndWhereClause = iEndRecordPhrase.
                 end.
                 else do:
                     if iLockStartPosition > 0 and iRecordPhraseOption > 0 then
                     do:
                         iEndWhereClause = min(iLockStartPosition,iRecordPhraseOption).
                     end.
                     else if iLockStartPosition > 0 then
                         iEndWhereClause = iLockStartPosition.
                     else if iRecordPhraseOption > 0 then
                         iEndWhereClause = iRecordPhraseOption.
                     else  
                         iEndWhereClause = iEndRecordPhrase.                        
                 end.    
                    
                 if iEndWhereClause > 0 then
                 do:
                     substring(pcWhere,iEndWhereClause - 1,0) = ")". 
                     substring(pcWhere,iInsertPos,0) = pcExpression + " and (". 
                 end.
                 else do:
                     pcWhere  = pcWhere + ")".
                     substring(pcWhere,iInsertPos,0) =  pcExpression + " and (". 
                 end.       
            end.
            else do:
                iRecordPhraseOption = StringUtil:MinNotZeroPosition(cTmpQuery,cRecordPhraseOptions).
                iLockEndPosition = StringUtil:FindAnyWordEnd(cTmpQuery,LockList).
                pcExpression = " where " + pcExpression. 
                if iLockEndPosition > 0 and (iLockEndPosition < iRecordPhraseoption or iRecordPhraseoption = 0)  then
                do: 
                     iInsertPos = iLockEndPosition.
                     substring(pcWhere,iInsertPos,0) = pcExpression.
                end.
                else if iRecordPhraseOption > 0 then 
                do:
                     iInsertPos = iRecordPhraseOption - 1.
                     substring(pcWhere,iInsertPos,0) = pcExpression.
                end.
                else do:
                     assign 
                         cTable    = WhereClauseBuffer(cTmpQuery)
                         iTablePos = index(cTmpQuery + StringConstant:SPACE,StringConstant:SPACE + cTable + StringConstant:SPACE) + 1
                         iInsertpos = iTablePos + length(cTable).
                     if iInsertPos < length(pcWhere) then
                     do: 
                         substring(pcWhere,iInsertPos,0) = pcExpression. 
                     end.
                     else 
                         pcWhere = pcWhere + pcExpression. 
                end. 
            end.                                    
        end.
        return pcWhere.
           
    end method. 
    
    method public static character StripQueryRecordphrase(pcQuery as  char):
        define variable cDummy as character no-undo.
        return StripQueryRecordphrase(pcQuery, false, output cDummy).  
    end method.    
    
    method public static character StripQueryRecordphrase(pcQuery as  char, output pcRecordPhrase as character):
        return StripQueryRecordphrase(pcQuery, true, output pcRecordPhrase).
    end method.  
    
    method private static character StripQueryRecordphrase(pcQuery as  char, plGetrecordphrase as logical, output pcRecordPhrase as character):
        define variable iEndRecordPhrase as integer   no-undo.
        define variable cTmpQuery        as character no-undo.
        cTmpQuery = StringUtil:MaskQuotes(pcQuery,'@').
        iEndRecordPhrase = StringUtil:MinNotZeroPosition(cTmpQuery,Pmfo.Util.QueryUtil:RecordphraseList).
        if iEndRecordPhrase > 0 then
        do:
            if plGetrecordphrase then
                pcRecordPhrase = right-trim(substring(pcQuery,iEndRecordPhrase)).
            return right-trim(substring(pcQuery,1,iEndRecordPhrase - 1)). 
        end.
        else 
            return pcQuery.
    end method. 
    
    method public static character GetQueryRecordphrase(pcQuery as  char):
        define variable iEndRecordPhrase as integer   no-undo.
        define variable cTmpQuery        as character no-undo.
        cTmpQuery = StringUtil:MaskQuotes(pcQuery,'@').
        iEndRecordPhrase = StringUtil:MinNotZeroPosition(cTmpQuery,Pmfo.Util.QueryUtil:RecordphraseList).
        if iEndRecordPhrase > 0 then
            return right-trim(substring(pcQuery,iEndRecordPhrase)). 
        else 
            return "".
    end method.    
    
    
    method public static integer GetInsertPosition(pcWhere as  char, output plHasWhere as logical, output piEndWhereClause as integer):
        /*------------------------------------------------------------------------------
         Purpose:     Get the position for inserting a where clsue into ONE buffer's where-clause.
         Parameters:  
              pcWhere      - Complete where clause with or without the FOR keyword,
                             but without any comma before or after.
              pcExpression - New expression OR OF phrase (Existing OF phrase is replaced)
         Notes:     - The new expression is embedded in parenthesis, but no parentheses
                      are placed around the existing one.  
                    - Lock keywords must be unabbreviated or without -lock (i.e. SHARE
                      or EXCLUSIVE.)   
                    - Cannot hsve comments.              
        ------------------------------------------------------------------------------*/  
        define variable cTable              as character no-undo.  
        define variable cWhereOrAnd         as character no-undo.  
        define variable iTablePos           as integer   no-undo.
        define variable iWherePos           as integer   no-undo.
        define variable cTmpQuery           as character no-undo.
        define variable iInsertPos          as integer   no-undo.
        define variable lHasWhere           as logical   no-undo.
        define variable cNewExpression      as character no-undo.
        define variable iEndRecordPhrase    as integer no-undo.
        define variable iLockEndPosition    as integer no-undo.
        define variable iLockStartPosition  as integer no-undo.
       
        define variable iRecordPhraseOption as integer no-undo.
        define variable cRecordPhraseOptions as character init "OUTER-JOIN,LEFT,OF,USE-INDEX,NO-PREFETCH,TENANT-WHERE" no-undo.
            
            assign
                /* Mask quoted strings to ensure the following table and keyword lookup
                   only finds stuff in the expression(in lack of parsing) */ 
                cTmpQuery   = StringUtil:MaskQuotes(pcWhere,'@')
                iWherePos   = index(cTmpQuery," where ")
                // lazy coders pay the prize 
                . 
                                 
            if iWherePos > 0 then
            do:
                 iInsertPos = iWherepos + 7. 
                 iEndRecordPhrase = StringUtil:MinNotZeroPosition(cTmpQuery,"BY,BREAK,INDEXED-REPOSITION,MAX-ROWS").
                 iLockStartPosition = StringUtil:FindAnyWord(cTmpQuery,LockList).
                 iRecordPhraseOption = StringUtil:MinNotZeroPosition(cTmpQuery,cRecordPhraseOptions,iInsertpos).
                  
                 if iLockStartPosition < iWherePos then
                 do: 
                     if iRecordPhraseOption > 0 then
                         piEndWhereClause = iRecordPhraseOption.
                     else 
                         piEndWhereClause = iEndRecordPhrase.
                 end.
                 else do:
                     if iLockStartPosition > 0 and iRecordPhraseOption > 0 then
                     do:
                         piEndWhereClause = min(iLockStartPosition,iRecordPhraseOption).
                     end.
                     else if iLockStartPosition > 0 then
                         piEndWhereClause = iLockStartPosition.
                     else if iRecordPhraseOption > 0 then
                         piEndWhereClause = iRecordPhraseOption.
                     else  
                         piEndWhereClause = iEndRecordPhrase.                        
                 end.    
                    
/*                 if piEndWhereClause > 0 then                       */
/*                 do:                                                */
/*                     substring(pcWhere,iEndWhereClause - 1,0) = ")".*/
/*                 end.                                               */
/*                 else do:                                           */
/*                     pcWhere  = pcWhere + ")".                      */
/*                 end.                                               */
                 plHasWhere = true.
               //  substring(pcWhere,iInsertPos,0) = pcExpression + " and (". 
                   
            end.
            else do:
                iRecordPhraseOption = StringUtil:MinNotZeroPosition(cTmpQuery,cRecordPhraseOptions).
                iLockEndPosition = StringUtil:FindAnyWordEnd(cTmpQuery,LockList).
            //    pcExpression = " where " + pcExpression. 
                if iLockEndPosition > 0 and (iLockEndPosition < iRecordPhraseoption or iRecordPhraseoption = 0)  then
                do: 
                     iInsertPos = iLockEndPosition.
               //      substring(pcWhere,iInsertPos,0) = pcExpression.
                end.
                else if iRecordPhraseOption > 0 then 
                do:
                     iInsertPos = iRecordPhraseOption - 1.
                //     substring(pcWhere,iInsertPos,0) = pcExpression.
                end.
                else do:
                     assign 
                         cTable    = WhereClauseBuffer(cTmpQuery)
                         iTablePos = index(cTmpQuery + StringConstant:SPACE,StringConstant:SPACE + cTable + StringConstant:SPACE) + 1
                         iInsertpos = iTablePos + length(cTable).
/*                     if iInsertPos < length(pcWhere) then               */
/*                     do:                                                */
/*                         substring(pcWhere,iInsertPos,0) = pcExpression.*/
/*                     end.                                               */
/*                     else                                               */
/*                         pcWhere = pcWhere + pcExpression.              */
                 end.                                                     
            end.                                    
            return iInsertPos.
        //return pcWhere.
           
    end method. 
    
    method public static character WhereClauseBuffer(pcWhere as character):
  
        /*------------------------------------------------------------------------------
          Purpose:     Returns the buffername of a where clause expression. 
                       This function avoids problems with leading or double blanks in 
                       where clauses.
          Parameters:
            pcWhere - Complete where clause for ONE table with or without the FOR 
                      keyword. The buffername must be the second token in the
                      where clause as in "EACH order OF Customer" or if "FOR" is
                      specified, the third token as in "FOR EACH order".
        ------------------------------------------------------------------------------*/
        define variable cDoubleSpace as character no-undo.
        
        cDoubleSpace = StringConstant:SPACE + StringConstant:SPACE.
        pcWhere = left-trim(pcWhere).
  
        /* Remove double blanks */
        do while index(pcWhere,cDoubleSpace) > 0:
            pcWhere = replace(pcWhere,cDoubleSpace,StringConstant:SPACE).
        end.
        /* Get rid of potential line break characters */   
        pcWhere = replace(pcWhere,StringConstant:LF,'':U). 

       return (if num-entries(pcWhere,StringConstant:SPACE) > 1 
               then entry(if pcWhere begins "FOR ":U or pcWhere begins "PRESELECT " then 3 else 2,pcWhere,StringConstant:SPACE)
               else "":U).
  
    end method.  
    
    method public static character extent SplitQuery(pcWhere as character):
        define variable cWorkQuery    as character no-undo.
        define variable iPos          as integer   no-undo.
        define variable iNextPos      as integer   no-undo.
        define variable iCount        as integer no-undo.
        define variable cExpressions  as character no-undo extent.
        define variable cFixedExpressions as character no-undo extent.
        define variable lRecount      as logical no-undo.
        define variable i             as integer no-undo.
        define variable cExpression   as character no-undo.
        cWorkQuery = StringUtil:MaskQuotes(pcWhere, "@").
        extent(cExpressions) = num-entries(cWorkQuery).
        do while true:
            iNextPos = index(cWorkQuery,",",iPos + 1).    
            iCount = iCount + 1.
            if iNextPos > 0 then 
            do:
                cExpression = trim(substr(pcWhere,iPos + 1,iNextPos - (iPos + 1))).
                iPos = iNextPos.
            end.
            else 
            do:
                cExpression = trim(substr(pcWhere,iPos + 1)).
            end.
            
            // check if the split was due to a comma in the expression and put it back together if it was*/      
            if iCount > 1 and lookup(entry(1,left-trim(cExpression),StringConstant:SPACE),"each,first,last") = 0 then
            do.
                cExpressions[iCount - 1] = cExpressions[iCount - 1] + "," + cExpression.
                iCount = iCount - 1.
            end.
            else 
                cExpressions[iCount] = cExpression.
                            
            if iNextpos = 0 then leave.
        end.
        
        if iCount < extent(cExpressions) then
        do:     
            extent(cFixedExpressions) = iCount.
            do i = 1 to extent(cFixedExpressions):
               cFixedExpressions[i] = cExpressions[i]. 
            end.      
            return cfixedExpressions.
        end.
        else 
            return cExpressions.
    end method.  
    
     // return 
    // - true if there is a queryexpression for non-blank value
    // - false if there is an expression for blank value
    // ? if field is not referenced 
        
    method public static logical QueryEqualsNotEmpty(pRequest as IGetDataTableRequest,pcfield as character,pcDatatype as character):
        define variable lEquals as logical no-undo.
        return QueryEqualsNotEmpty(pRequest,pcField,pcDataType,false,true,output lEquals).
    end method.  
    
     // return 
    // - true if there is a queryexpression for non-blank value
    // - false if there is an expression for blank value
    // ? if field is not referenced 
        
    method public static logical QueryEqualsNotEmpty(pRequest as IGetDataTableRequest,pcfield as character,pcDatatype as character,plUnknownIsEmpty as logical):
        define variable lEquals as logical no-undo.
        return QueryEqualsNotEmpty(pRequest,pcField,pcDataType,plUnknownIsEmpty,true,output lEquals).
    end method.  
    
    // return 
    // - true if there is a queryexpression for non-blank value
    // - false if there is an expression for blank value
    // ? if field is not referenced 
        
    method public static logical QueryEqualsNotEmpty(pRequest as IGetDataTableRequest,pcfield as character,pcDatatype as character, output pEqualsOperator as logical):
        return QueryEqualsNotEmpty(pRequest,pcField,pcDataType,false,true,output pEqualsOperator).
    end method.  
    
    // return 
    // - true if there is a queryexpression for non-empty value 
    // - false if there is an expression for emtpy value 
    // ? if field is not referenced 
    method public static logical QueryEqualsNotEmpty(pRequest as IGetDataTableRequest,pcfield as character,pcDatatype as character, plUnknownIsEmpty as logical,output pEqualsOperator as logical):
        return QueryEqualsNotEmpty(pRequest,pcField,pcDataType,plUnknownIsEmpty,true,output pEqualsOperator).
    end method.  
    
    // return  
    // - true if there is a queryexpression for non-empty value 
    // - false if there is an expression for emtpy value 
    // ? if field is not referenced 
    // optionally delete expression if found 
    method public static logical QueryEqualsNotEmpty(pRequest as IGetDataTableRequest,pcfield as character,pcDatatype as character, plUnknownIsEmpty as logical, plRemoveIfempty as logical,output pEqualsOperator as logical):
        define variable oQueryEntries as IQueryEntry extent no-undo.
        define variable oQueryPredicate as IQueryPredicate no-undo.
        define variable lResult as logical no-undo.
        
        oQueryEntries = pRequest:GetQueryEntries(pcfield).
        
        if extent(oQueryEntries) > 0 then
        do:  
            if extent(oQueryEntries) = 1 then 
                oQueryPredicate = cast(oQueryEntries[1],IQueryPredicate) no-error.
            if valid-object(oQueryPredicate) then
            do:
                lResult = QueryEqualsNotEmpty(oQueryPredicate,pcDataType,plUnknownIsEmpty,output pEqualsOperator).
                if lResult = false and plRemoveIfempty then
                    pRequest:RemoveQueryEntry(oQueryPredicate).
                return lResult.    
            end.         
            undo, throw new UnsupportedOperationError(subst("Query with &1 is too complex. Cannot resolve expressions with OR for this field.",pcField)). 
        end.
        return ?.
    end method.
    
    method public static logical QueryEqualsNotEmpty(pQueryPredicate as IQueryPredicate,pcDatatype as character, plUnknownIsEmpty as logical,output pEqualsOperator as logical):
        define variable llndexedOperator as logical no-undo.
        return QueryExcludesEmpty(pQueryPredicate, pcDatatype, plUnknownIsEmpty, false, output pEqualsOperator, output llndexedOperator).         
    end method. 
    
    method public static logical QueryExcludesBlank(pQueryPredicate as IQueryPredicate, output pEqualsOperator as logical, output plndexedOperator as logical):
        return QueryExcludesEmpty(pQueryPredicate, "character", false, true, output pEqualsOperator, output plndexedOperator).         
    end method.
    
    method public static logical QueryExcludesBlank(pQueryPredicate as IQueryPredicate):
        define variable lEqualsOperator as logical no-undo.
        define variable llndexedOperator as logical no-undo.
        return QueryExcludesEmpty(pQueryPredicate, "character", false, true, output lEqualsOperator, output llndexedOperator).         
    end method.
    
    method public static logical QueryExcludesZero(pQueryPredicate as IQueryPredicate, pcDatatype as character):
        define variable lEqualsOperator as logical no-undo.
        define variable llndexedOperator as logical no-undo.
        if lookup(pcDataType,"integer,int64,decimal") = 0 then 
             undo, throw new IllegalArgumentError(subst("Datatype &1",pcDataType)).
        return QueryExcludesEmpty(pQueryPredicate, 
                                 pcDatatype, 
                                 false,  // unknownisempty false - difficult to resolve rangfe with null -   
                                 true, // range
                                 output lEqualsOperator, output llndexedOperator).         
    end method.
    
    
    method public static logical QueryExcludesZero(pQueryPredicate as IQueryPredicate, pcDatatype as character, output pEqualsOperator as logical, output plndexedOperator as logical):
        if lookup(pcDataType,"integer,int64,decimal") = 0 then 
             undo, throw new IllegalArgumentError(subst("Datatype &1",pcDataType)).
        return QueryExcludesEmpty(pQueryPredicate, 
                                 pcDatatype, 
                                 false, // unknownisempty false - difficult to resolve rangfe with null -   
                                 true, // range
                                 output pEqualsOperator, 
                                 output plndexedOperator).         
    end method.
   
    method protected static logical QueryExcludesEmpty(pQueryPredicate as IQueryPredicate,pcDatatype as character, plUnknownIsEmpty as logical,plRange as logical,output pEqualsOperator as logical, output plndexedOperator as logical):
        define variable lExcludesEmpty as logical no-undo. 
        define variable lValue as logical no-undo.
        define variable cValue as character no-undo.
        define variable cValues as character extent no-undo.
        define variable dValue as decimal no-undo.
        define variable lUnknown as logical no-undo.
        define variable lUnsupported as logical no-undo.
        define variable iExt as integer no-undo.
        if valid-object(pQueryPredicate) then 
        do:
            if type-of(pQueryPredicate,Pmfo.Core.BusinessLogic.IQueryPredicate) then
            do: 
                cValue = cast(pQueryPredicate,Pmfo.Core.BusinessLogic.IQueryPredicate):StringValue.
                cValues = cast(pQueryPredicate,Pmfo.Core.BusinessLogic.IQueryPredicate):StringValues.
                lUnknown = cValue = ? or cValue = "'?'" or cValue = '"?"'.
                lUnsupported = false.
                case pcDataType: 
                    when "character" then
                    do:
                        case pQueryPredicate:Operator:
                            when QueryOperatorEnum:Eq or when QueryOperatorEnum:Begins or when QueryOperatorEnum:Matches then
                            do: 
                                if plUnknownIsEmpty then
                                    lExcludesEmpty = lUnknown = false.
                                else
                                    lExcludesEmpty = cValue <> "". 
                                pEqualsOperator = pQueryPredicate:Operator <> QueryOperatorEnum:Matches.    
                                plndexedOperator = pEqualsOperator.    
                            end.
                            when QueryOperatorEnum:NE then
                            do: 
                                if plUnknownIsEmpty then
                                    lExcludesEmpty = lUnknown.
                                else
                                    lExcludesEmpty = cValue = "".
                            end.
                            when QueryOperatorEnum:GE then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = cValue <> "".
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:GT then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = true. // any value will exclide empty 
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:LT then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = true. // any value will exclude empty 
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:LE then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = false. // any value will include empty 
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:InRange then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = cValues[1] > "".  
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:InList then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = Array:Find("", cValues) = 0 .  
                                    pEqualsOperator = true.
                                    plndexedOperator = pEqualsOperator.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            otherwise 
                                lUnsupported = true.
                        end case.
                        if lUnsupported then 
                             undo, throw new UnsupportedOperationError(subst("Filter '&1' with operator '&2'. Only EQ, BEGINS, MATCHES or NE is suppported for '&1'.",pQueryPredicate:FieldName,caps(pQueryPredicate:Operator:ToString()))).
                     
                    end.
                    when "logical" then
                    do:
                        lValue = logical(cValue).
                        case pQueryPredicate:Operator:
                            when QueryOperatorEnum:Eq then
                            do: 
                                if plUnknownIsEmpty then
                                   lExcludesEmpty = lUnknown = false.
                                else 
                                   lExcludesEmpty = lValue.
                                pEqualsOperator = true.    
                                plndexedOperator = true.    
                            end.       
                            when QueryOperatorEnum:NE then
                            do: 
                                if plUnknownIsEmpty then
                                    lExcludesEmpty = lUnknown.
                                else   
                                    lExcludesEmpty = not lValue.
                            end.     
                            otherwise 
                                 undo, throw new UnsupportedOperationError(subst("Filter '&1' with operator '&2'. Only EQ or NE is suppported for '&1'.",pQueryPredicate:FieldName,caps(pQueryPredicate:Operator:ToString()))).
             
                        end case.
                    end.
                    when "integer" or when "int64" or when "decimal" then do:
                        dValue = decimal(cValue).
                        case pQueryPredicate:Operator:
                            when QueryOperatorEnum:Eq then
                            do: 
                               if plUnknownIsEmpty then
                                   lExcludesEmpty = lUnknown = false.
                               else 
                                   lExcludesEmpty = dValue <> 0.
                                
                                pEqualsOperator = true. 
                                plndexedOperator = true.    
                            end.       
                            when QueryOperatorEnum:NE then
                            do: 
                                if plUnknownIsEmpty then
                                    lExcludesEmpty = lUnknown.
                                else   
                                    lExcludesEmpty = dValue = 0.
                            end. 
                            when QueryOperatorEnum:GE then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = dValue > 0.
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:GT then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = dValue > 0 or dValue = 0.  
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:LT then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = dValue < 0 or dValue = 0.  
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:LE then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = dValue < 0. 
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:InRange then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    lExcludesEmpty = decimal(cValues[1]) > 0.  
                                    plndexedOperator = true.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            when QueryOperatorEnum:InList then
                            do:
                                if plRange and plUnknownIsEmpty = false then
                                do:
                                    do iExt = 1 to extent(CValues): 
                                       lExcludesEmpty = decimal(cValues[1]) <> 0.
                                       if lExcludesEmpty = false then 
                                           leave.
                                    end.     
                                    pEqualsOperator = true.
                                    plndexedOperator = pEqualsOperator.
                                end.
                                else 
                                    lUnsupported = true.    
                            end.
                            otherwise
                                lUnsupported = true. 
                        end case.
                        if lUnsupported then
                            undo, throw new UnsupportedOperationError(subst("Filter '&1' with operator '&2'. Only EQ or NE is suppported for '&1'.",pQueryPredicate:FieldName,caps(pQueryPredicate:Operator:ToString()))).
                    end.
                    
                    otherwise 
                        undo, throw new IllegalArgumentError(subst("DataType &1. Only 'character', 'integer' and 'logical' are supported",pcDataType)).      
                end.
            end.
            else 
                undo, throw new UnsupportedOperationError(subst("QueryPredicate of type &1 for search of related inspection",pQueryPredicate:GetClass():TypeName)).
        
            return lExcludesEmpty.
        end.    
        return ?.         
    end method. 
    
end class.