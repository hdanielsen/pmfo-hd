 
 /*------------------------------------------------------------------------
    File        : BaseFilterParser
    Purpose     : Parses a complex query with <field> <operator> <value>
                  expressions and any number of parameters and a sort 
                  and returns them as query objects to be passed to server
                  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Apr 05 23:38:00 EDT 2019
    Notes       : Use FilterParser subclass for parsing of public filter ans sort with 
                  public field names (serialize-names) and forward slash as qualifier
                - Use KeyValueParser for key expressions with no spaces
                  and comma deleimiter.   
                - does not handle functions as value
                - Expression MUST have spaces between tokens 
                  = is not word delimiter
                - The QueryBuilder will use the output objects here 
                  and build a database query   
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.List from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.BusinessLogic.IQueryDefinition from propath.
using Pmfo.Core.BusinessLogic.QueryDefinition from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Util.StringUtil from propath.
 
using OpenEdge.Core.String from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
 

class Pmfo.Core.BusinessLogic.Parser.BaseFilterParser: 
    
     define public property HasWarnings as logical no-undo get. set.
      
     define public property Warnings as IList  
         get():
             if not valid-object(Warnings) then 
                 Warnings = new List().
             return Warnings.    
         end.     
         set(pColl as IList):
             Warnings = pColl.
             HasWarnings = true.
         end.      
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define protected property TableDelimiter as character  init "."  get. set.
     
    constructor public BaseFilterParser ():
       super().
    end constructor.
   
    method public IQueryDefinition GetQueryDefinition(phBuffer as handle, pFilter as character):
        return GetQueryDefinition(phBuffer,pFilter,"").
    end method.
    
    method public IQueryDefinition GetQueryDefinition(phBuffer as handle, pFilter as character,pSort as character):
        
        define variable oFilter      as IQueryEntry no-undo.
        define variable oSortEntries as IQuerySortEntry extent no-undo.
        define variable cFieldMap    as character no-undo.
        
        if pFilter > "" then
            oFilter = GetFilter(phBuffer,pFilter).
        
        if pSort > "" then 
            oSortEntries = GetSort(phBuffer,pSort).
        
        if valid-object(oFilter) and extent(oSortEntries) <> ? then     
            return new QueryDefinition(oFilter,oSortentries).
        else if valid-object(oFilter) then     
            return new QueryDefinition(oFilter).
        else if extent(oSortEntries) <> ? then     
            return new QueryDefinition(oSortentries).
        return ?.     
    end method.
    
    method protected IQueryEntry GetFilter(pBuffer as handle,pExpression as character):
        define variable cWorkexp       as character   no-undo.
        define variable i              as integer     no-undo.
        define variable cWord          as character   no-undo.
        define variable cRealValue     as character   no-undo.
        define variable cType          as character   no-undo.
        define variable iCount         as integer     no-undo.
        define variable oJoin          as JoinEnum    no-undo.
        define variable iPos           as integer     no-undo.
        define variable oEntries       as IList       no-undo.
        define variable oPredicate     as QueryPredicate no-undo.
        define variable cField         as character   no-undo.
        define variable cValue         as character   no-undo.
        define variable oOperator      as QueryOperatorEnum no-undo.
        define variable iEndGroup      as logical     no-undo.
        define variable oEntry         as IQueryEntry no-undo.
        define variable cSubExpression as character   no-undo.
        define variable lNot           as logical     no-undo.
        define variable lEndswith      as logical     no-undo.
        define variable lContains      as logical    no-undo.
        define variable lSingleQuote   as logical no-undo.
        define variable lDoubleQuote   as logical no-undo.
        define variable lInList        as logical no-undo.
        define variable cEnd           as character no-undo.
        define variable cRequestedField as character no-undo.
        define variable cFieldName      as character no-undo.  
        define variable hQualBuffer     as handle no-undo.
        define variable cValues         as character extent no-undo.
        define variable iValue          as integer no-undo.
        if pExpression = "" then 
            return ?.
        
        pExpression = trim(pExpression).
        if pExpression begins "(" and substr(pExpression,length(pExpression),1) = ")" then  
            pExpression = substr(pExpression,2,length(pExpression) - 2).
       
        assign 
            cWorkexp    = StringUtil:MaskQuotes(pExpression,"@")
            iPos        = 1
            oEntries    = new List().
           
        if num-entries(cWorkExp,"(") <> num-entries(cWorkExp,")") then 
            undo, throw new BadRequestError("Mismatched parenthesis in filter").
        
        if num-entries(cWorkexp,StringConstant:SPACE) lt 3 then
            undo, throw new IllegalArgumentError(subst("Filter passed to GetFilter '&1'",pExpression)). 
        do i = 1 to num-entries(cWorkexp,StringConstant:SPACE):
            cWord = entry(i,cWorkexp,StringConstant:SPACE).
            if cWord <> "" then 
            do:
                iCount = iCount + 1.
                case iCount:
                    when 1 then do: 
                        ctype = "field".
                        if cWord begins "(" then 
                            cType = "group". 
                        else 
                            cField = cWord.
                        if cField = "not" then 
                        do:
                            iCount = 0.
                            cField = "".
                            lNot = true.
                        end.    
                    end.    
                    when 2 then do:
                        ctype = "operator".
                        
                        if cWord = "endswith" then
                        do:
                            oOperator = QueryOperatorEnum:Matches.
                            lEndsWith  = true.
                        end.
                        else if cWord = "contains" then
                        do:
                            oOperator = QueryOperatorEnum:Matches.
                            lContains  = true.
                        end.    
                        else if cWord = "in" then
                        do:
                            oOperator = QueryOperatorEnum:InList.
                            lInList = true.
                        end.    
                        else 
                            oOperator = QueryUtil:GetOperatorEnum(cWord).
                        if not valid-object(oOperator) then 
                            undo, throw new IllegalArgumentError(subst("Operator '&1' in expression '&2'",cWord,pExpression)).    
       
                    end.
                    when 3 then 
                    do:
                        ctype = "value".
                        cValue = substr(pExpression,ipos,length(cWord)).
                        
                        // TODO: Identify and fix the reason for the right trim of the 
                        // parenthesis (likely because it is not delimited by space)
                        // This fix seems as a bad hack and problematic for the list case 
                        // which has the values in parenthesis
                        // NOTE: Update about TODO: 
                        //       Tested many variations of inlist inside parenthesis and
                        //       there was never an extra parenthesis at thispoint    
                        if lInlist then
                        do: 
                           //if trim(cValue) 
                            cValues = GetInListValues(cValue).
                        end.
                        else do:
                            cValue = right-trim(cValue,")").
                                   
                            if cValue = "null" then
                                cValue = ?.   
                            else if cValue = 'and' or cValue = 'or' then 
                                undo, throw new BadRequestError(subst("Invalid filter expression. Join operator '&1' encountered instead of a value.",cValue)).  
                        end.
                        if IsFieldQualified(cfield) then 
                        do: 
                            cRequestedField = cField. // keep for error - 3rd param false - allow any relation 
                            GetOtherTableBufferAndField(pBuffer, cField, false, "filter", output hQualBuffer, output cField).
                           
                            if not valid-handle(hQualBuffer) then 
                                undo, throw new IllegalArgumentError(subst("'&1' is not a valid filter property reference for the '&2' request. The qualified property name must match an entity in the expected response.",
                                                                           cRequestedField,
                                                                           BufferUtil:GetRowName(pBuffer)
                                                                          ) 
                                                                       ).   
                            if lInList then 
                            do i = 1 to extent(cValues): 
                                cValues[i] = GetAblStringValue(hQualBuffer, cField, cValues[i], pExpression).
                            end.                                           
                            else
                                cValue = GetAblStringValue(hQualBuffer, cField, cValue, pExpression).
                             
                            // use table name since buffer names will not survive call to server (static tables are unique in dataset)
                            cFieldName = subst("&1.&2",hQualBuffer:table-handle:name,cField).
                        end.     
                        else do:
                            // allow subclass to map the external value (json) to an internal value (tt)
                            cfield = GetUseFieldName(pBuffer,cfield).
                            if lInList then 
                            do i = 1 to extent(cValues): 
                                cValues[i] = GetAblStringValue(pBuffer, cField, cValues[i], pExpression).
                            end.                                           
                            else
                                cValue = GetAblStringValue(pBuffer, cField, cValue, pExpression).
                            cFieldName = cField.
                        end.
                        
                        if lInlist then
                            oPredicate = new QueryPredicate(cFieldname,oOperator,cValues).
                        else do:
                            if lendsWith then 
                                cValue = "*" + cValue.
                            else if lcontains then 
                                cValue = "*" + cValue + "*".
                            oPredicate = new QueryPredicate(cFieldname,oOperator,cValue).
                        end.
                    end.     
                    when 0 then 
                    do:
                        cType = "join".
                        if cWord = "and" then oJoin = JoinEnum:And.
                        else if cWord = "or" then oJoin = JoinEnum:Or.
                        else
                            undo, throw new BadRequestError(subst("'&1' is not a join operator",cWord)).
                    end.    
                end case.
                
                if cType = "group" then 
                do:
                    cSubExpression = StringUtil:ExtractParenthesis(substr(cWorkexp,ipos)).
                    cSubExpression = substr(pExpression,ipos,length(cSubExpression)).
                    oEntry = GetFilter(pBuffer,cSubExpression).
                       
                    if lNot then 
                        oJoin = getNot(oJoin).
                    if valid-object(oJoin) then
                    do:
                        if type-of(oEntry,QueryGroup) then
                            cast(oEntry,QueryGroup):Join = oJoin.
                        else 
                            cast(oEntry,QueryPredicate):Join = oJoin.
                    end.
                    
                    oEntries:Add(oEntry).
                    assign 
                        i      = i + num-entries(cSubExpression,StringConstant:SPACE) - 1
                        iPos   = ipos + length(cSubExpression) + 1
                        cField = ""
                        iCount = -1   
                        cWord  = "" 
                        oJoin  = ?
                        lContains  = false
                        lEndswith = false. 
                        lNot = false
                        .
                end.
                else do:
                    if iCount = 3 then 
                    do:
                       if lNot then 
                           oJoin = getNot(oJoin).
                       if valid-object(oJoin) then
                           oPredicate:Join = oJoin.
                       oEntries:Add(oPredicate).
                       assign
                           iCount    = -1 
                           cField    = ""
                           cValue    = ""
                           oJoin     = ?
                           oOperator = ?
                           lContains  = false
                           lEndswith = false
                           linlist   = false 
                           lNot = false.
                       
                    end.
                    iPos = iPos + length(cWord) + 1.
                    cWord = "".
                end.               
            end.     
            else do: 
               iPos = iPos + 1.
            end.   
        end.   
        if iCount = 2 then 
            undo, throw new BadRequestError(subst("Invalid filter expression. It appears as '&1 &2' is missing the value.",cField,lc(oOperator:ToString()))).
        if iCount = 1 then 
            undo, throw new BadRequestError(subst("Invalid filter expression. It appears as '&1' is missing operator and value.",cField)).
        if oEntries:Size = 1 then 
            return cast(oEntries:Get(1),IQueryEntry).
        else  do:
            define variable oGroup as QueryGroup no-undo.
            oGroup = new QueryGroup(cast(oEntries:ToArray(),IQueryEntry)).
            return oGroup. 
        end.
    end method.
    
    method protected character extent GetInListValues( pcExpression  as character):
        define variable cValues as character extent no-undo.
      
        define variable cWorkExpression as character no-undo. 
        define variable iLength         as integer   no-undo.
        define variable iCharnum        as integer   no-undo.
        define variable i               as integer   no-undo.
        define variable cValueEntry     as character no-undo.
        iLength = length(pcExpression).
         
        if pcExpression begins "(" and substr(pcExpression,iLength) = ")" then
        do:
            cWorkExpression = substr(pcExpression,2,iLength - 2).
            // if quoted values then mask commas inside quotes so we can use num-entries  
            if index(cWorkExpression,"'") > 0 or index(cWorkExpression,'"') > 0 then
                cWorkExpression = StringUtil:MaskCharInQuotes(cWorkExpression,",",output iCharnum).
            
            extent(cValues) = num-entries(cWorkExpression).
            do i = 1 to extent(cValues):   
               cValueEntry = trim(entry(i,cWorkExpression)).
               if iCharNum > 0 then 
                   cValueEntry = replace(cValueEntry,chr(iCharNum),","). 
               cValues[i] = cValueEntry.
              
            end.
        end.
        else 
            undo, throw new IllegalArgumentError(subst("Invalid value list &1 encountered after IN Operator. The values must be specifed as a comma separated list in parenthesis.")).
        return cValues.
    end method.  
    
    method protected character GetAblStringValue(phBuffer as handle,pcFieldName as character, pcClientValue as character, pcExpression  as character):
        define variable cValue as character no-undo.
        do on error undo, throw:                                              
            cValue = BufferUtil:AssertAndConvertFromPublicValue(phBuffer, pcFieldName, pcClientValue, pcExpression).
            catch e as ApplicationError :
                // temp hack 
                HandleDataError(phBuffer, pcFieldName, e).    
                cValue = StringUtil:UnquoteValue(pcClientValue).
            end catch.
        end.
        return cValue.
    end method.  
      
    method protected logical IsFieldQualified(pcfield as characte):
        // subclasses may have more than two 
        return num-entries(pcField,TableDelimiter) > 1.
    end method.    
    
    // Allow subclass to map the external value (json) to an internal value (tt)
    method protected character GetUseFieldName(phBuffer as handle,pcfield as characte):
        return pcField.
    end method.    
    
    method public IQuerySortEntry extent GetSort(phBuffer as handle,pSort as character):
        define variable oSortEntries as IQuerySortEntry extent no-undo.
        define variable lAbl         as logical no-undo.
        define variable i            as integer no-undo.
        define variable hQualBuffer  as handle no-undo.
        define variable cField       as character no-undo.
        define variable cUsename     as character no-undo.
        define variable hField       as handle no-undo.
        define variable cInvalidFieldTmpl     as character init "'&1' is not a valid property name in '&2'. The orderBy&3is invalid." no-undo.
        
        // support ABL sort...   
        if trim(pSort) begins "by " then 
        do:
            // paranoid - by is a keyword, but could be a serializename
            // so be 100% sure this is an abl expression
            // the trimming and space removal happens in the utils also
            // so no difference in behavior 
            pSort = trim(psort).
            do while index(pSort,StringConstant:SPACE + StringConstant:SPACE) > 0:
                pSort = replace(pSort,StringConstant:SPACE + StringConstant:SPACE,StringConstant:SPACE).
            end. 
            lAbl = num-entries(pSort,StringConstant:SPACE) > 1
                   and index(pSort,",") = 0
                   and entry(2,StringConstant:SPACE) <> "desc" 
                   and entry(2,StringConstant:SPACE) <> "asc". 
        end.    
        
        if lAbl then 
            oSortEntries = QueryUtil:GetQuerySort(pSort).
        else if pSort > "" then 
            oSortEntries = QueryUtil:GetQuerySortFromDelimitedExpression(pSort).
        
        do i = 1 to extent(oSortEntries):
            
            if IsFieldQualified(oSortEntries[i]:FieldName) then 
            do:
                 GetOtherTableBufferAndField(phBuffer, 
                                             oSortEntries[i]:FieldName, 
                                             true, // Assert not one to many relation  
                                             "sort", // operation for message 
                                             output hQualBuffer, 
                                             output cField).
                 if valid-handle(hQualBuffer) then
                 do:
                     hField = hQualBuffer:buffer-field(cField) no-error.
                     if not valid-handle(hField) then 
                          undo, throw new IllegalArgumentError(subst(cInvalidFieldTmpl,
                                                                     oSortEntries[i]:FieldName,
                                                                     hQualBuffer:serialize-name,
                                                                     pSort
                                                           )).    
                     
                     cast(oSortEntries[i],QuerySortEntry):FieldName = subst("&1.&2",hQualBuffer:table-handle:name,cField).
                 end.    
                 else
                     undo, throw new IllegalArgumentError(subst("'&1' is not a valid sort property reference for the '&2' request. The qualified property name must match an entity in the expected response.",
                                                           oSortEntries[i]:FieldName,
                                                           BufferUtil:GetRowName(phBuffer)
                                                           )).      
            end.
            else do:
                cUsename = GetUseFieldName(phBuffer, oSortEntries[i]:FieldName).
                hField = phBuffer:buffer-field(cUsename) no-error.
                if not valid-handle(hField) then 
                    undo, throw new IllegalArgumentError(subst(cInvalidFieldTmpl,
                                                               oSortEntries[i]:FieldName,
                                                               phBuffer:serialize-name,
                                                               // don't show the sort if it is only the one field already shown in the message  
                                                               if extent(oSortEntries) = 1 then " " else subst("'&1'",pSort)
                                                           )).    
                if cUsename <> oSortEntries[i]:FieldName then 
                    cast(oSortEntries[i],QuerySortEntry):FieldName = cUseName. 
                    
            end.
        end.    
        return oSortEntries.    
    end method.    
    
    method protected final void GetOtherTableBufferAndField(pMainBuffer as handle,pcField as character, plAssertNotOneToMany as logical, pcoperation as character, output phQualBuffer as handle, output pcTableField as character):
        define variable iNumQualifiers      as integer no-undo.
        define variable cQualifier          as character no-undo.
        define variable cOtherTableFieldMap as character no-undo. 
        define variable cField              as character no-undo.
        
        iNumQualifiers = num-entries(pcfield,TableDelimiter).
        cQualifier = pcField. 
        cField = entry(iNumQualifiers,cQualifier,TableDelimiter).
        entry(iNumQualifiers,cQualifier,TableDelimiter) = "".
        cQualifier = right-trim(cQualifier,TableDelimiter).
        
        // allow subclasses to override to return buffer and field differently 
        GetOtherTableBufferAndField(pMainBuffer,cQualifier,cField,plAssertNotOneToMany, pcoperation, output phQualBuffer, output pcTableField ).
                    
    end method.
    
     // allow subclasses to override to return buffer and field differently 
    method protected void GetOtherTableBufferAndField(pMainBuffer as handle,pcQualifier as character, pcField as character,  plAssertNotOneToMany as logical, pcOperation as character, output phQualBuffer as handle, output pcOutField as character):
        define variable hDataset as handle no-undo.
       
        hDataset = pMainBuffer:dataset.
        phQualBuffer = hDataset:get-buffer-handle(pcQualifier).
        if not valid-handle(phQualBuffer) then 
            undo, throw new ApplicationError(subst("Field qualifier '&1' is not valid for request",pcQualifier)).
        pcOutField = pcField.            
    end method.
    
    method protected JoinEnum GetNot(pJoin as JoinEnum):
        case pJoin:
            when JoinEnum:And then
                return JoinEnum:AndNot.
            when JoinEnum:Or then
                return JoinEnum:OrNot.
            when ? then
                return JoinEnum:Not.
        end case.
                  
    end.   
     
    method protected void HandleDataError(phBuffer as handle, pcField as character, pError as ApplicationError):
        define variable cDataType as character no-undo.
        define variable lHandled as logical no-undo.
        if type-of(pError,UnsupportedOperationError) then 
        do: 
            // we did unfortunately allow bad data so now we need to forgive
            // We have added protection against values that are not true integer or decimal in 
            // BufferUTil:AssertAndConvertFromPublicValue so this should  be safe 
            cDatatype = phBuffer:buffer-field(pcField):data-type no-error.
            if cDatatype begins "int" or cDataType = "decimal" or cDataType = "logical" then
            do: 
               // Should we log this?    
             //  Warnings:Add(new String(subst("The &1 Filter has unsafe data. The request was executed, but please report this to IT.",BufferUtil:GetRowName(phBuffer,true)))).                   
             //  Warnings:Add(new String(pError:GetStrippedMessage())).
               lHandled = true.                   
            end.
        end.
        if not lHandled then 
        do:
            undo, throw pError.
        end.         
    end.
               
end class.