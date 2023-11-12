 
 /*------------------------------------------------------------------------
    File        : FieldExpressionParser
    Purpose     : parse <field> <EQ <value> [and] expressions
                  returning a StringStringMap with the names and values
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Aug 28 00:13:21 EDT 2019
    Notes       : Separated out of FieldValueParser since the difference 
                  of parsing with commas and '=' versus spaces, EQ and AND became difficult 
                  to manage in the same code    
                - Intended for startid - does not support single value for child 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.IMapEntry from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.List from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.Map.EntityFieldMapHolder from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Core.BusinessLogic.Parser.FieldExpressionParser  : 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable InvalidFieldTmpl as character init "'&1' is not a valid property name in '&2'" no-undo.
    define variable MissingQuotesMsgTmpl as character init "Invalid value &1 for property &2. &3 Values must be single quoted." no-undo.
    define variable BadQuotesMsgTmpl as character init "Invalid value &1 for property &2. &3 Values cannot be quoted." no-undo.
    define protected property FieldMapHolder as EntityFieldMapHolder no-undo get. protected set. 
    
    constructor public FieldExpressionParser ():
        this-object(new EntityFieldMapHolder()).
    end constructor.
    
    constructor public FieldExpressionParser (pFieldMap as EntityFieldMapHolder):
        super().
        FieldMapHolder = pFieldMap.
    end constructor.

    method protected IStringStringMap ParseExpressions (pcString as character) :
        return ParseExpressions(pcString, false). 
    end method.    
    
    method protected character GetBufferFieldName(phBuffer as handle,pcfield as characte):
        define variable cMap      as character no-undo.
        define variable cMappedField as character no-undo.
        define variable lBadEntry as logical no-undo.
        
        cMap = FieldMapHolder:GetFieldMap(phBuffer).
        cMappedField = StringUtil:GetMappedName(pcfield, cMap,output lBadEntry).
        
        if cMappedField = "" then 
        do:
            if lBadEntry then 
                undo, throw new IllegalArgumentError(subst(InvalidFieldTmpl,
                                                            pcField,
                                                            BufferUtil:GetRowName(phBuffer)
                                                           )
                                                     ).
            return pcfield.                                         
        end. 
        return cMappedField.
    end method.  
    
    method public IQueryEntry GetKeyEntry (pcString as character,phBuffer as handle) :
        define variable pParameters as IStringStringMap no-undo.
        define variable oList as IList no-undo.        
        do on error undo, throw:
            if lookup(pcstring,StringConstant:SPACE) = 0  then  
            do:
                pcString = ServiceInfo:ConvertSingleKeyToExpression(phbuffer, 
                                                                   pcString, 
                                                                   true, // public name in expression  
                                                                   false, // no single key for child  
                                                                   true) // use EQ as queryoperator
                                                                   .
            end.
            pParameters = ParseExpressions(pcString).
            catch e as Progress.Lang.Error :
                undo, throw new IllegalArgumentError(subst("'&1' is not a valid expression",pcString),e).        
            end catch.
        end.
        
        oList = SetParameters(pParameters, phbuffer ).
        
        if oList:Size = 1 then
            return cast(oList:Get(1),IQueryEntry). 
        else if oList:Size > 1 then    
            return new QueryGroup(cast(oList:ToArray(),IQueryEntry)). 
    end.
    
    method protected IList SetParameters(pParameters as IStringStringMap, phbuffer as handle):
         define variable oIterator      as IIterator  no-undo.
         define variable oMapEntry      as IMapEntry no-undo.
         define variable oList          as IList no-undo.
         define variable cField         as character no-undo.
         define variable cValue         as character no-undo.
         define variable hField         as handle   no-undo.
         define variable lBadEntry      as logical   no-undo.       
         
         define variable cInternalField as character no-undo.
         oIterator = pParameters:EntrySet:Iterator().         
         oList = new List(). 
         
         do while oIterator:HasNext():
             oMapEntry = cast(oIterator:Next(),IMapEntry).
             cField = string(cast(oMapEntry:Key,String):Value).
             cInternalfield = GetBufferFieldName(phBuffer,cField).
             cValue = string(cast(oMapEntry:Value,String):Value).
             cValue = BufferUtil:AssertAndConvertFromPublicValue(phBuffer,cInternalfield, cValue).
             oList:Add(new QueryPredicate(cInternalfield, QueryOperatorEnum:Eq,cValue)).
         end.    
         return oList.       
    end.
    
    
    method public IStringStringMap ParseExpressions (pcString as character,plKeepQuotes as logical) :
        define variable oMap              as IStringStringMap no-undo.
        define variable iLength           as integer         no-undo.
        define variable cMaskedExpression as character no-undo.
        define variable i                 as integer no-undo.
        define variable iWord             as integer no-undo.
        define variable cWord             as character no-undo.
        define variable cQuotedValues     as character extent no-undo.
        define variable cField            as character no-undo.
        define variable cValue            as character no-undo.
        define variable iStringCount      as integer no-undo.
        
        oMap = new StringStringMap(). 
        
        pcString   = trim(pcString).
        iLength    = length(pcString).
        
        cMaskedExpression = StringUtil:MaskQuotes(input pcString, input '@', output cQuotedValues).
        
        do i = 1 to num-entries(cMaskedExpression,StringConstant:SPACE):
            cWord = entry(i,cMaskedExpression,StringConstant:SPACE).
            
            if cWord > "" then 
            do:
                iWOrd = iWord + 1.
                case iWord:
                    when 1 then 
                    do:
                        if cWord begins "@" then 
                        do: 
                            iStringCount = iStringCount + 1. 
                            cWord = cQuotedValues[iStringCount].
                            undo, throw new IllegalArgumentError(subst("Quoted string &1 is not a valid field reference",cWord)).
                        end.
                        
                        cField = cWord.
                    end.
                    when 2 then
                    do: 
                        if cWord <> "eq" then 
                        do:
                            if cWord begins "@" then 
                            do: 
                                iStringCount = iStringCount + 1. 
                                cWord = cQuotedValues[iStringCount].
                                undo, throw new IllegalArgumentError(subst("Quoted string &1 is not a valid operator",cWord)).
                            end.
                            undo, throw new IllegalArgumentError(subst("Unsupported or invalid operator '&1'. Only 'eq' operator allowed in key expression",cWord)).
                        end.      
                    end.       
                    when 3 then
                    do:                       
                        if cWord begins "@" then 
                        do: 
                            iStringCount = iStringCount + 1. 
                            cValue = cQuotedValues[iStringCount].
                        end.
                        else
                            cValue = cWord.   
                            
                        oMap:put(cField,cValue).    
                    end.   
                    when 4 then 
                    do: 
                        if cWord <> "and" then 
                        do:
                            if cWord begins "@" then 
                            do: 
                                iStringCount = iStringCount + 1. 
                                cWord = cQuotedValues[iStringCount]. 
                                undo, throw new IllegalArgumentError(subst("Found quoted string &1 in expression instead of the expected 'and' operator",cWord)).
                  
                            end.    
                            undo, throw new IllegalArgumentError(subst("Found '&1' in expression instead of the expected 'and' operator",cWord)).
                        end.
                        iWord = 0.            
                    end.  
                end.  
            end.
        end.
        if iWord <> 3 then
            undo, throw new IllegalArgumentError(subst("'&1' is not a valid key expression",pcString)).  
        
        return oMap.
    end method.      
    
    
end class.