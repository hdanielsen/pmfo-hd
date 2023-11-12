 
 /*------------------------------------------------------------------------
    File        : KeyValueParser
    Purpose     : Parses an expression with fields and values and returns
                  them as QueryEntry    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Aug 28 00:22:41 EDT 2019
    Notes       : Only allowed operators = "eq" and "=". 
                  "=" is also word delimiter and can be used without spaces
                  Delimiter is " and " or comma. 
                  Comma is also word delimiter and can be used without spaces 
                - Use FilterParser for complex queries 
----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.IMapEntry from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.List from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.Parser.FieldValueParser from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Core.BusinessLogic.Parser.KeyValueParser: 
    
    // This variation supports a single key value - the last parameter specifies if parent join fields can be ignored so that a single key can 
    // be derrived for a multi-key buffer when the leading fields are in the join        
    method public IQueryEntry GetKeyEntry (pcString as character,phBuffer as handle,plHasPublicNames as logical, plUsechildKeys as logical) :
        
        pcString = ServiceInfo:ConvertSingleKeyToExpression(phbuffer, pcString, plHasPublicNames, plUsechildKeys).
        
        return GetKeyEntry (pcString,phBuffer,plHasPublicNames).
    end.
    
    method public IQueryEntry GetKeyEntry (pcString as character,phBuffer as handle,plHasPublicNames as logical) :
        define variable cFieldMap as character no-undo.
        define variable pParameters as IStringStringMap no-undo.
        define variable oFieldValueParser as FieldValueParser no-undo.
        define variable oList as IList no-undo.        
        do on error undo, throw:
            oFieldValueParser = new FieldValueParser().
            pParameters = oFieldValueParser:ParseExpressions(pcString, true). // keep quotes for vlaidation here
            catch e as Progress.Lang.Error :
                undo, throw new IllegalArgumentError(subst("'&1' is not a valid expression",pcString),e).        
            end catch.
        end.
        return GetKeyEntry (pParameters,phBuffer,plHasPublicNames).
            
    end.
    
    method public IQueryEntry GetKeyEntry (poParameterMap as IStringStringMap,phBuffer as handle,plHasPublicNames as logical) :
        define variable cFieldMap as character no-undo.
        define variable oList as IList no-undo.        
         
        if plHasPublicNames then
            cFieldMap = BufferUtil:GetPublicToNameFieldPairs(phBuffer).
        
        oList = SetParameters(poParameterMap, phbuffer, plHasPublicNames, cFieldMap).
        
        if oList:Size = 1 then
            return cast(oList:Get(1),IQueryEntry). 
        else if oList:Size > 1 then    
            return new QueryGroup(cast(oList:ToArray(),IQueryEntry)). 
    end.
    
    method protected IList SetParameters(pParameters as IStringStringMap, phbuffer as handle, plPublic as logical, pFieldMap as character):
         define variable oIterator    as IIterator  no-undo.
         define variable oMapEntry    as IMapEntry no-undo.
         define variable oList        as IList no-undo.
         define variable cField       as character no-undo.
         define variable cMappedField as character no-undo.
         define variable cValue       as character no-undo.
         define variable hField       as handle no-undo.
         define variable lBadEntry    as logical no-undo.       
         oIterator = pParameters:EntrySet:Iterator().         
         oList = new List(). 
         do while oIterator:HasNext():
             oMapEntry = cast(oIterator:Next(),IMapEntry).
             cField = string(cast(oMapEntry:Key,String):Value).
             cValue = string(cast(oMapEntry:Value,String):Value).
             
             if plPublic then
             do:
                 if pFieldMap > "" then
                 do:
                     cMappedField = StringUtil:GetMappedName(cField, pFieldMap,output lBadEntry).
                     if cMappedField = "" then 
                     do:
                          if lBadEntry then 
                              undo, throw new IllegalArgumentError(subst("'&1' is not a valid property name in '&2'",
                                                                      cField,
                                                                      BufferUtil:GetRowName(phBuffer)
                                                                     )
                                                                   ).
                     end.                                                
                     else 
                         cField = cMappedField.                                             
                 end.         
                 do on error undo, throw:
                     cValue = BufferUtil:AssertAndConvertFromPublicValue(phBuffer, cField, cValue).
                 end.
             end.
             else 
             do on error undo, throw:
                 hField = phBuffer:buffer-field(cField).
                 catch e as Progress.Lang.Error :
                     undo, throw new IllegalArgumentError(subst("'&1' is not a valid field name in '&2'",
                                                                cField,
                                                                BufferUtil:GetRowName(phBuffer)
                                                               ),
                                                           e).      
                 end catch.
             end.   
             oList:Add(new QueryPredicate(cField, QueryOperatorEnum:Eq,cValue)).
         end.    
         return oList.       
    end.
    
   
end class.