 
 /*------------------------------------------------------------------------
    File        : JsonParameterParser
    Purpose     : Build a ParameterList from Json body 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Aug 28 08:01:17 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
using Progress.Reflect.Parameter from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using Pmfo.Core.BusinessLogic.Parser.FieldValueParser from propath.
using Pmfo.Util.BufferUtil from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IMapEntry from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.JsonUtil from propath.
using Pmfo.Core.Error.UnprocessableEntityError from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Util.ServiceInfo from propath.

class Pmfo.Core.BusinessLogic.Parser.TableFieldValueParser : 
    
    define protected property Method   as Method no-undo get. set.
    // for error message 
    define protected property Resource as character no-undo get. set.
    
    define protected property PublicParameters as IParameter extent no-undo get. set.
    define protected property ParameterList as ParameterList no-undo get. set.
    
    define protected property ParameterNames as character init ? no-undo 
         get():
             define variable cDlm as character no-undo.
             define variable i as integer no-undo.
             define variable oParams as Parameter extent no-undo.
             if ParameterNames = ? then
             do:
                 ParameterNames = "". 
                 do i = 1 to extent(PublicParameters):
                    ParameterNames = ParameterNames
                                  + (if i = 1 then "" else ",")
                                  + PublicParameters[i]:name.                              
                 end.
                  
             end.
             return ParameterNames. 
         end. 
         set.
    
    constructor public TableFieldValueParser (  ):
        super ().
    end constructor.
    
    method protected IStringStringMap ParseExpression(phBuffer as handle,pcExpression as character):
        define variable oFieldValueParser as FieldValueParser no-undo.
        pcExpression = ServiceInfo:ConvertSingleKeyToExpression(phbuffer, pcExpression, true, true).
        
        oFieldValueParser = new FieldValueParser().
        return oFieldValueParser:ParseExpressions(pcExpression,true). // keep quotes
    end method.     
    
    method public void CreateRow(phBuffer as handle, pcFieldExpression as character):
        define variable cFieldMap         as character no-undo.
        define variable oParameters       as IStringStringMap no-undo.       
        
        oParameters = ParseExpression(phBuffer, pcFieldExpression).
        cFieldMap = BufferUtil:GetPublicToNameFieldPairs(phBuffer).
        CreateBufferAndAssignValues(oParameters, phbuffer, cFieldMap).  
    end.   
    
    method public void AssertJsonAndAddValues(pJson as jsonObject, phBuffer as handle, pcFieldExpression as character):
        define variable cFieldMap         as character no-undo.
        define variable oParameters       as IStringStringMap no-undo.  
             
        oParameters = ParseExpression(phBuffer, pcFieldExpression).
        cFieldMap = BufferUtil:GetPublicToNameFieldPairs(phBuffer).
       
        AssertJsonAndAddValues(oParameters, pJson, phbuffer, cFieldMap).  
    end.   
   
    method protected void CreateBufferAndAssignValues(pParameters as IStringStringMap, phbuffer as handle, pFieldMap as character):
         define variable oIterator as IIterator  no-undo.
         define variable oMapEntry as IMapEntry no-undo.
         
         define variable cField       as character no-undo.
         define variable cPublicField as character no-undo.
         
         define variable cMappedField as character no-undo.
         define variable cValue       as character no-undo.
         define variable hField       as handle no-undo.
         define variable lResetTrack  as logical no-undo.
         define variable lBadEntry    as logical no-undo.
         if phBuffer:table-handle:tracking-changes = false then
         do:      
             phBuffer:table-handle:tracking-changes = true.
             lResetTrack = true.
         end.         
         phBuffer:buffer-create().
         oIterator = pParameters:EntrySet:Iterator().         
         do while oIterator:HasNext():
             oMapEntry = cast(oIterator:Next(),IMapEntry).
             cPublicField = string(cast(oMapEntry:Key,String):Value).
             cValue = string(cast(oMapEntry:Value,String):Value).
             cField = GetTTFieldName(cPublicField, pFieldMap, BufferUtil:GetRowName(phBuffer)). 
             hField = phBuffer:buffer-field(cField).
             do on error undo, throw:
                 cValue = BufferUtil:AssertAndConvertFromPublicValue(phBuffer, cField, cValue).
             end.
             hField:buffer-value = cValue.
         end.    
         catch e2 as Progress.Lang.Error :
             phBuffer:buffer-delete().
             undo, throw e2.    
         end catch.
         
         finally: 
            if lResetTrack then
                phBuffer:table-handle:tracking-changes = false.      
         
         end.    
    end.
    
   
    method protected void AssertJsonAndAddValues(pParameters as IStringStringMap, pJson as JsonObject,phbuffer as handle, pFieldMap as character):
         define variable oIterator as IIterator  no-undo.
         define variable oMapEntry as IMapEntry no-undo.
         
         define variable cField       as character no-undo.
         define variable cPublicField as character no-undo.
         
         define variable cMappedField as character no-undo.
         define variable cValue       as character no-undo.
         define variable hField       as handle no-undo.
         define variable lBadEntry    as logical no-undo.
           
         oIterator = pParameters:EntrySet:Iterator().         
         do while oIterator:HasNext():
             
             oMapEntry    = cast(oIterator:Next(),IMapEntry).
             cPublicField = string(cast(oMapEntry:Key,String):Value).
             cValue = string(cast(oMapEntry:Value,String):Value).
             
             cField = GetTTFieldName(cPublicField, pFieldMap, BufferUtil:GetRowName(phBuffer)). 
             hField = phBuffer:buffer-field(cField) no-error.
             
             do on error undo, throw:
                 cValue = BufferUtil:AssertAndConvertFromPublicValue(phBuffer, cField, cValue).
             end.
             if pJson:Has(cPublicField) then
             do: 
                 if JsonUtil:GetStringValue(pJson, cPublicField) <> cValue then 
                     undo, throw new UnprocessableEntityError(subst("'&1' value in Json and URL does not match",cField)).       
             end.
             else do:
                 AddValueToJson(pJson, cPublicField, hField:data-type, cValue).
             end.
        end.    
    end.
    
    method protected character GetTTFieldName (pPublicName as character,pFieldMap as longchar, pcEntityName as character):
        define variable cMappedField as character no-undo.
        define variable lBadEntry as logical no-undo.
        if pFieldMap > "" then
        do: 
            cMappedField = StringUtil:GetMappedName(pPublicName, pFieldMap,output lBadEntry).
            if cMappedField = "" then 
            do:
                if lBadEntry then 
                    undo, throw new IllegalArgumentError(subst("'&1' is not a valid property name in '&2'",
                                                         pPublicName,
                                                         pcEntityName
                                                         )
                                                    ).
                 return pPublicName.  // no mapping reurn name as-is                                          
             end.                      
             else                          
                 return cMappedField. // return the mapped name
         end.
         return pPublicName. 
    end method.
    
    
    method protected void AddValueToJson(pJson as JsonObject,
                                         pcName as character,
                                         pcDataType as character,
                                         pcStringValue as character ):
        define variable longval as longchar no-undo.    
        
        case pcDataType:
            when "character" then do:
                pJson:Add(pcName,pcStringValue).
            end.    
            when "date" then do:
                pJson:Add(pcName,date(pcStringValue)).
            end.    
            when "datetime" then do:
                pJson:Add(pcName,datetime(pcStringValue)).
            end.    
            when "datetime-tz" then do:
                pJson:Add(pcName,datetime-tz(pcStringValue)).
            end.    
            when "decimal" then do:
                pJson:Add(pcName,decimal(pcStringValue)).
            end.
            when "integer" then do:
                 pJson:Add(pcName,integer(pcStringValue)).
            end.
            when "int64" then do:
                pJson:Add(pcName,int64(pcStringValue)).
            end.    
            when "logical" then do:
                pJson:Add(pcName,pcStringValue).
            end.    
            when "longchar" then do:
                pJson:Add(pcName,pcStringValue).
            end.    
            otherwise 
                undo, throw new UnsupportedOperationError(subst("URL key expression with datatype '&1'",pcDataType)).
        end case.         
        
    end method.
     

end class.