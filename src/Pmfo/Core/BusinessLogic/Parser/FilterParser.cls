 
 /*------------------------------------------------------------------------
    File        : FilterParser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Nov 01 14:24:13 EDT 2021 - moved base behavior to BaseFilter
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetGetManager from propath.
using Pmfo.Core.BusinessLogic.Map.EntityFieldMapHolder from propath.
using Pmfo.Core.BusinessLogic.Parser.BaseFilterParser from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.StringUtil from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.

class Pmfo.Core.BusinessLogic.Parser.FilterParser inherits BaseFilterParser:
     
    define protected property FieldMapHolder as EntityFieldMapHolder no-undo get. protected set. 
    define protected property Relations      as ICollection no-undo get. protected set. 
    
    constructor public FilterParser (pRelations as ICollection):         
        this-object(new EntityFieldMapHolder(),pRelations).
    end constructor.                            
    
    constructor public FilterParser (pFieldMap as EntityFieldMapHolder, pRelations as ICollection):
        super().
        FieldMapHolder = pFieldMap.
        TableDelimiter = "/".
        Relations = pRelations.
    end constructor.
    
    method protected IRelationDefinition GetRelation(phRelation as handle):
        define variable oIterator as IIterator no-undo.    
        define variable oRelation as IRelationDefinition no-undo.    
        if valid-object(Relations) then
        do: 
            oIterator = Relations:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelationDefinition).         
                if  oRelation:ParentName = phRelation:parent-buffer:serialize-name
                and oRelation:PublicName = phRelation:child-buffer:serialize-name then 
                    return oRelation.
            
            end.
        end.
    end method.
    
    method protected void AssertNotOneToMany(phTopBuffer as handle, pcField as character, phRelation as handle, pcOperation as character): 
        define variable oRelation as IRelationDefinition no-undo.    
        oRelation = GetRelation(phRelation).
        
        if valid-object(oRelation) = false  
        or RelationTypeEnum:OneToMany:Equals(oRelation:Cardinality) then
            undo, throw new IllegalOperationError(subst("Cannot &1 &2 on &3. The relation from &4 to &5 is a one to many relation.",
                                                         pcOperation,
                                                         phTopbuffer:serialize-name,
                                                         pcField,
                                                         phRelation:parent-buffer:serialize-name,
                                                         phRelation:child-buffer:serialize-name)
                                                   ).
       if phRelation:parent-buffer <> phTopBuffer then
       do: 
           if valid-handle(phRelation:parent-buffer:parent-relation) then 
               AssertNotOneToMany(phTopBuffer,pcField,phRelation:parent-buffer:parent-relation, pcOperation).
       end.
    end method.      
    
    method protected override void GetOtherTableBufferAndField(pMainBuffer as handle,pcQualifier as character, pcField as character, plAssertUniqueRelation as logical, pcOperation as character, output phQualBuffer as handle, output pcOutField as character):
        define variable cOtherTableFieldMap as character no-undo.
        define variable oDatasetGetManager  as IDatasetGetManager no-undo.
         
        phQualBuffer = BufferUtil:GetBufferFromUrlQualifier(pMainBuffer, pcQualifier).
        
        if not valid-handle(phQualbuffer) then 
            undo, throw new IllegalArgumentError(subst("'&1' is not a valid qualifier for child tables of '&2'",
                                                         pcQualifier,
                                                         BufferUtil:GetRowName(pMainBuffer)
                                                           )).
        
        if plAssertUniqueRelation then 
        do: 
             AssertNotOneToMany(pMainBuffer, subst("&1&3&2",pcQualifier,pcField,TableDelimiter),phQualbuffer:parent-relation,pcOperation).
        end.
        
        cOtherTableFieldMap = FieldMapholder:GetFieldMap(phQualBuffer).
 
        pcOutField = StringUtil:GetUseName(pcField, cOtherTableFieldMap).
    end method.  
 
    
       
    // override to use field map to provide the temp-table field name for the main buffer
    method protected override character GetUseFieldName(phBuffer as handle,pcfield as characte):
        define variable cMap      as character no-undo.
        define variable cUseField as character no-undo.
        cMap = FieldMapHolder:GetFieldMap(phBuffer).
        return StringUtil:GetUseName(pcField, cMap) .
    end method.  
     
end class.