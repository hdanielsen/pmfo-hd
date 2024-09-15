 
 /*------------------------------------------------------------------------
    File        : QueryEntryBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Apr 07 10:33:37 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryWalker.
using Pmfo.Core.Common.IColumnColumnMap from propath.
using Ccs.BusinessLogic.IQueryGroup from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.Collection from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Ccs.Common.Support.ICharacterArrayHolder from propath.
using Pmfo.Core.DataLayer.IDataSourceMap from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.BusinessLogic.CompareQueryPredicate from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryEntryBuilder from propath.
using Pmfo.Core.BusinessLogic.QueryEntry from propath.
using Pmfo.Core.BusinessLogic.ModuloQueryPredicate from propath.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.QueryBuilder.QueryEntryBuilder inherits QueryWalker: 

    define protected property ColumnMap     as IColumnColumnMap no-undo get. set.
    define protected property SourceMap     as IDataSourceMap   no-undo get. set.
    
    define protected property CurrentCollection as ICollection no-undo get. set.
     
    define protected property TopCollection as ICollection no-undo get. set.
    
    define public property QueryEntry as IQueryEntry 
        get():
            define variable oArray as IQueryEntry extent no-undo.
            if TopCollection:Size > 1 then 
            do:
                 return new QueryGroup(cast(TopCollection:ToArray(),IQueryEntry)).  
            end.
            else if TopCollection:Size = 1 then
            do:
                oArray = cast(TopCollection:ToArray(),IQueryEntry).
                return oArray[1]. 
            end.
            return ?.    
        end.
    
    constructor public QueryEntryBuilder (pMap as IColumnColumnMap  ):
        super ().
        ColumnMap = pMap.
    end constructor.
    
    constructor public QueryEntryBuilder (pMap as IColumnColumnMap, pSourceMap as IDataSourceMap ):
        this-object (pMap).
        SourceMap = pSourceMap.
    end constructor.
    
    constructor public QueryEntryBuilder ( pSourceMap as IDataSourceMap ):
        SourceMap = pSourceMap.
    end constructor.
    
    method override public void Traverse( input pQueryGroup as IQueryGroup ):
        define variable oParentCollection as ICollection no-undo.
        define variable oGroup            as QueryGroup  no-undo.
        define variable lTop              as logical no-undo.
       
        if not valid-object(CurrentCollection) then
        do: 
            lTop = true.
            oParentCollection = new Collection(). 
        end.         
        else
            oParentCollection = CurrentCollection.
        
        CurrentCollection = new Collection().
        super:Traverse(input pQueryGroup).
        
        oGroup = new QueryGroup(cast(CurrentCollection:ToArray(),IQueryEntry)).
        oGroup:Join = pQueryGroup:Join.
        oParentCollection:Add(oGroup). 
        
        if lTop then
        do:
            TopCollection = oParentCollection.
        end.    
        else     
            CurrentCollection = oParentCollection.
    end method.

    method override public void Traverse( input pPredicate as IQueryPredicate ):
        define variable cDBField      as character no-undo.
        define variable oPredicate    as QueryPredicate no-undo.
        define variable oNewPredicate as QueryPredicate no-undo.
        define variable cValues       as character extent no-undo.
        define variable cValueList    as character no-undo.
        define variable i             as integer no-undo.
        define variable oNewEntry     as IQueryEntry no-undo.
        
        if valid-object(ColumnMap) then
            cDbField = ColumnMap:Get(pPredicate:FieldName).
        else 
            cDbField = SourceMap:ColumnSource(pPredicate:FieldName).    
        if cdbfield = ? then 
            undo, throw new IllegalOperationError(subst("'&1' is not a valid field name in the temp-table",pPredicate:fieldname)).
        
        // NOTE: we create new objects since these preficates may be used on client after this and will fail if we changed the 
        // field name to databaser field name   
        if type-of(pPredicate,CompareQueryPredicate) then 
        do:
            oPredicate = cast(pPredicate,QueryPredicate).
            if extent(oPredicate:StringValues) <> ? then
                 oNewPredicate = new CompareQueryPredicate(cDbField,oPredicate:Operator,oPredicate:StringValues).
            else
                 oNewPredicate = new CompareQueryPredicate(cDbField,oPredicate:Operator,oPredicate:StringValue).
            cast(oNewPredicate,CompareQueryPredicate):Strength = cast(pPredicate,CompareQueryPredicate):Strength.
                
        end.
        else if type-of(pPredicate,ModuloQueryPredicate) then 
        do:
            oPredicate = cast(pPredicate,QueryPredicate).
            oNewPredicate = new ModuloQueryPredicate(
                               cDbField,
                               cast(oPredicate,ModuloQueryPredicate):ModuloBase,
                               cast(oPredicate,ModuloQueryPredicate):ModuloOperator,
                               integer(oPredicate:StringValue)).
                 
        end.
        
        else if type-of(pPredicate,QueryPredicate) then 
        do:
            oPredicate = cast(pPredicate,QueryPredicate).
            if extent(oPredicate:StringValues) <> ? then
                 oNewPredicate = new QueryPredicate(cDbField,oPredicate:Operator,oPredicate:StringValues).
            else
                 oNewPredicate = new QueryPredicate(cDbField,oPredicate:Operator,oPredicate:StringValue).
        end.
        else do:
            if extent(pPredicate:Values) <> ? then 
            do:
                if type-of(pPredicate:Values,ICharacterArrayHolder) then
                    cValues = cast(pPredicate:Values,ICharacterArrayHolder):Value.
                else do:    
                    cValueList = pPredicate:Values:toString().
                    extent(cValues) = num-entries(cValueList).
                    do i = 1 to extent(cValues):
                        cValues[i] = entry(i,cValueList).
                    end.    
                end.
                oNewPredicate = new QueryPredicate(cDbField,pPredicate:Operator,cValues).        
            end.    
            else do:
                oNewPredicate = new QueryPredicate(cDbField,pPredicate:Operator,pPredicate:Value:ToString()).        
            end.
          
        end.       
        oNewPredicate:Join = pPredicate:Join.     
        
        if not valid-object(CurrentCollection) then 
        do:
            CurrentCollection = new Collection().
            TopCollection     = CurrentCollection.
            
        end.    
        if valid-object(SourceMap) then
        do:
            // allow overiders to deal with this differently 
            // they could return a more complex entry 
            // or return null if it need to be handled elsewhere 
            //  (for example fin a before-row event)
            oNewEntry = SourceMap:ColumnExpression(oNewPredicate).
            if valid-object(oNewEntry) then 
            do:
                CurrentCollection:Add(oNewEntry).
                cast(oNewEntry,QueryEntry):Join = oNewPredicate:Join. 
            end.    
            else 
                CurrentCollection:Add(oNewPredicate ).
                
        end.
        else
            CurrentCollection:Add(oNewPredicate). 
            
    end method.
end class.