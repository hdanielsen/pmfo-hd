 
 /*------------------------------------------------------------------------
    File        : QueryDefinition
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 03 07:57:44 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

 
block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.BusinessLogic.QueryDefinition from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.BusinessLogic.IQueryDefinition from propath.
using Pmfo.Core.BusinessLogic.IQueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchPredicates from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchSort from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchFields from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchSortFields from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchPredicateFields from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedFields from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedPredicates from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedSortFields from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedPredicateFields from propath.

class Pmfo.Core.BusinessLogic.QueryDefinition inherits QueryDefinition implements IQueryDefinition: 

    constructor public QueryDefinition (  ):
        super ().
    end constructor.

    constructor public QueryDefinition ( input pEntry as IQueryEntry ):
        super (input pEntry).
    end constructor.
    
    constructor public QueryDefinition ( input pEntry as IQuerySortEntry extent ):
        Assert:HasDeterminateExtent(pEntry, 'Query sort entries').
        QuerySort = pEntry.
    end constructor.
    
    constructor public QueryDefinition ( input pEntry as IQuerySortEntry ):
        define variable oEntry as IQuerySortEntry extent 1 no-undo.
        Assert:NotNull(pEntry, 'Query sort entry').
        oEntry[1] = pEntry.
        QuerySort = oEntry.
    end constructor.
    
    constructor public QueryDefinition ( input pEntry as IQueryEntry, input pSort as IQuerySortEntry extent ):
        super (input pEntry, input pSort).
    end constructor.

    method override public character ToString():
        define variable cExpression as character no-undo.
        define variable i           as integer no-undo.
        if valid-object(QuerySelection) then 
            cExpression = "where " + QuerySelection:ToString().
        do i = 1 to extent(QuerySort):
            cExpression = cExpression 
                        + (if cExpression = "" then "" else " ")
                        + QuerySort[i]:ToString().
        end.
        return cExpression.    
    end.    
    
    method public character GetFieldReferences().
        define variable oQuerySearch as QuerySearchFields no-undo.
        oQuerySearch  = new QuerySearchFields().
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:FieldNames.  
    end method.
    
    method public character GetSortFieldReferences().
        define variable oQuerySearch as QuerySearchSortFields no-undo.
        oQuerySearch  = new QuerySearchSortFields().
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:FieldNames.  
    end method.
    
    method public character GetPredicateFieldReferences().
        define variable oQuerySearch as QuerySearchPredicateFields no-undo.
        oQuerySearch  = new QuerySearchPredicateFields().
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:FieldNames.  
    end method.
    
    method public character GetPredicateFieldReferences(pOperator as QueryOperatorEnum).  
        define variable oQuerySearch as QuerySearchPredicateFields no-undo.
        oQuerySearch  = new QuerySearchPredicateFields(pOperator).
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:FieldNames.  
    end method.
    
    method public character GetPredicateFieldReferences(pOperators as QueryOperatorEnum extent).
        define variable oQuerySearch as QuerySearchPredicateFields no-undo.
        oQuerySearch  = new QuerySearchPredicateFields(pOperators).
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:FieldNames.  
      
    end method.
    
    method public character GetPredicateFieldEqualMatches().  
        define variable oQuerySearch as QuerySearchPredicateFields no-undo.
        define variable oOperators   as QueryOperatorEnum extent 3 no-undo.
        
        oOperators[1] = QueryOperatorEnum:Eq. 
        oOperators[2] = QueryOperatorEnum:Begins. 
        oOperators[3] = QueryOperatorEnum:InList.           
        oQuerySearch  = new QuerySearchPredicateFields(true,oOperators).
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:FieldNames.  
    end method.
    
    method public character GetPredicateFieldRangeMatches().  
        define variable oQuerySearch as QuerySearchPredicateFields no-undo.
        define variable oOperators   as QueryOperatorEnum extent 5 no-undo.
        //  begins is currently included in GetPredicateFieldEqualMatches 
        oOperators[1] = QueryOperatorEnum:Ge.
        oOperators[2] = QueryOperatorEnum:Gt.
        oOperators[3] = QueryOperatorEnum:Le.
        oOperators[4] = QueryOperatorEnum:Lt.
        oOperators[5] = QueryOperatorEnum:InRange.
                
        oQuerySearch  = new QuerySearchPredicateFields(true,oOperators).
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:FieldNames.  
    end method.
    
    method public character GetQualifiedTableReferences().    
        define variable oQuerySearch as QuerySearchQualifiedFields no-undo.
        oQuerySearch  = new QuerySearchQualifiedFields().
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:TableReferences.
    end method.    
    
    method public character GetQualifiedPredicateTableReferences().    
        define variable oQuerySearch as QuerySearchQualifiedPredicateFields no-undo.
        oQuerySearch  = new QuerySearchQualifiedPredicateFields().
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:TableReferences.
    end method.  
   
    method public character GetQualifiedSortTableReferences().    
        define variable oQuerySearch as QuerySearchQualifiedSortFields no-undo.
        oQuerySearch  = new QuerySearchQualifiedSortFields().
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:TableReferences.
    end method.  
    
    method public IQueryEntry extent GetEntries(pcField as character):
        define variable oColl as ICollection no-undo.
        oColl = SearchEntries(pcField). 
        return cast(oColl:ToArray(),IQueryEntry).
    end method.
    
    method public IQueryEntry extent GetEntries(pcFields as character extent):
        define variable oColl as ICollection no-undo.
        oColl = SearchEntries(pcFields). 
        return cast(oColl:ToArray(),IQueryEntry).
    end method.
    
    method public IQueryEntry extent GetEntries(pcField as character, pOperator as QueryOperatorEnum):
        define variable oColl as ICollection no-undo.
        oColl = SearchEntries(pcField,pOperator). 
        return cast(oColl:ToArray(),IQueryEntry).
    end method.
    
    method public IQueryEntry extent GetEntries(pcFields as character extent, pOperator as QueryOperatorEnum):
        define variable oColl as ICollection no-undo.
        oColl = SearchEntries(pcFields,pOperator). 
        return cast(oColl:ToArray(),IQueryEntry).
    end method.
    
    method public IQueryEntry extent GetQualifiedEntries(pcQualifier as character):
        define variable oColl as ICollection no-undo.
        oColl = SearchQualifiedEntries(pcQualifier). 
        return cast(oColl:ToArray(),IQueryEntry).
    end method.
    
    method public logical HasPredicate(pcField as character).
        define variable oColl as ICollection no-undo.
        oColl = SearchEntries(pcField). 
        return oColl:size > 0.
    end method.
    
    method public logical HasPredicate(pcFields as character extent).
        define variable oColl as ICollection no-undo.
        oColl = SearchEntries(pcFields). 
        return oColl:size > 0.
    end method.
    
    method public logical HasPredicate(pcField as character, pOperator as QueryOperatorEnum).
        define variable oColl as ICollection no-undo.
        oColl = SearchEntries(pcField,pOperator). 
        return oColl:size > 0.
    end method.
    
    method public logical HasPredicate(pcFields as character extent, pOperator as QueryOperatorEnum).
        define variable oColl as ICollection no-undo.
        oColl = SearchEntries(pcFields,pOperator). 
        return oColl:size > 0.
    end method.
    
    method protected ICollection SearchEntries(pcFields as character extent, pOperator as QueryOperatorEnum):
        define variable oQuerySearch as QuerySearchPredicates no-undo.
        oQuerySearch  = new QuerySearchPredicates(pcFields,pOperator).
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:QueryEntryCollection.
    end method.
    
    method protected ICollection SearchEntries(pcFields as character extent):
        define variable oQuerySearch as QuerySearchPredicates no-undo.
        oQuerySearch  = new QuerySearchPredicates(pcFields).
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:QueryEntryCollection.
    end method.
    
    method protected ICollection SearchEntries(pcField as character, pOperator as QueryOperatorEnum):
        define variable oQuerySearch as QuerySearchPredicates no-undo.
        oQuerySearch  = new QuerySearchPredicates(pcField,pOperator).
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:QueryEntryCollection.
    end method.
    
    method protected ICollection SearchEntries(pcField as character):
        define variable oQuerySearch as QuerySearchPredicates no-undo.
        oQuerySearch  = new QuerySearchPredicates(pcField).
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:QueryEntryCollection.
    end method.   
    
    method protected ICollection SearchQualifiedEntries(pcQualifier as character):
        define variable oQuerySearch as QuerySearchQualifiedPredicates no-undo.
        oQuerySearch  = new QuerySearchQualifiedPredicates(pcQualifier).
        oQuerySearch:Traverse(this-object).
        return oQuerySearch:QueryEntryCollection.
    end method.
    
    method public IQuerySortEntry extent GetSortEntries(pcField as character):
        define variable cOneField as character extent 1 no-undo.
        cOneField[1] = pcField.
        return GetSortEntries(cOneField).
    end method.
    
    method public IQuerySortEntry extent GetSortEntries(pcFields as character extent):
        define variable oQuerySearch as QuerySearchSort no-undo.
        oQuerySearch  = new QuerySearchSort(pcFields).
        oQuerySearch:Traverse(this-object).
        return cast(oQuerySearch:SortEntries:ToArray(),IQuerySortEntry).
    end method.
    
    method public IQueryPredicate GetPredicate(pcField as character, pOperator as QueryOperatorEnum):
        define variable oEntries as IQueryEntry extent no-undo.
        oEntries = GetEntries(pcField,pOperator).
        if extent(oEntries) = 1 and type-of(oEntries[1],IQueryPredicate) then
            return cast(oEntries[1],IQueryPredicate).
        else 
            return ?.
    end method.   
    
    method public IQueryPredicate GetPredicate(pcField as character):
        define variable oEntries as IQueryEntry extent no-undo.
        oEntries = GetEntries(pcField).
        if extent(oEntries) = 1 and type-of(oEntries[1],IQueryPredicate) then
            return cast(oEntries[1],IQueryPredicate).
        else 
            return ?.
    end method.   
    
    method public logical RemoveEntries(pQueryEntries as IQueryEntry extent):
        define variable i   as integer no-undo.
        define variable lOk as logical no-undo.
        define variable lRemoved as logical no-undo.
        lok = true.
        do i = 1 to extent(pQueryEntries):
            lRemoved = RemoveEntry(pQueryEntries[i]).
            if lRemoved = false then
                lOk = false.
        end.    
        return lok.
    end method.    
    
    method public logical RemoveEntry(pQueryEntry as IQueryEntry):
        define variable lok as logical no-undo.
        define variable oGroup as IQueryGroup no-undo.
        if valid-object(QuerySelection) and QuerySelection = pQueryEntry then
        do:
            QuerySelection = ?.
            return true.
        end.
        else if type-of(QuerySelection,IQueryPredicate) then
        do:
            return false. 
        end.    
        if type-of(QuerySelection,IQueryGroup) then
        do:
            oGroup = cast(QuerySelection,IQueryGroup).
            lok = oGroup:RemoveEntry(pQueryEntry).
            if lok and extent(oGroup:Entries) = 1 then 
            do:
                QuerySelection = oGroup:Entries[1].
            end.    
            return lok.
        end.
        else 
            undo, throw new IllegalOperationError(subst("Cannot remove entry from group of type &1",QuerySelection:GetClass():TypeName)).       
    end method.
    
    method public logical AddEntries(pQueryEntries as IQueryEntry extent).
        define variable oGroup as IQueryGroup no-undo.
        if extent(pQueryEntries) > 1 then
        do: 
            oGroup = new QueryGroup(pQueryEntries).
            return AddEntry(oGroup).
        end.
        else 
            return AddEntry(pQueryEntries[1]).
    end method.    
    
    method public logical AddEntry(pQueryEntry as IQueryEntry):
        define variable oGroup as IQueryGroup no-undo.
        if not valid-object(QuerySelection) then
            QuerySelection = pQueryEntry.
        else if type-of(QuerySelection,IQueryGroup) then
            cast(QuerySelection,IQueryGroup):AddEntry(pQueryEntry).
        else     
            QuerySelection = new QueryGroup(cast(Array:ToArray(QuerySelection,pQueryEntry),IQueryEntry)).
        return true.
    end method.   
    
    method public logical AnyOr():
        define variable oPredicate as IQueryPredicate no-undo.
        define variable oGroup as IQueryGroup no-undo.
        define variable oCssGroup as Ccs.BusinessLogic.IQueryGroup no-undo.
        define variable i as integer no-undo.
        if not valid-object(QuerySelection) then
        do:
            return false.
        end.    
        else if type-of(QuerySelection,IQueryPredicate) then
        do:
            return false.
        end.    
        else if type-of(QuerySelection,IQueryGroup) then
        do:
            return oGroup:AnyOr().
        end.
        else if type-of(QuerySelection,Ccs.BusinessLogic.IQueryGroup) then
        do:
            oCssGroup = cast(QuerySelection,Ccs.BusinessLogic.IQueryGroup).
            do i = 1 to extent(oCssGroup:Entries):
                case oCssGroup:Entries[i]:Join:
                    when JoinEnum:Or or when JoinEnum:OrNot then 
                        return true.
                end.    
            end. 
            return false.     
        end. 
        else // there should not be any more types.. 
            undo, throw new IllegalOperationError(subst("Cannot get AnyOr() from type &1",QuerySelection:GetClass():TypeName)).        
     end method. 
    
    method public IQuerySortEntry extent RemoveSort():
        define variable oSort as IQuerySortEntry extent no-undo.
        if extent(QuerySort) <> ? then 
        do:
            oSort = QuerySort.
            extent(QuerySort) = ?.
        end.    
        return oSort.
    end.    
    
    method public logical SetSort(pSort as IQuerySortEntry):
        define variable oSort as IQuerySortEntry extent 1 no-undo.
        if extent(QuerySort) <> ? then 
        do:
            extent(QuerySort) = ?.
        end.    
        oSort[1] = pSort.
        QuerySort = oSort.
        return true.
    end.
    
    method public logical SetSort(pSort as IQuerySortEntry extent):
        if extent(QuerySort) <> ? then 
        do:
            extent(QuerySort) = ?.
        end.    
        QuerySort = pSort.
        return true.
    end method.
    
   
    
end class.