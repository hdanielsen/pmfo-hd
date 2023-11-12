
/*------------------------------------------------------------------------
    File        : IQueryDefinition
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jun 18 20:14:21 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.BusinessLogic.IQueryDefinition.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.

interface Pmfo.Core.BusinessLogic.IQueryDefinition inherits IQueryDefinition:  
    
    // returns a comma spearted list of all fields references in predicates and sort 
    method public character GetFieldReferences().
           // returns a comma spearted list of all fields references in predicates  
    method public character GetSortFieldReferences().
        // returns a comma spearted list of all fields references in  sort 
    method public character GetPredicateFieldReferences().    
    method public character GetPredicateFieldReferences(pOperator as QueryOperatorEnum).  
    method public character GetPredicateFieldReferences(pOperators as QueryOperatorEnum extent).  
    method public character GetPredicateFieldEqualMatches().  
    method public character GetPredicateFieldRangeMatches().  
    
    // returns a comma separted list of all qualifiers found in field references in predicate and sort
    method public character GetQualifiedTableReferences().
    // returns a comma separated list of all qualifiers found in predicates
    method public character GetQualifiedPredicateTableReferences().        
    // returns a comma separated list of all qualifiers found in sort
    method public character GetQualifiedSortTableReferences().        
    
    method public IQuerySortEntry extent GetSortEntries(pcField as character).
    method public IQuerySortEntry extent GetSortEntries(pcFields as character extent).
    
    
    method public IQueryEntry extent GetEntries(pcField as character).
    method public IQueryEntry extent GetEntries(pcFields as character extent).
    method public IQueryEntry extent GetEntries(pcField as character, pOperator as QueryOperatorEnum).
    method public IQueryEntry extent GetEntries(pcFields as character extent, pOperator as QueryOperatorEnum).
    method public IQueryEntry extent GetQualifiedEntries(pcQualifier as character).
    
        
    method public logical HasPredicate(pcField as character).
    method public logical HasPredicate(pcFields as character extent).
    method public logical HasPredicate(pcField as character, pOperator as QueryOperatorEnum).
    method public logical HasPredicate(pcFields as character extent, pOperator as QueryOperatorEnum).
    
    method public IQueryPredicate GetPredicate(pcField as character, pOperator as QueryOperatorEnum).
    method public IQueryPredicate GetPredicate(pcField as character).
    
    method public logical AnyOr().
    method public logical RemoveEntry(pQueryEntry as IQueryEntry).
    method public logical AddEntry(pQueryEntry as IQueryEntry).
    method public logical RemoveEntries(pQueryEntries as IQueryEntry extent).
    method public logical AddEntries(pQueryEntries as IQueryEntry extent).
    method public IQuerySortEntry extent RemoveSort().
    method public logical SetSort(pSort as IQuerySortEntry).
    method public logical SetSort(pSort as IQuerySortEntry extent).
      
end interface.