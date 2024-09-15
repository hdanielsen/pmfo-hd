
/*------------------------------------------------------------------------
    File        : IGetDataTableRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 03 23:59:48 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.INamedQuery from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.

interface Pmfo.Core.BusinessLogic.IGetDataTableRequest inherits IGetDataTableRequest:
     
    /* Returns   paging context */
    define public property PagingContextEntry  as IQueryEntry no-undo get. set.  
    define public property NamedQuery          as INamedQuery no-undo get. set. 
    define public property FindUnique          as logical     no-undo  get. set.
    define public property Count               as logical     no-undo  get. set.
    define public property PageBackward        as logical     no-undo  get. set.
    define public property ClientOnly          as logical     no-undo  get. set.
    define public property SortExpression      as character   no-undo  get.  
    
    // requested expand internal table names 
    define public property ClientExpands       as character   no-undo  get. set.
    // derrived from expands with option innerjoin=true  
    define public property InnerJoinExpands    as character   no-undo  get. set.
    // derrived from spply with qualified aggregate options  
    define public property InnerJoinAggregates as character   no-undo  get. set.
    // set in request to specify that child join table is server only for cases where the 
    // current automatic logic does not work 
    //(i.e if the qualified filter is on on a grand child field it will currently not be 
    // detected by the DataAccess when managing innerjoins   
    // as of current it will only be used for cases were this table is not expanded 
    define public property ManualServerInnerJoins as character   no-undo  get. set.
    
    /* 
    Optional list of fields to select. 
    The request will (can) still use the full temp-table, but the DataSource can 
    choose to not populate other fields. This can thus be used to decide wheteher 
    to skip the after-row event and/or joins for performance reasons
      
    Use with care! Must only be used for web/stateless requests that does not 
    keep the BE around (unless one implement a dynamic temp-table with only 
    requested fields)   
    */  
    define public property SelectFields        as character no-undo get. set. 
    
    // set to true when paging context props is to be sent to external client
    // to be passed back as url query props 
    // (this will add serialize-name as field name in props)    
    define public property ExternalPaging     as logical no-undo  get. set.
    
    // set to true by DataRequestBuilder so the server can identify where to support innerjoin  
    define public property IsMainTable        as logical no-undo  get. set.
     // conventience to check if paging instead of checking top, skip and PagingContextEntry
    define public property IsPaging as logical get. 
    
    method public logical HasSort().   
    method public logical HasSort(pcField as character).
    method public logical HasSort(pcFields as character extent).
    
        // returns a comma separated list of all fields references in predicates and sort 
    method public character GetFieldReferences().
        // returns a comma separated list of all fields references in sort  
    method public character GetSortFieldReferences().
        // returns a comma separated list of all fields references in predicates 
    method public character GetPredicateFieldReferences().    
    method public character GetPredicateFieldReferences(pOperator as QueryOperatorEnum).  
    method public character GetPredicateFieldReferences(pOperators as QueryOperatorEnum extent).  
    method public character GetPredicateFieldEqualMatches().  
    method public character GetPredicateFieldRangeMatches().  
    
    // Return union of InnerJoinExpands, InnerJoinAggregates and QueryDefinition:GetQualifiedTableReferences
    method public character GetInnerJoins().
    // Return union of InnerJoinExpands, InnerJoinAggregates and QueryDefinition:GetQualifiedTableReferences with
    // output pcServerInnerJoins for references ONLY found in query (used to set no fill on server for performance)  
    method public character GetInnerJoins(output pcServerInnerJoins as character).
    
    method public character GetQualifiedTableReferences().    
    method public character GetQualifiedPredicateTableReferences().    
    method public character GetQualifiedSortTableReferences().    
    
      
    method public IQueryEntry extent GetQueryEntries(pcField as character).
    method public IQueryEntry extent GetQueryEntries(pcField as character, pOperator as QueryOperatorEnum).
    method public IQueryEntry extent GetQueryEntries(pcFields as character extent).
    method public IQueryEntry extent GetQueryEntries(pcFields as character extent, pOperator as QueryOperatorEnum).
    method public IQueryEntry extent GetQualifiedQueryEntries(pcQualifier as character).
    
    method public logical HasPredicate(pcField as character).
    method public logical HasPredicate(pcFields as character extent).
    method public logical HasPredicate(pcField as character, pOperator as QueryOperatorEnum).
    method public logical HasPredicate(pcFields as character extent, pOperator as QueryOperatorEnum).
    
    
    method public IQueryPredicate GetQueryPredicate(pcField as character, pOperator as QueryOperatorEnum).
    method public IQueryPredicate GetQueryPredicate(pcField as character).
    method public logical RemoveQueryEntry(pQueryEntry as IQueryEntry).
    method public logical AddQueryEntry(pQueryEntry as IQueryEntry).
    method public logical HasQueryEntry(pQueryEntry as IQueryEntry).
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pcValue as character).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,piValue as int64).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,plValue as logical).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as decimal).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as date).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime-tz).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as IPrimitiveHolder).
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pcValues as character extent).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,piValues as int64 extent).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,plValues as logical extent).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as decimal extent).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as date extent).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime extent).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime-tz extent).
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValueHolder as IPrimitiveArrayHolder).
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pcValue as character).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,piValue as int64).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,plValue as logical).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as decimal).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as date).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as datetime-tz).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValue as IPrimitiveHolder).
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pcValues as character extent).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,piValues as int64 extent).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,plValues as logical extent).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as decimal extent).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as date extent).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime extent).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as datetime-tz extent).
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValueHolder as IPrimitiveArrayHolder).
    
    method public logical RemoveQueryEntries(pQueryEntries as IQueryEntry extent).
    method public logical AddQueryEntries(pQueryEntries as IQueryEntry extent).
    method public IQuerySortEntry extent RemoveQuerySort().
    method public logical SetQuerySort(pSort as IQuerySortEntry).
    method public logical SetQuerySort(pSort as IQuerySortEntry extent).
     
end interface.