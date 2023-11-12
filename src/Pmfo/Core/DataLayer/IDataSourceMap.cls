
/*------------------------------------------------------------------------
    File        : IDataSourceMap
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Feb 14 08:36:06 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.

interface Pmfo.Core.DataLayer.IDataSourceMap:  
    /* return the db column mapped to the clinet column  
       Will typically return mapped column from buffer:data-source-complete-map, 
       but can be used to return a non existing qualified name for calc columns
       that then can be manipulated in ColumnSortExpression and ColumnExpression  
       */
    method public character              ColumnSource(pcColumn as char).
    
    /* called with the a SortEntry built on db colu8mn name returned from ColumnSource 
       allows more complex sort to be returned for example fpor calculated columns 
    */
    method public IQuerySortEntry extent ColumnSort (pSort as IQuerySortEntry).
    
    /* called with the a QueryPredicate built on db colu8mn name returned from ColumnSource 
       allows different or more complex query to be returned for example fpor calculated columns 
    */
    method public IQueryEntry            ColumnExpression(pPredicate as IQueryPredicate). 
end interface.