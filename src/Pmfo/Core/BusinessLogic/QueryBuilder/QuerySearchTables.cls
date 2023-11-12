 
 /*------------------------------------------------------------------------
    File        : QuerySearchTables
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Apr   2019
    Notes       : Build a list of table references for query and sort 
                  after the fields have been qualifed with table name
                - Used to find out where to insert a query entry with OR
                  join of components with different tables 
                - Sort is for completeness. Not needed for the current use 
                  and won't be reached either as we will start search down 
                  in the query     
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryWalker from propath.
using Pmfo.Core.Common.IColumnColumnMap from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.DataLayer.IDataSourceMap from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchTables inherits QueryWalker: 
    
    define protected property SourceMap    as IDataSourceMap   no-undo get. set.
  //  define protected property ColumnMap  as IColumnColumnMap no-undo get. set.
    define public    property Tables     as character        no-undo get. set.
       
    constructor public QuerySearchTables (pmap as IDataSourceMap ):
        super ().
        SourceMap = pmap.
    end constructor.

    method override public void Traverse( input pPredicate as IQueryPredicate ):
         AddFieldTable(pPredicate:FieldName).
    end method.

    method override public void Traverse( input pSortEntry as IQuerySortEntry ):
        AddFieldTable(pSortEntry:FieldName).
    end method.
    
    method protected logical AddFieldTable(pcColumn as char):
        define variable cDbField as character no-undo.
        define variable cTable as character no-undo.
        
        cDbField = SourceMap:ColumnSource(pcColumn).
        if cdbfield = ? then 
            undo, throw new IllegalOperationError(subst("'&1' is not a valid field name in the temp-table",pcColumn)).
        
        if cDbField > "" then
        do:
            Assert:isTrue(num-entries(cDbField,".") = 2,subst("Column &1 is qualified with table",cDbField)).
            ctable = entry(1,cDbField,".").
            if lookup(cTable,Tables) = 0 then
                Tables = Tables 
                       + (if Tables = "" then "" else ",")
                       + cTable.
        end.                
    end method.    
end class.