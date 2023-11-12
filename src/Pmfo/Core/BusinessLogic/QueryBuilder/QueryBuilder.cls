 
 /*------------------------------------------------------------------------
    File        : QueryBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Apr 07 01:57:57 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.IQueryDefinition from propath.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQueryGroup from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryEntryBuilder from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchTables from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryWalker from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.
using Pmfo.Core.Common.ColumnColumnMap from propath.
using Pmfo.Core.Common.IColumnColumnMap from propath.
using Pmfo.Core.DataLayer.IDataSourceMap from propath.
using Pmfo.Util.QueryUtil from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QueryBuilder inherits QueryWalker: 
    
    define protected property SourceMap    as IDataSourceMap   no-undo get. set.
//    define protected property ColumnMap    as IColumnColumnMap no-undo get. set.
    
    define protected property DbTables     as character no-undo get. set.
    define protected property BaseQuery    as character no-undo get. set.   
    define public property WhereClauses    as character extent no-undo get. protected set.
    define public property SortExpression  as character no-undo get. protected set.
     
    constructor public QueryBuilder (pmap as IDataSourceMap,pDBTables as char,pBaseQuery as char):
        super ().
        SourceMap = pmap.
        DbTables = pDbTables.
        BaseQuery = pBaseQuery. 
        extent(WhereClauses) = num-entries(pDBTables).
    end constructor.
    
    method protected void ClearBuildData():
        define variable i as integer no-undo.
        do i = 1 to extent(WhereClauses):
            WhereClauses[i] = "".
        end.    
        SortExpression = "".
        
/*        if valid-object(ColumnMap) then*/
/*            delete object ColumnMap.   */
       
    end method.
    
    method public character Build (pTableRequest as IGetDataTableRequest,pClientBuffer as handle ):
        define variable cQuery  as character no-undo.
        Assert:NotNull(pTableRequest,"Table Request").
        
        ClearBuildData().
        
     //   ColumnMap = CreateColumnMap(pClientBuffer).    
        
        Traverse(pTableRequest).
        
        cQuery = QueryUtil:InsertQueryExpressions(BaseQuery, WhereClauses).
        if SortExpression > "" then
            cQuery = QueryUtil:SetSort(cQuery, SortExpression). 
        return cQuery.
    end method.      
    
    method public character Build (pQueryDefinition as IQueryDefinition,pClientBuffer as handle ):
        define variable cQuery  as character no-undo.
        Assert:NotNull(pQueryDefinition,"Query Definition").
        
        ClearBuildData().
        
     //   ColumnMap = CreateColumnMap(pClientBuffer).    
        
        Traverse(pQueryDefinition).
       
        cQuery = QueryUtil:InsertQueryExpressions(BaseQuery, WhereClauses).
        if SortExpression > "" then
            cQuery = QueryUtil:SetSort(cQuery, SortExpression). 
        return cQuery.
    end method.      
    
    method public character BuildFind (pQueryEntry as IQueryEntry,pClientBuffer as handle ):
        define variable cQuery  as character no-undo.
        cQuery = Build(pQueryEntry,pClientBuffer). 
        
        cQuery = QueryUtil:RemoveQueryOptions(cQuery).
        
        return left-trim(substr(cQuery,index(cQuery," where "))).
   
    end method.      
   
    method public character Build (pQueryEntry as IQueryEntry,pClientBuffer as handle ):
        define variable cQuery  as character no-undo.
        Assert:NotNull(pQueryEntry,"Query Entry").
        
        ClearBuildData().
        
      //  ColumnMap = CreateColumnMap(pClientBuffer).    
        
        Traverse(pQueryEntry).
        cQuery = QueryUtil:InsertQueryExpressions(BaseQuery, WhereClauses).
       
        return cQuery.
    end method.       
    
    method public character Build (pQuerySort as IQuerySortEntry extent,pClientBuffer as handle ):
        define variable cQuery  as character no-undo.
        
        ClearBuildData().
        
      //  ColumnMap = CreateColumnMap(pClientBuffer).    
        
        Traverse(pQuerySort).
        
        if SortExpression > "" then
            cQuery = QueryUtil:SetSort(BaseQuery, SortExpression). 
        
        return cQuery.
    end method.      
      
    method protected QuerySearchTables CreateSearch().
        return new QuerySearchTables(SourceMap).
    end method. 
       
    method protected IColumnColumnMap CreateColumnMap(pClientBuffer as handle):
        define variable i    as integer no-undo.
        define variable oMap as IColumnColumnMap no-undo.
        define variable hFld as handle no-undo.
        
        Assert:IsTrue(pClientBuffer:type = "buffer","ClientBuffer is type 'buffer'").
        oMap = new ColumnColumnMap().
         
        do i = 1 to pClientBuffer:num-fields:
            hFld = pClientBuffer:buffer-field(i).
            oMap:Put(hFLd:name,SourceMap:ColumnSource(hFld:name)). 
        end.
        return oMap.
    end method.
    
    method protected logical AnyOr(pGroup as IQueryGroup):
        define variable i      as integer no-undo.
        define variable oEntry as IQueryEntry no-undo.
        do i = 1 to extent(pGroup:Entries):
            oEntry = pGroup:entries[i].
            case oEntry:Join:
                when JoinEnum:Or or when JoinEnum:OrNot then 
                    return true.
            end.    
        end.    
        return false.
    end method.    
    
    method protected integer GetTableNumber(pcTables as char):
        define variable i    as integer no-undo.
        define variable iMax as integer no-undo.
        do i = 1 to num-entries(pctables):
            iMax = Max(iMax,lookup(entry(i,pcTables),DbTables)).
        end.
        return iMax.    
    end.  
    
    method override public void Traverse( input pQueryGroup as IQueryGroup ):
        if AnyOr(pQueryGroup) then
        do: 
            AddEntry(pQueryGroup).
        end.    
        else 
            super:Traverse(input pQueryGroup).
    end method.

    method override public void Traverse( input pPredicate as IQueryPredicate ):
        AddEntry(pPredicate).
    end method.
    
    method protected void AddEntry(pQueryEntry as IQueryEntry ):
        define variable oSearch       as QuerySearchTables no-undo.
        define variable oEntry        as IQueryEntry       no-undo.
        define variable oEntryBuilder as QueryEntryBuilder no-undo.
        define variable iTable        as integer no-undo.
        define variable cTables       as character no-undo.
        
        oSearch = createSearch().
        oSearch:Traverse(pQueryEntry).
        ctables = oSearch:Tables.
      
        iTable = GetTableNumber(cTables).
        
        /* iTable 0 this means there fields do not map to a table
           This could be an error, but it could also be a valid 
           temp-table reference in the context of the dataosurce 
           and can be added to the first table
           query-prepare will protest if this is wrong
         */
           
        if iTable = 0 then 
            iTable = 1.

        oEntryBuilder = new QueryEntryBuilder(SourceMap).
        oEntryBuilder:Traverse(pQueryEntry).
        
        WhereClauses[iTable] = WhereClauses[iTable]
                             + (if WhereClauses[iTable] = "" then "" else " and ")
                             +  oEntryBuilder:QueryEntry:ToString().
    end method.    
    
    method override public void Traverse( input pSort as IQuerySortEntry ):
        define variable cDbColumn as character no-undo.
        define variable i as integer no-undo.
        define variable oQuerySorts as IQuerySortEntry extent no-undo.
        
        cDBColumn = SourceMap:ColumnSource(pSort:FieldName).
        oQuerySorts = SourceMap:ColumnSort(new QuerySortEntry(cDbColumn,pSort:SortOrder)).
        
        do i = 1 to extent(oQuerySorts):
            SortExpression = SortExpression 
                        + (if SortExpression = "" then "" else " ")
                        + oQuerySorts[i]:ToString().
        end.
    end method.
    
end class.