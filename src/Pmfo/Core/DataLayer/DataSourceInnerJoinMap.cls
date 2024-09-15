 
 /*------------------------------------------------------------------------
    File        : DataSourceInnerJoinMap
    Purpose     : IDataSourceMap Adapter for QueryBuilder that redirects column* requests
                  for qualified fields for the xpanded Innerjoins to the correct datasources 
                  and unqualified fields to the main source 
                  Currently used specifically in DataSource prepareinnerJoins 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Sep 21 23:15:58 EDT 2021
    Notes       : manages renamed buffers which is needed in cazses where
                  datasources have many buffers and some of them conflicts 
                  with buffers from other datasources     
----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQuerySortEntry from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.DataLayer.IDataSourceMap from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryRenameTableQualifiers from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.

class Pmfo.Core.DataLayer.DataSourceInnerJoinMap implements IDataSourceMap: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    define protected variable DataSourceMap as IHandleDataSourceMap no-undo.
    define protected variable TTtoDBMap as IStringStringMap no-undo.
    define protected variable TTtoDBRenameMap as IStringStringMap no-undo.
    define protected variable Dataset as handle no-undo.   
    define protected variable MainDataSource as IDataSource no-undo.   
    define protected variable DbFieldSourceMap as character no-undo.
    define protected variable DbFieldRenameMap as character no-undo.
    
    define protected property QueryRenameUtil as QueryRenameTableQualifiers no-undo
         get().
             if not valid-object(QueryRenameUtil) then
             do:
                 QueryRenameUtil = new QueryRenameTableQualifiers(true).  // true - list has rename entry first  
             end.
             return QueryRenameUtil.    
         end.    
         set.
         
    constructor public DataSourceInnerJoinMap (pMainDataSource as IDataSource, poDataSourceMap as IHandleDataSourceMap, pTtToDbmap as IStringStringMap, pTtToDbRenameMap as IStringStringMap, phDataset as handle ):
        super ().
        DataSourceMap = poDataSourceMap.
        TTtoDBMap = pTtToDbmap.
        TTtoDBRenameMap = pTtToDbRenameMap.
        this-object:Dataset = phDataset. 
        MainDataSource = pMainDataSource.
    end constructor.
    
    method protected void RenameTableQualifier (pQueryentry as Ccs.BusinessLogic.IQueryEntry):
        QueryRenameUtil:Traverse(pQueryEntry).
    end method.    
    
    method protected void RenameTableQualifier (pQuerySortEntries as Ccs.BusinessLogic.IQuerySortEntry extent):
        QueryRenameUtil:Traverse(pQuerySortEntries).
    end method. 
    
    method public Ccs.BusinessLogic.IQueryEntry ColumnExpression( input pPredicate as Ccs.BusinessLogic.IQueryPredicate ):
        define variable iSource as integer no-undo.
        define variable hBuffer as handle no-undo.
        define variable oDataSource as IDataSource no-undo.
        define variable cQualifierForSource as character no-undo.
        define variable cTable as character no-undo.
        define variable cField  as character no-undo.
        define variable oSourceEntry as Ccs.BusinessLogic.IQueryEntry no-undo.
         
        // DBFieldsourcemap is a paired list of column and table built in columnsource to keep track of fields oin ionnerjointables   
        iSource = lookup(pPredicate:FieldName,DBFieldSourceMap). 
        
        if iSource > 0 then 
        do on error undo, throw:
            hBuffer = this-object:Dataset:get-buffer-handle(entry(iSource + 1,DBFieldSourceMap)).  
            oDataSource = DataSourceMap:Get(hBuffer).
            cTable = entry(1,pPredicate:FieldName,".").  
            
            cQualifierforsource = StringUtil:GetMappedName(cTable, DbFieldRenameMap).
            if cQualifierForSource > "" then
            do:
                cField = entry(2,pPredicate:FieldName,".").
                cast(pPredicate,QueryPredicate):FieldName = subst("&1.&2",cQualifierForSource,cField). 
                oSourceEntry = oDataSource:ColumnExpression(pPredicate).  
                RenameTableQualifier(oSourceEntry).
            end.
            else
                oSourceEntry = oDataSource:ColumnExpression(pPredicate). 
           
            return oSourceEntry.    
            catch e as Progress.Lang.Error :
                
               if session:remote then 
               do: 
                   // as of current the known use cases works with no conversion, so just add back the original fieldname 
                   // and log the error 
                   cast(pPredicate,QueryPredicate):FieldName = subst("&1.&2",cTable,cField). 
                   message subst("Failed to resolve predicate &1",pPredicate:toString()).
               end.    
               else 
                   undo, throw new ApplicationError(subst("Failed to resolve predicate &1",pPredicate:toString()),e).
                     
            end catch. 
        end.     
        else 
            return MainDataSource:ColumnExpression(pPredicate).
                
    end method.

    method public Ccs.BusinessLogic.IQuerySortEntry extent ColumnSort( input pSort as Ccs.BusinessLogic.IQuerySortEntry ):
        define variable iSource as integer no-undo.
        define variable hBuffer as handle no-undo.
        define variable oDataSource as IDataSource no-undo.
        define variable cQualifierForSource as character no-undo.
        define variable cTable as character no-undo.
        define variable cField  as character no-undo.
        define variable cSingleEntry as Ccs.BusinessLogic.IQuerySortEntry extent 1 no-undo.
        define variable oSourceEntries as Ccs.BusinessLogic.IQuerySortEntry extent no-undo.
        
         
        // DBFieldsourcemap is a paired list of column and table built in columnsource to keep track of fields oin ionnerjointables   
        iSource = lookup(pSort:FieldName,DBFieldSourceMap). 
        
        if iSource > 0 then 
        do on error undo, throw:
            hBuffer = this-object:Dataset:get-buffer-handle(entry(iSource + 1,DBFieldSourceMap)).  
            oDataSource = DataSourceMap:Get(hBuffer).
            cTable = entry(1,pSort:FieldName,".").  
            cQualifierforsource = StringUtil:GetMappedName(cTable, DbFieldRenameMap).
            if cQualifierForSource > "" then
            do:
                cField = entry(2,pSort:FieldName,".").
                cast(pSort,QuerySortEntry):FieldName = subst("&1.&2",cQualifierForSource,cField). 
                oSourceEntries = oDataSource:ColumnSort(pSort).  
                RenameTableQualifier(oSourceEntries).
            end.
            else
                oSourceEntries = oDataSource:ColumnSort(pSort). 
            
            return oSourceEntries.
            catch e as Progress.Lang.Error :
                // add bac k the 
               if session:remote then 
               do: 
                   // as of current the known use cases works with no conversion, so just add back the origal data 
                   // and log the error 
                   cast(pSort,QuerySortEntry):FieldName = subst("&1.&2",cTable,cField). 
                   message subst("Failed to resolve predicate &1",pSort:toString()).
               end.    
               else 
                   undo, throw new ApplicationError(subst("Failed to resolve predicate &1",pSort:toString()),e).
                     
            end catch. 
        end.     
        else do: 
            return MainDataSource:ColumnSort(pSort). 
        end.    
      
        return cSingleEntry.     
    end method.

    method public character ColumnSource( input pcColumn as character ):
        define variable oDataSource   as IDataSource no-undo.
        define variable hBuffer       as handle no-undo.
        define variable cColumn       as character no-undo.
        define variable cBufferName   as character no-undo.
        define variable cTTBufferName as character no-undo.
        define variable cReturnedBufferName as character no-undo.
        define variable cRenames      as character no-undo. 
        define variable cRename       as character no-undo.
       
        if num-entries(pcColumn,".") > 1 then  
        do:
            hBuffer = this-object:Dataset:get-buffer-handle(entry(1,pcColumn,".")).
            oDataSource = DataSourceMap:Get(hBuffer).      
            cTTBufferName = entry(1,pcColumn,".").       
            cBufferName = TTtoDbMap:Get(cTTBufferName).
            cColumn = oDataSource:ColumnSource(entry(2,pcColumn,".")).
            if valid-object(TTtoDBRenameMap) then 
            do:
                cRenames = TTtoDBRenameMap:Get(cTTBufferName). 
                if cRenames > "" then 
                do:
                    cReturnedBufferName = entry(1,cColumn,".").
                    cRename = StringUtil:GetUseName(cReturnedBufferName, cRenames).
                    if cRename <> cReturnedBufferName then 
                    do:
                       entry(1,cColumn,".") = cRename.
                       if lookup(cRename,DbFieldRenameMap) = 0 then
                       do:
                          DbFieldRenameMap = DbFieldRenameMap 
                                 + (if DbFieldRenameMap = "" then "" else ",")
                                 + cRename + "," + cReturnedBufferName.
                          QueryRenameUtil:TableRenames = DbFieldRenameMap.
                       end.               
                    end.    
                end.     
            end.
            if lookup(cColumn,DBfieldSourceMap) = 0 then
            do:
                DBFieldSourceMap = DbFieldSourceMap 
                                 + (if DbFieldSourceMap = "" then "" else ",")
                                 + cColumn + "," + entry(1,pcColumn,".").
            end.                        
            
        end.
        else do:    
            cColumn = MainDataSource:ColumnSource(pcColumn).
        end.    
        
        return cColumn.
    end method.

end class.