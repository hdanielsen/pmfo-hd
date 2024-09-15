 
 /*------------------------------------------------------------------------
    File        : GetDataTableRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 03 23:36:29 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.INamedQuery from propath.
using Ccs.BusinessLogic.IQueryDefinition from propath.
using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.QueryDefinition from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.StringUtil from propath.
using Ccs.Common.Support.IPrimitiveHolder from propath.
using Pmfo.Util.Array from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.

class Pmfo.Core.BusinessLogic.GetDataTableRequest implements IGetDataTableRequest: 
    /* Returns the number of records requested by the caller of the Business Entity getData method */
    define public property NumRecords as int64 no-undo get. set.
     
    /* Returns   paging context */
    define public property PagingContextEntry as IQueryEntry no-undo get. set. 
   
    /* Returns the paging context for compatibility  */
    define public property PagingContext           as character no-undo 
        get(): 
            if valid-object(PagingContextEntry) then
                return PagingContextEntry:ToString().
            else 
                return "".    
        end.
    
    define public property NamedQuery   as INamedQuery no-undo get. set.     
   
    /* Returns the abstract query defintion for this request */
    define public property QueryDefinition as IQueryDefinition no-undo get. set. 
         
    /* Returns the number of records to skip */
    define public property Skip as int64 no-undo get. set. 
    
    /* Returns the name of the ProDataset Table */
    define public property TableName as character no-undo 
        get.
        private set. 
    
    define public property ClientOnly     as logical no-undo  get. set.
    define public property FindUnique     as logical no-undo  get. set.
    define public property Count          as logical no-undo  get. set.
    define public property PageBackward   as logical no-undo  get. set.
    define public property ExternalPaging as logical no-undo  get. set.
    
    // set to true by DataRequestBuilder so the server can identify where to support innerjoin  
    define public property IsMainTable    as logical no-undo  get. set.
    
    // requested expand internal table names 
    define public property ClientExpands       as character   no-undo  get. set.
         
    // derrived from expands with option innerjoin=true - stored with table name
    define public property InnerJoinExpands as character no-undo  get. set.
    
    // derrived from apply with qualified aggregate options  
    define public property InnerJoinAggregates as character   no-undo  get. set. 
    //  see interface
    define public property ManualServerInnerJoins as character   no-undo  get. set.
    
      // performance option - see comment in interfave
    define public property SelectFields   as character no-undo get. set. 
    /* Returns the Query String for this table */
    define public property QueryString as character no-undo 
         get():
             define variable cQuery as character no-undo.
        if FindUnique then
            cQuery = subst("find &1",TableName).
        else    
            cQuery = subst("for each &1",TableName).
        
        if valid-object(QueryDefinition) then 
            cQuery = cQuery + " " + QueryDefinition:ToString().
        return cQuery. 
    end.     
    
    // convenience to check if paging
    define public property IsPaging as logical 
         get(): 
             return NumRecords > 0 or this-object:Skip > 0 or valid-object(PagingContextEntry). 
         end. 
         
    define public property SortExpression     as character   no-undo  
         get():
             define variable i as integer no-undo.
             define variable cExpression as character no-undo.
             if valid-object(QueryDefinition) then
             do:
                 do i = 1 to extent(QueryDefinition:QuerySort):
                     cExpression = cExpression 
                        + (if cExpression = "" then "" else " ")
                        + QueryDefinition:QuerySort[i]:ToString().
                 end.
                 return cExpression.
             end.
             return "".    
         end.  
       
    constructor public GetDataTableRequest ( input pTableName as character ):
        super ().
        Assert:NotNull(pTableName, 'Table name').
        assign this-object:TableName = pTableName.        
    end constructor.
    
    constructor public GetDataTableRequest ( input phHandle as handle ):
        super ().
        Assert:NotNull(phHandle, 'Table or buffer handle').
        case phHandle:type:
            when "buffer" then 
                assign this-object:TableName = phHandle:table-handle:name.        
            when "temp-table" then 
                assign this-object:TableName = phHandle:name.        
            otherwise
                undo, throw new IllegalArgumentError(substitute("Handle is of type &1",phHandle:type)).              
        end case.
    end constructor.
    
    method public logical AddQueryEntries(pQueryEntries as IQueryEntry extent):
        define variable i      as integer no-undo.
        define variable lOk    as logical no-undo.
        define variable lAdded as logical no-undo.
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        
        lok = true.
        
        oDef = GetAndAssertQueryDefinition().
        if not valid-object(oDef) then
        do: 
            oDef = new QueryDefinition().
            QueryDefinition = oDef.
        end.    
        do i = 1 to extent(pQueryEntries):
            lAdded = oDef:AddEntry(pQueryEntries[i]).
            if lAdded = false then
                lOk = false.
        end.    
        return lok.
    end method.    
    
    method public logical AddQueryEntry(pQueryEntry as IQueryEntry):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if not valid-object(oDef) then
        do: 
            oDef = new QueryDefinition().
            QueryDefinition = oDef.
        end.  
        return oDef:AddEntry(pQueryEntry).
                     
    end method.
    
    /******** no join single value ***/
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pcValue as character).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pcValue)).
    end method.
    
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,piValue as int64).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,piValue)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,plValue as logical).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,plValue)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValue as decimal).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pdValue)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as date).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pValue)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as datetime).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pValue)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as datetime-tz).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pValue)).
    end method.
     
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as IPrimitiveHolder).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pValue:ToString())).
    end method.
    
    /******** no join extent  ***/
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pcValues as character extent).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pcValues)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,piValues as int64 extent).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,piValues)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,plValues as logical extent).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,plValues)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pdValues as decimal extent).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pdValues)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValues as date extent).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pValues)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValues as datetime extent).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pValues)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValues as datetime-tz extent).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,pValues)).
    end method.
    
    method public logical AddQueryEntry(pcField as character,pOperator as QueryOperatorEnum,pValue as IPrimitiveArrayHolder).
        return AddQueryEntry(new QueryPredicate(pcfield,pOperator,Array:ToStringArray(pValue))).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pcValue as character).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pcValue)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,piValue as int64).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,piValue)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,plValue as logical).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,plValue)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValue as decimal).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pdValue)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValue as date).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pValue)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValue as datetime).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pValue)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValue as datetime-tz).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pValue)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValue as IPrimitiveHolder).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pValue:ToString())).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pcValues as character extent).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pcValues)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,piValues as int64 extent).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,piValues)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,plValues as logical extent).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,plValues)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pdValues as decimal extent).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pdValues)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValues as date extent).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pValues)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValues as datetime extent).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pValues)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValues as datetime-tz extent).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,pValues)).
    end method.
    
    method public logical AddQueryEntry(pJoin as JoinEnum,pcField as character,pOperator as QueryOperatorEnum,pValue as IPrimitiveArrayHolder).
        return AddQueryEntry(new QueryPredicate(pJoin,pcfield,pOperator,Array:ToStringArray(pValue))).
    end method.
 
        
    method public logical RemoveQueryEntries(pQueryEntries as IQueryEntry extent):
        define variable i        as integer no-undo.
        define variable lOk      as logical no-undo.
        define variable lRemoved as logical no-undo.
        define variable oDef     as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        
        lok = true.
        
        oDef = GetAndAssertQueryDefinition().
        
        do i = 1 to extent(pQueryEntries):
            if valid-object(oDef) then 
                lRemoved = oDef:RemoveEntry(pQueryEntries[i]).
            else do:
                return false.
            end.   
            if lRemoved = false then
                lOk = false.
        end.    
        return lok.
    end method.
     
    method public logical HasQueryEntry(pQueryEntry as IQueryEntry):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        define variable lOk as logical no-undo.
        
        if valid-object(pQueryEntry) then
        do:
            oDef = GetAndAssertQueryDefinition().
            if valid-object(oDef) and valid-object(oDef:QuerySelection) then 
            do:
                if pQueryEntry:Equals(oDef:QuerySelection) then
                    return true.
                if type-of(oDef:QuerySelection,Pmfo.Core.BusinessLogic.IQueryGroup) then 
                    return cast(oDef:QuerySelection,Pmfo.Core.BusinessLogic.IQueryGroup):HasEntry(pQueryEntry).
            end.
        end. 
        return false.    
    end method. 
     
    method public logical RemoveQueryEntry(pQueryEntry as IQueryEntry):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        define variable lOk as logical no-undo.
        oDef = GetAndAssertQueryDefinition().
        
        if valid-object(oDef) then 
        do:
            lok = oDef:RemoveEntry(pQueryEntry).
            if valid-object(oDef:QuerySelection) = false and extent(oDef:QuerySort) = ? then
                QueryDefinition = ?.
            return lok.    
        end.    
        else
            return false.    
    end method.
       
    method public logical SetQuerySort(pSort as IQuerySortEntry):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do: 
            oDef:SetSort(pSort).
        end.    
        else 
           QueryDefinition = new QueryDefinition(pSort).
        return true.    
    end method.
    
    method public IQuerySortentry extent RemoveQuerySort():
        define variable oDef  as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        define variable oSort as IQuerySortEntry extent no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            oSort = oDef:RemoveSort().
            if valid-object(oDef:QuerySelection) = false and extent(oDef:QuerySort) = ? then
                QueryDefinition = ?.
       
        end.
        return oSort.   
    end method.
    
    method public character GetFieldReferences().
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetFieldReferences().
        end.
        else 
            return "".  
    end method.
    
    // Returns union of ExpandInnerJoins and QueryDefinition:GetQualifiedTableReferences
    // output pcServerInnerJoins are references ONLY found in query (no fill on server for performance)   
    method public character GetInnerJoins().
       define variable cDummy as character no-undo.
       return GetInnerJoins(output cDummy). 
    end method.
      
    // Return union of InnerJoinExpands, InnerJoinAggregates and QueryDefinition:GetQualifiedTableReferences
    method public character GetInnerJoins(output pcServerInnerjoins as character).
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        define variable cInnerJoins as character no-undo.
        define variable cQualInnerJoins as character no-undo.
        define variable i as integer no-undo.
        define variable cTable as character no-undo.
        
        oDef = GetAndAssertQueryDefinition().
        cInnerJoins = InnerJoinExpands.
         
        if valid-object(oDef) then   
        do:
            // DataSource:prepareInnerJoins will order these from parent to child 
            cQualInnerJoins = oDef:GetQualifiedTableReferences().
            do i = 1 to num-entries(cQualInnerJoins):
                cTable = entry(i,cQualInnerJoins).
                if lookup(cTable,cInnerJoins) = 0 then 
                do: 
                    cInnerJoins = StringUtil:AppendEntry(cInnerJoins, cTable).
                    // if the table only is referenced in a filter and not in the client expand then
                    // this is a server only join
                    // This is extremely important for performance when filtering on values in one to many     
                    if lookup(cTable,ClientExpands) = 0 then
                    do: 
                        pcServerInnerjoins = StringUtil:AppendEntry(pcServerInnerJoins, cTable).
                    end. 
                end.        
            end.     
        end.
        
        // property for complex cases  (for example a parent of a qualified grandchild field )
        // assumed to not need to set innerJoins
        if ManualServerInnerJoins > "" then 
        do i = 1 to num-entries(ManualServerInnerJoins): 
            cTable = entry(i,ManualServerInnerJoins). 
            if lookup(cTable,cInnerJoins) = 0 then
                cInnerJoins = StringUtil:AppendEntry(cInnerJoins, cTable). 
            if lookup(cTable,ClientExpands) = 0 and lookup(cTable,pcServerInnerjoins) = 0 then
                pcServerInnerjoins = StringUtil:AppendEntry(pcServerInnerjoins, cTable).           
        end.
        
        // table references only in aggregates must be added to the inner join  
        if InnerJoinAggregates > "" then 
        do i = 1 to num-entries(InnerJoinAggregates): 
            cTable = entry(i,InnerJoinAggregates). 
            if lookup(cTable,cInnerJoins) = 0 then
                StringUtil:AppendEntry(cInnerJoins, cTable).           
        end.
        
        return cInnerJoins.     
             
    end method.

     // returns a comma spearted list of all fields references in sort  
    method public character GetSortFieldReferences():
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetSortFieldReferences().
        end.
        else 
            return "".
    end method.
    
    // returns a comma separated list of all table qualifers referenced in  predicates and sort  
    method public character GetQualifiedTableReferences():
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetQualifiedTableReferences().
        end.
        else 
            return "".
    end method.
    
    // returns a comma separated list of all table qualifers referenced in predicated  
    method public character GetQualifiedPredicateTableReferences():
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetQualifiedPredicateTableReferences().
        end.
        else 
            return "".
    end method.
    
    // returns a comma separated list of all table qualifers referenced in sort  
    method public character GetQualifiedSortTableReferences():
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetQualifiedSortTableReferences().
        end.
        else 
            return "".
    end method.
    
        
        // returns a comma separated list of all fields references  
    method public character GetPredicateFieldReferences():    
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetPredicateFieldReferences().
        end.
        else 
            return "".
    end method.
    
    method public character GetPredicateFieldReferences(pOperator as QueryOperatorEnum).  
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetPredicateFieldReferences(pOperator).
        end.
        else 
            return "".
    end method.  
    
    method public character GetPredicateFieldReferences(pOperators as QueryOperatorEnum extent).
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetPredicateFieldReferences(pOperators).
        end.
        else 
            return "".
    end method.
    
    method public character GetPredicateFieldEqualMatches().  
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetPredicateFieldEqualMatches().
        end.
        else 
            return "".
    end method.
    
    method public character GetPredicateFieldRangeMatches().  
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetPredicateFieldRangeMatches().
        end.
        else 
            return "".
    end method.
    
    
    method public IQueryEntry extent GetQueryEntries(pcField as character):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        define variable oDummy as IQueryEntry extent no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetEntries(pcField).
        end.
        else 
            return oDummy.    
    end method.
   
    method public IQueryEntry extent GetQueryEntries(pcFields as character extent):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        define variable oDummy as IQueryEntry extent no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetEntries(pcFields).
        end.
        else 
            return oDummy.    
    end method.
    
    method public IQueryEntry extent GetQueryEntries(pcField as character, pOperator as QueryOperatorEnum):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        define variable oDummy as IQueryEntry extent no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetEntries(pcField,poperator).
        end.
        else 
            return oDummy.    
    end method.
    
    method public IQueryEntry extent GetQueryEntries(pcFields as character extent, pOperator as QueryOperatorEnum):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        define variable oDummy as IQueryEntry extent no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetEntries(pcFields,poperator).
        end.
        else 
            return oDummy.    
    end method.
    
    method public IQueryEntry extent GetQualifiedQueryEntries(pcQualifier as character):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        define variable oDummy as IQueryEntry extent no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return oDef:GetQualifiedEntries(pcQualifier).
        end.
        else 
            return oDummy.    
    end method.
    
    method public logical HasPredicate(pcField as character).
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
            return oDef:HasPredicate(pcField).
        else 
            return false.
    end method.
    
    method public logical HasPredicate(pcFields as character extent).
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
            return oDef:HasPredicate(pcFields).
        else 
            return false.
    end method.
    
    method public logical HasPredicate(pcField as character, pOperator as QueryOperatorEnum).
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
            return oDef:HasPredicate(pcField,pOperator).
        else 
            return false.
    end method.
    
    method public logical HasPredicate(pcFields as character extent, pOperator as QueryOperatorEnum).
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
            return oDef:HasPredicate(pcFields,pOperator).
        else 
            return false.
    end method.
    
    method public logical HasQuery(pcField as character).
        define variable oQuery as IQueryEntry extent no-undo.
        oQuery = GetQueryEntries(pcField).
        return extent(oQuery) <> ?. 
    end method.
    
    method public logical HasSort():
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            return extent(oDef:QuerySort) <> ?.
        end.    
        return false.   
    end method.
    
    method public logical HasSort(pcField as character).
        define variable oSort as IQuerySortEntry extent no-undo.
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            oSort = oDef:GetSortEntries(pcField).
        end.    
        return extent(oSort) <> ?. 
    end method.
     
    method public logical HasSort(pcFields as character extent).
        define variable oSort as IQuerySortEntry extent no-undo.
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do:
            oSort = oDef:GetSortEntries(pcFields).
        end.    
        return extent(oSort) <> ?. 
    end method. 
     
    method public IQueryPredicate GetQueryPredicate(pcField as character, pOperator as QueryOperatorEnum):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
            return oDef:GetPredicate(pcField,poperator).
            
        else 
            return ?.    
    end method.
    
    method public IQueryPredicate GetQueryPredicate(pcField as character):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
            return oDef:GetPredicate(pcField).
        else 
            return ?.    
    end method.
    
    method protected Pmfo.Core.BusinessLogic.IQueryDefinition GetAndAssertQueryDefinition():
        if valid-object(QueryDefinition) then 
        do:
            if type-of(QueryDefinition,Pmfo.Core.BusinessLogic.IQueryDefinition) then
                return cast(QueryDefinition,Pmfo.Core.BusinessLogic.IQueryDefinition).
            else 
               undo, throw new UnsupportedOperationError(subst("Class &1 cannot handle operation. Use extended subclass",QueryDefinition:GetClass():TypeName)). 
        end.
        else 
            return ?. 
    end method.
  
    
    method public logical SetQuerySort(pSort as IQuerySortEntry extent):
        define variable oDef as Pmfo.Core.BusinessLogic.IQueryDefinition no-undo.
        oDef = GetAndAssertQueryDefinition().
        if valid-object(oDef) then 
        do: 
            oDef:SetSort(pSort).
        end.    
        else 
            QueryDefinition = new QueryDefinition(pSort). 
        return true.
    end method.
    
    method override public character ToString():
        return substitute('&1: query:&2; numrec:&3; skip:&4 cntxt:&5 count:&6',
                this-object:TableName,
                this-object:QueryString,
                this-object:NumRecords,
                this-object:Skip,
                this-object:PagingContext,
                this-object:Count
                ).
    end method.
end class.