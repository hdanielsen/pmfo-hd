 
 /*------------------------------------------------------------------------
    File        : QuerySearchPredicateFields
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jun 18 19:45:27 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchFields from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchPredicateFields inherits QuerySearchFields: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define variable CheckCanUseindex as logical no-undo.
    
    constructor public QuerySearchPredicateFields (pcTableName as character):
        super (pctablename).
    end constructor.
    
    constructor public QuerySearchPredicateFields ():
        super ().
    end method.
    
    constructor public QuerySearchPredicateFields ( input pOperator as QueryOperatorEnum ):
        super (input pOperator).
    end constructor.
    
    constructor public QuerySearchPredicateFields ( input plCheckCanUseindex as logical,input pOperator as QueryOperatorEnum ):
        super ( pOperator).
        CheckCanUseindex = plCheckCanUseindex.
    end constructor.
    
    constructor public QuerySearchPredicateFields ( input plCheckCanUseindex as logical,input pOperators as QueryOperatorEnum extent):
        super ( pOperators).
        CheckCanUseindex = plCheckCanUseindex.
    end constructor.
        
    constructor public QuerySearchPredicateFields ( input pOperators as QueryOperatorEnum extent ):
        super (input pOperators).
    end constructor.
     
    method protected logical AnyDifferentOr(pGroup as Ccs.BusinessLogic.IQueryGroup, pcfieldname as char, pOperator as QueryOperatorEnum, ICurrent as integer):
        define variable i      as integer no-undo.
        define variable oEntry as IQueryEntry no-undo.
        define variable cFieldname as character no-undo.
        do i = 1 to extent(pGroup:Entries):
            if i <> iCurrent then
            do:
                oEntry = pGroup:entries[i].
                case oEntry:Join:
                    when JoinEnum:Or or when JoinEnum:OrNot then
                    do: 
                        // TODO Progress can use multiple indexes also with different fields and OR (I think) 
 
                        if type-of(oEntry,IQueryPredicate) = false or cast(oEntry,IQueryPredicate):FieldName <> pcfieldName then 
                            return true.
                        // if it is the same field then return false if operators are not EQ or InList in both   
                        if (pOperator <> QueryOperatorEnum:Eq and pOperator <> QueryOperatorEnum:InList) then
                            return true.
                              
                        if  cast(oEntry,IQueryPredicate):operator <> QueryOperatorEnum:Eq  
                        and cast(oEntry,IQueryPredicate):operator <> QueryOperatorEnum:InList then 
                        do: 
                            return true.  
                        end.
                    end.    
                end.
            end.    
        end.    
        return false.
    end method.    
    
    method public override void Traverse (pQueryGroup as Ccs.BusinessLogic.IQueryGroup):
        define variable i as integer no-undo.
        define variable lHasOr as logical no-undo.
        if CheckCanUseindex then
        do i = 1 to extent(pQueryGroup:Entries):
            if type-of(pQueryGroup:Entries[i],IQueryPredicate) then 
            do:
                if AnyDifferentOr(pQueryGroup, cast(pQueryGroup:Entries[i],IQueryPredicate):fieldname,cast(pQueryGroup:Entries[i],IQueryPredicate):operator, i) then 
                do:
                    lHasOr = true.
                    leave.
                end.    
            end.
           
        end.
        if not lHasOr then 
            super:Traverse(pQueryGroup).    
    end method.
       
    
      method override public void Traverse( pSort as IQuerySortEntry ):
       // do nothing
    end method.
        
end class.