 
 /*------------------------------------------------------------------------
    File        : QuerySearchPredicates
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jun 18 19:45:27 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQueryGroup from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.List from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryWalker from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchPredicates inherits QueryWalker: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define protected property FieldNames as character extent no-undo get. set.
    define protected property Operator as QueryOperatorEnum no-undo get. set.
    define public property QueryEntryCollection as IList no-undo get. set.
      
    define private variable SearchFound        as logical no-undo.
    define private variable SearchGroup        as logical no-undo.
    define private variable FoundOtherOperator as logical no-undo.
    
    constructor public QuerySearchPredicates (pcFieldName as character):
        define variable cField as character extent 1 no-undo.
        super ().
        extent(FieldNames)  = 1.
        FieldNames[1] = pcFieldname.
        // use list to preserve order 
        QueryEntryCollection = new List().
    end constructor.
    
    constructor public QuerySearchPredicates (pcFieldNames as character extent):
        super ().
        FieldNames = pcFieldNames.
        // use list to preserve order 
        QueryEntryCollection = new List().
    end constructor.
    
    constructor public QuerySearchPredicates (pcFieldName as character, pcOperator as QueryOperatorEnum ):
        this-object(pcFieldName).
        Operator             = pcoperator.    
    end constructor.
    
    constructor public QuerySearchPredicates (pcFieldNames as character extent, pcOperator as QueryOperatorEnum ):
        this-object(pcFieldNames).
        Operator             = pcoperator.    
    end constructor.
   
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
    
    method override public void Traverse( input pPredicate as IQueryPredicate ):
        define variable i as integer no-undo.
        if valid-object(Operator) = false or FoundOtherOperator = false then
        do:
            do i = 1 to extent(FieldNames):
                if SearchGroup then 
                do: 
                    if not SearchFound then 
                    do:
                        if pPredicate:FieldName = FieldNames[i] then
                        do: 
                            if not valid-object(Operator) or Operator:Equals(pPredicate:Operator) then  
                                SearchFound = true.
                            else 
                                FoundOtherOperator = true.
                        end.        
                    end.
                end.
                else if pPredicate:FieldName = FieldNames[i] then
                do:
                    if not valid-object(Operator) or Operator:Equals(pPredicate:Operator) then  
                        QueryEntryCollection:Add(pPredicate).
                    else 
                        FoundOtherOperator = true. 
                end.
            end.  
        end.
        if valid-object(Operator) = true and FoundOtherOperator then
            QueryEntryCollection:Clear().    
    end method.
    
    method override public void Traverse( input pQueryGroup as IQueryGroup ):
        define variable lTopOfOrGroup as logical no-undo.
        
        if valid-object(Operator) = false or FoundOtherOperator = false then
        do:
            if not SearchGroup then 
            do:
                if AnyOr(pQueryGroup) then
                do: 
                    SearchGroup = true.
                    SearchFound = false.
                    lTopOfOrGroup = true.
                end.
            end.    
            
            super:Traverse(input pQueryGroup).
                
            if lTopOfOrGroup then
            do:
                if SearchFound then 
                   QueryEntryCollection:Add(pQueryGroup).
               
                SearchGroup = false.
                SearchFound = false. 
            end.
        end.    
        if valid-object(Operator) = true and FoundOtherOperator then
            QueryEntryCollection:Clear().  
    end method.
    
        
end class.