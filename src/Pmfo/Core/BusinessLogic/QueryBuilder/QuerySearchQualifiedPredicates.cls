 
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

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedPredicates inherits QueryWalker: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define protected property Qualifier as character   no-undo get. set.
    define public property QueryEntryCollection as IList no-undo get. set.
      
    define private variable SearchFound        as logical no-undo.
    define private variable SearchGroup        as logical no-undo.
    
    
    constructor public QuerySearchQualifiedPredicates (pcQualifier as character):
        define variable cField as character extent 1 no-undo.
        super ().
        Qualifier = pcQualifier.
        // use list to preserve order 
        QueryEntryCollection = new List().
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
        
        if SearchGroup then 
        do: 
            if not SearchFound then 
            do:
                if num-entries(pPredicate:FieldName,".") > 1 and entry(1,pPredicate:FieldName,".") = Qualifier then
                do:  
                    SearchFound = true.
                end.        
            end.
        end.
        else if num-entries(pPredicate:FieldName,".") > 1 and entry(1,pPredicate:FieldName,".") = Qualifier then
        do:
            QueryEntryCollection:Add(pPredicate).
        end.
               
         
    end method.
    
    method override public void Traverse( input pQueryGroup as IQueryGroup ):
        define variable lTopOfOrGroup as logical no-undo.
        
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
       
    end method.
    
        
end class.