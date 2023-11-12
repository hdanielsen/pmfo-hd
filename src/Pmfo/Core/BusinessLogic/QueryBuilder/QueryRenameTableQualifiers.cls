 
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

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryWalker from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.BusinessLogic.QuerySortEntry from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QueryRenameTableQualifiers inherits QueryWalker: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define protected property TableRenames as character no-undo get. protected set.
    
    constructor public QueryRenameTableQualifiers (pcTableRenames as character, plRenameisFirstEntry as logical):  
        define variable cSwitchedlist as character no-undo.
        define variable i as integer no-undo.
        define variable cDlm as character no-undo.
       
        super ().
        
        if plRenameisFirstEntry then 
        do:
            do i = 1 to num-entries(pcTableRenames) by 2:
                cSwitchedList = subst("&1&2&3,&4",cSwitchedList,cDlm,entry(i + 1,pcTableRenames),entry(i,pcTableRenames)).
                cDlm = ",".
            end.     
            TableRenames = cSwitchedList.
        end.
        else 
            TableRenames = pcTableRenames.
    end constructor.
    
    constructor public QueryRenameTableQualifiers (pcTableRenames as character):  
        super ().
        TableRenames = pcTableRenames.
    end constructor.
    
    method override public void Traverse( input pTableRequest as IGetDataTableRequest):
        super:Traverse(pTableRequest). 
    end method.
    
    method override public void Traverse( input pPredicate as IQueryPredicate ):
        define variable i as integer no-undo.
        define variable lComplex as logical no-undo.
        define variable cNewFieldname as character no-undo.
        define variable cQualifier as character no-undo.
        define variable cUseQualifier as character no-undo.
        
        lcomplex =  index(pPredicate:fieldname,StringConstant:SPACE) > 0 
                 or index(pPredicate:fieldname,"(") > 0.
                 
        if lcomplex then 
        do:
            cNewFieldname = pPredicate:fieldname.
            do i = 1 to num-entries(TableRenames) by 2:
                cNewFieldname = QueryUtil:ReplaceTableName(cNewFieldName, entry(i,TableRenames), entry(i + 1,TableRenames)).        
            end.
            cast(pPredicate,QueryPredicate):FieldName = cNewFieldname.
        end.     
        else do:
            cQualifier = entry(1,pPredicate:Fieldname,".").
            cUseQualifier = StringUtil:GetMappedName(cQualifier, TableRenames).
            if cUseQualifier > "" then
               cast(pPredicate,QueryPredicate):FieldName = subst("&1.&2",cUseQualifier,entry(2,pPredicate:Fieldname,".")).
        end.                      
    end method.
    
    method override public void Traverse( pSort as IQuerySortEntry ):
        define variable i as integer no-undo.
        define variable lComplex as logical no-undo.
        define variable cNewFieldname as character no-undo.
        define variable cQualifier as character no-undo.
        define variable cUseQualifier as character no-undo.
        lcomplex =  index(pSort:fieldname,StringConstant:SPACE) > 0 
                 or index(pSort:fieldname,"(") > 0.
                 
        if lcomplex then 
        do:
            cNewFieldname = pSort:fieldname.
            do i = 1 to num-entries(TableRenames) by 2:
                cNewFieldname = QueryUtil:ReplaceTableName(cNewFieldName, entry(i,TableRenames), entry(i + 1,TableRenames)).        
            end.
            cast(pSort,QuerySortEntry):FieldName = cNewFieldname.
        end.     
        else do:
            cQualifier = entry(1,pSort:Fieldname,".").
            cUseQualifier = StringUtil:GetMappedName(cQualifier, TableRenames).
            if cUseQualifier > "" then
               cast(pSort,QuerySortEntry):FieldName = subst("&1.&2",cUseQualifier,entry(2,pSort:Fieldname,".")).
        end.                      
    end method.
 
        
end class.