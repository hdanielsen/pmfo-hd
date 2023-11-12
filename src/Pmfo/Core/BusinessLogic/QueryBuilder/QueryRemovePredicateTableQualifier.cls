 
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

class Pmfo.Core.BusinessLogic.QueryBuilder.QueryRemovePredicateTableQualifier inherits QueryWalker: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define protected property Qualifier as character no-undo get. protected set.
    
    constructor public QueryRemovePredicateTableQualifier (pcQualifier as character):  
         Qualifier = pcQualifier.
    end constructor.
    
    method override public void Traverse( input pTableRequest as IGetDataTableRequest):
        super:Traverse(pTableRequest). 
    end method.
    
    method override public void Traverse( input pPredicate as IQueryPredicate ):
        define variable lComplex as logical no-undo.
        define variable cNewFieldname as character no-undo.
         
        lcomplex =  index(pPredicate:fieldname,StringConstant:SPACE) > 0 
                 or index(pPredicate:fieldname,"(") > 0.
                 
        if lcomplex then 
        do:
            cNewFieldname = pPredicate:fieldname.
            QueryUtil:ReplaceTableName(cNewFieldName, Qualifier,"").        
            cast(pPredicate,QueryPredicate):FieldName = cNewFieldname.
        end.     
        else do:
            if num-entries(pPredicate:FieldName,".") > 1 and entry(1,pPredicate:FieldName,".") = Qualifier then
                cast(pPredicate,QueryPredicate):FieldName = entry(2,pPredicate:FieldName,".").
        end.                      
    end method.
    
    method override public void Traverse( pSort as IQuerySortEntry ):
         // nothing    
    end method.
 
        
end class.