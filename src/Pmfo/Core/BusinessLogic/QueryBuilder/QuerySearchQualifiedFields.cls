 
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
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryWalker from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchFields from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchQualifiedFields inherits QuerySearchFields: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define public property TableReferences as character no-undo get. protected set.
    
    constructor public QuerySearchQualifiedFields (pcTableName as character):  
        super ().
        TableName = pcTableName.
    end constructor.
    
    constructor public QuerySearchQualifiedFields ():
        super ().
    end method.
     
    method override public void Traverse( input pTableRequest as IGetDataTableRequest):
        if TableName = "" then
            undo, throw  new IllegalOperationError("Field Search in IGetDataTableRequest without TableName").
        if pTableRequest:TableName = TableName then
            super:Traverse(pTableRequest). 
    end method.
    
    method override public void Traverse( input pPredicate as IQueryPredicate ):
        define variable cTable as character no-undo.
        if num-entries(pPredicate:FieldName,".") > 1 then
        do:
            super:Traverse(pPredicate).
            cTable = entry(1,pPredicate:FieldName,".").
            if lookup(cTable,TableReferences) = 0 then
                 TableReferences = TableReferences 
                                 + (if TableReferences = "" then "" else ",")
                                 + cTable.
        end.                      
    end method.
    
    method override public void Traverse( pSort as IQuerySortEntry ):
        define variable cTable as character no-undo.
        if num-entries(pSort:FieldName,".") > 1 then
        do:
            super:Traverse(pSort).
            cTable = entry(1,pSort:FieldName,".").
            if lookup(cTable,TableReferences) = 0 then
                 TableReferences = TableReferences 
                                 + (if TableReferences = "" then "" else ",")
                                 + cTable.
        end.                      
    end method.
        
end class.