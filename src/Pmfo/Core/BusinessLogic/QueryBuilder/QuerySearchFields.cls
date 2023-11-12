 
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
using Ccs.BusinessLogic.QueryOperatorEnum from propath.

class Pmfo.Core.BusinessLogic.QueryBuilder.QuerySearchFields inherits QueryWalker: 
    
   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define public property FieldNames as character no-undo get. protected set.
    define public property TableName as character no-undo get. protected set.
    define protected property Operators as QueryOperatorEnum no-undo extent get. set.
    
    constructor public QuerySearchFields (pcTableName as character):  
        super ().
        TableName = pcTableName.
    end constructor.
    
    constructor public QuerySearchFields (pOperator as QueryOperatorEnum):
        super ().
        extent(Operators) = 1.
        Operators[1] = pOperator.
    end method.
    
    constructor public QuerySearchFields (pOperators as QueryOperatorEnum extent):
        super ().
        Operators = pOperators.
    end method.
    
    constructor public QuerySearchFields ():
        super ().
    end method.
     
    method override public void Traverse( input pTableRequest as IGetDataTableRequest):
        if TableName = "" then
            undo, throw  new IllegalOperationError("Field Search in IGetDataTableRequest without TableName").
        if pTableRequest:TableName = TableName then
            super:Traverse(pTableRequest). 
    end method.
    
    method override public void Traverse( input pPredicate as IQueryPredicate ):
        define variable i as integer no-undo.
        define variable lOk as logical no-undo.
        if lookup(pPredicate:FieldName,FieldNames) = 0 then 
        do:
           if extent(Operators) > 0 then
           do: 
               do i = 1 to extent(Operators):
                   if pPredicate:Operator = Operators[i] then 
                   do:
                       lok = true.
                       leave.
                   end.    
               end.
           end.
           else 
               lok = true.
           if lok then        
               FieldNames = FieldNames 
                          + (if fieldNames = "" then "" else ",")
                          + pPredicate:FieldName.
        end.                      
    end method.
    
    method override public void Traverse( pSort as IQuerySortEntry ):
        if lookup(pSort:FieldName,FieldNames) = 0 then 
           FieldNames = FieldNames 
                      + (if fieldNames = "" then "" else ",")
                      + pSort:FieldName.        
    end method.
        
end class.