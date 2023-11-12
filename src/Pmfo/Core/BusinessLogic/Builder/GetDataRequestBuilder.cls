 
 /*------------------------------------------------------------------------
    File        : GetDataRequestBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Oct 31 08:58:03 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
//using Pmfo.Core.BusinessLogic.Builder.AbstractBuilder from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.Builder.GetDataRequestBuilder from propath.
 
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Util.QueryUtil from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.Builder.GetDataRequestBuilder  : 
    
    define public property Request as IGetDataRequest no-undo get. protected set.
    define variable TableRequests as IGetDataTableRequest extent no-undo.
    define protected variable Tables as character no-undo.
    define private variable nothing as logical no-undo.
        /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public GetDataRequestBuilder (  ):
        super ().
        Request = new GetDataRequest(). 
    end constructor.
    
    method public GetDataRequestBuilder Find(pcWhere as character):
        return AddTableRequest(pcWhere, true).
    end method. 
    
    method public GetDataRequestBuilder Each(pcWhere as character):
        return AddTableRequest(pcWhere, false).
    end method.
    
    method protected GetDataRequestBuilder AddTableRequest(pcWhere as character,plUnique as logical):
        define variable cTable          as character no-undo.
        define variable iWherePos       as integer no-undo.
        define variable lHasWhereClause as logical no-undo.
        define variable iEndWhereClause as integer no-undo.
        define variable i               as integer no-undo.
        define variable cWhere          as character no-undo.
        define variable oTableRequests  as IGetDataTableRequest extent no-undo.
        define variable iExtents        as integer no-undo.
        cTable = entry(1,left-trim(pcwhere)," ").
        
        iWherepos = QueryUtil:GetInsertPosition(pcWhere, output lHasWhereClause, output iEndWhereClause). 
        
        cWhere = substring(pcWhere,iWherePos).        
        
        iExtents = extent(TableRequests) + 1.
        extent(oTableRequests) = iExtents.
        
        do i = 1 to extent(TableRequests):
            oTableRequests[i] = TableRequests[i].
        end.        
        
        oTableRequests[iExtents] = new GetDataTableRequest(cTable).
        if plUnique then 
            oTableRequests[iExtents]:FindUnique = true.
        return this-object. 
        
    end method.
    
    
    method public GetDataRequestBuilder Build(): 
         
        return this-object. 
    end method.    
    
    method public GetDataRequestBuilder And(pcExpression as character):
         
        return this-object. 
    end method.    
    
    method public GetDataRequestBuilder And(pcfield as character,pOperator as QueryOperatorEnum, pcValue as character): 
         
        return this-object. 
    end method.    
    
    method public GetDataRequestBuilder Or(): 
        
        return this-object. 
    end method.    
           
end class.


