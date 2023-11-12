 
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
using Pmfo.Core.BusinessLogic.Builder.GetDataTableRequestBuilder from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.Parser.BaseFilterParser from propath.
using OpenEdge.BusinessLogic.QueryDefinition from propath.
using Pmfo.Core.BusinessLogic.IQueryDefinition from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Ccs.Common.Application from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
 

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.Builder.GetDataTableRequestBuilder  : 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define public    property TableRequest as IGetDataTableRequest no-undo get. protected set. 
    
    define protected property Where as character no-undo get. set. 
    define protected property Sort  as character no-undo get. set. 
    
    define protected property Parser as BaseFilterParser no-undo 
        get():
            if not valid-object(Parser) then 
                 Parser = new BaseFilterParser().
            return Parser.    
        end. 
        set. 
        
    constructor public GetDataTableRequestBuilder (  ):
        super ().
    end constructor.
    
    method public GetDataTableRequestBuilder Where(pcWhere as character):
        this-object:Where = pcWhere.
        return this-object. 
    end method.     
    
    method public GetDataTableRequestBuilder Sort(pcSort as character):
        this-object:Sort = pcSort.
        return this-object. 
    end method.     
    
    method public GetDataTableRequestBuilder Build(pcentity as character):
        define variable oBe as IBusinessEntity no-undo.
        oBe = cast(Application:ServiceManager,IServiceManager):CreateBusinessEntity(pcEntity). 
        return Build(oBe:Datasets[1]:get-buffer-handle(1)). //  Parser:GetQueryDefinition(phBuffer, pFilter).
    end method.     
    
    method public GetDataTableRequestBuilder Build(phbuffer as handle):
        TableRequest = new GetDataTableRequest(phbuffer:table-handle:name). 
        cast(TableRequest,GetDataTableRequest):QueryDefinition = Parser:GetQueryDefinition(phBuffer, this-object:Where,this-object:Sort).
        return this-object. 
    end method.     
    
end class.