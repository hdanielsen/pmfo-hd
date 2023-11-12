 
 /*------------------------------------------------------------------------
    File        : ClientDataSource
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Aug 13 20:28:03 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.ClientDataManager from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IClientEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableResponse from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.BusinessLogic.IClientFilteredEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.BusinessLogic.GetDataTableResponse from propath.

class Pmfo.Core.BusinessLogic.ClientFilterDataSource inherits ClientDataManager: 
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    define protected property ClientFilteredEntity as IClientFilteredEntity no-undo get. set.
    define protected property Response             as IGetDataResponse      no-undo get. set.
    
    define protected override property ClientName as character no-undo  
        get():
            if type-of(ClientFilteredEntity,IBusinessEntity) then
                return cast(ClientFilteredEntity,IBusinessEntity):Resource.
            else
                return ClientFilteredEntity:GetClass():TypeName.    
        end.
   
    constructor public ClientFilterDataSource (pClientFilteredEntity as IClientFilteredEntity  ):
        super ().
        ClientFilteredEntity = pClientFilteredEntity. 
    end constructor.
    
    method protected void DeepCopy(phHandle as handle, output table-handle phTable):
        phTable = phHandle:table-handle.
    end method.
    
    method protected override handle CreateSourceTable(phBuffer as handle):
        define variable hTable as handle no-undo.
        DeepCopy(phBuffer, output table-handle hTable).
        phBuffer:table-handle:empty-temp-table ().
        return hTable.       
    end method.   
     
    method protected override void CallBack(input pRequest as IGetDataTableRequest, input pResponse as IGetDataTableResponse,input phBuffer as handle):
        define variable hTable as handle no-undo.
        define variable iCount as int64 no-undo.
        hTable = phbuffer:table-handle.
        iCount = ClientFilteredEntity:ReceiveSourceRecords(input table-handle hTable by-reference).
         // if count requestd and no client filter then assign the count to the reponse 
        // (if there is a querydefintion then this is done by actually counting the filtered records) 
        if UseClientQuery(pRequest, phBuffer) = false and pRequest:Count then
        do:
            if iCount = ? then
            do:
                //  we could possibly count the records, but the assumption is that 
                //  it is uncommon to count and if it is needed it is easy and better
                //  to implement in the code that creates the records   
                undo, throw new NotImplementedError(subst("Count of &1",ClientName)).
            end.   
            cast(pResponse,GetDataTableResponse):NumResults = iCount.
        end.   
    end method. 
    
    // apply filter to the data if applicable  
    method public void ReceiveBuffer(input pRequest as IGetDataTableRequest, input pResponse as IGetDataTableResponse, input phBuffer as handle).
        define variable cClientFields    as character extent no-undo.
        define variable lHasClientFilter as logical no-undo.
        define variable lHasClientSort   as logical no-undo.
        
        if not valid-handle(phbuffer) then 
            undo, throw new IllegalArgumentError("Buffer is unknown").
        
        if phbuffer:type <> "buffer" then
            undo, throw new IllegalArgumentError(subst("Buffer parameter should be of type 'buffer' not '&1'",phbuffer:type)).
      
        ProcessRecords(pRequest,pResponse, phBuffer).
         
    end method. 
end class.