 
 /*------------------------------------------------------------------------
    File        : DatasetHolder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 14 15:29:55 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

 

block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.Dataset.DatasetHolder from propath.
using Pmfo.Core.BusinessLogic.Dataset.IDatasetClientServiceHolder from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
 

class Pmfo.Core.BusinessLogic.Dataset.DatasetClientServiceHolder abstract inherits DatasetHolder implements IDatasetClientServiceHolder: 
    
    define public    property ServerDataset   as handle no-undo get. protected set.
    
    constructor public DatasetClientServiceHolder (phClientDataset as handle, phServerDataset as handle ):
        super (phClientDataset).
        ServerDataset = phServerDataset.
    end constructor.
    
    // clones the client data request and adds TableRequests for missing tables 
    // removes the NamedQuery as the assumtion is that this is used in a Function 
    
    method public IGetDataRequest CreateServerDataRequest(pClientDataRequest as IGetDataRequest).
        define variable oRequest as IGetDataRequest no-undo.
        define variable i as integer no-undo.
        define variable hBuffer as handle no-undo. 
        oRequest = cast(pClientDataRequest:Clone(),IGetDataRequest). 
        do i = 1 to ServerDataset:num-buffers: 
            hBuffer = ServerDataset:get-buffer-handle(i).
            
            // may have a different top request
            if i = 1 and not valid-handle(hBuffer:parent-relation) then 
                 oRequest:MainRequestedBuffer = hBuffer:name. 
            
            if not valid-object(oRequest:GetTableRequest(hBuffer:table-handle:name)) then 
            do:
                oRequest:AddTableRequest(new GetDataTableRequest(hBuffer:table-handle:name)).     
            end. 
               
        end.    
        return oRequest.                  
    end method. 
    
    
end class.