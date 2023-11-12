 
 /*------------------------------------------------------------------------
    File        : FindParentsRequestBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Jul 30 08:08:41 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.ISet from propath.
using Pmfo.Core.BusinessLogic.RequestBuilder.DataRequestBuilder from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using OpenEdge.Core.Collections.IMap from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.

class Pmfo.Core.BusinessLogic.RequestBuilder.FindParentsRequestBuilder inherits DataRequestBuilder: 

        
    constructor public FindParentsRequestBuilder (  ):
        super ().
    end constructor.
    
    method override protected void ProcessMainTableRequest( input pRequests as OpenEdge.Core.Collections.ICollection, input pClientRequest as Pmfo.Core.Service.IClientGetRequest, input pTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest, input phBuffer as handle, input piCollection as integer, input poRequestType as Pmfo.Core.BusinessLogic.RequestTypeEnum ):
        // nothing        
    end method.
    
    // separated out for super class override 
    method protected override logical IsRequestMainTable(pClientRequest as IClientRequest,piCollection as integer):
        // For find request second to last is the main table so add one to collection   
        return super:IsRequestMainTable( pClientRequest, piCollection + 1).  
    end method.    
    
end class.