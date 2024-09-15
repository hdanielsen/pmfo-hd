
/*------------------------------------------------------------------------
    File        : IDatasetHolder
    Purpose     : Allow different relations and different tables, but same data for same tables for client and server  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 14 15:25:20 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IRelationDefinition from propath.
using Pmfo.Core.Service.IClientUpdateRequest from propath.

interface Pmfo.Core.BusinessLogic.Dataset.IDatasetHolder:  
     // the main public dataset - should be backed by relation definitions for correct one-to one and many-to one serialization  
     define public property Dataset   as handle no-undo get.
     
     // When we have two datasets that shares data we do not currently fully support expand since it is based 
     // dynamically created buffers from each BE so this method tries to remedy this limitation somewhat   
     // since this dataset typically overrides the one returned from the Datasetrouter
     // by adding the select from the expand and by adding relations for correct json serialization if expand
     // is not used  
      
     method public void AddLinksAndSelects(pBe as IBusinessEntity,pClientRequest as IClientGetRequest).
     method public void AddUpdateResponseRelations(pClientRequest as IClientUpdateRequest).
     method public void setVisibleChildren(phDataset as handle,
                                          pcPath as character, 
                                          pPublicPropnames as character).
    // method public IRelationDefinition GetClientLink(pcParentName as character, pcPublicName as character):
           
     // allow deep copy (only for resue of main defs - if you need both keep the class alivee)
     method public void GetDataset(output dataset-handle phDataset).
    
end interface.