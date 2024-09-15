 
 /*------------------------------------------------------------------------
    File        : DatasetRequestResponseHolder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 14 15:29:55 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.Dataset.DatasetHolder from propath.
using Pmfo.Core.BusinessLogic.Dataset.IDatasetRequestResponseHolder from propath.
using Pmfo.Core.Service.IClientUpdateRequest from propath.

class Pmfo.Core.BusinessLogic.Dataset.DatasetRequestResponseHolder abstract inherits DatasetHolder implements IDatasetRequestResponseHolder: 
    
    define public    property ResponseDataset   as handle no-undo get. protected set.
    
    constructor public DatasetRequestResponseHolder (phRequestDataset as handle, phResponseDataset as handle ):
        super (phRequestDataset).
        ResponseDataset = phResponseDataset.
    end constructor.
    
    method public override abstract void AddUpdateResponseRelations(pClientRequest as IClientUpdateRequest).
 
     
end class.