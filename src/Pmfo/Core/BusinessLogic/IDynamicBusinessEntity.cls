
/*------------------------------------------------------------------------
    File        : IDynamicBusinessEntity
    Purpose     : Allow BE to create dataset and temp-table dynamcially 
                  Typically when linked to parenr 
    Description : 
    Author(s)   : hdaniels
    Created     : May 2024 
    Notes       :    
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.

interface Pmfo.Core.BusinessLogic.IDynamicBusinessEntity:
    // create dataset and temp table from some handle (typicially parent buffer handle in relation)
    method public void CreateFromParentBuffer(phParentHandle as handle, poRelation as IRelation ). 
    // set when started from manager that already has the created table in the dataset
    method public void SetBuffer(phTable as handle).   
end interface.