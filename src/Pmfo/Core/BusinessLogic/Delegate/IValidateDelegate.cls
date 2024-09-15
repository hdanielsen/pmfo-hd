
/*------------------------------------------------------------------------
    File        : IValidateDelegate
    Purpose     : Handle Validate actions for a BE 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 03 06:26:58 EST 2024
    Notes       : Validate actions are actions that trigger BE
                  ValidateTable and FieldValidationService for a specific resource
                  without data access 
  ----------------------------------------------------------------------*/

using Progress.Lang.Object from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetUpdateManager from propath.
using Pmfo.Core.BusinessLogic.Delegate.IBusinessEntityDelegator from propath.
using Pmfo.Core.Common.IKey from propath.
using Pmfo.Core.Common.IFieldValue from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataEventArgs from propath.
 
interface Pmfo.Core.BusinessLogic.Delegate.IValidateDelegate:  
    method public void ValidateBuffer(pDelegator as IBusinessEntityDelegator, 
                                      pRequest as IUpdateDataEventArgs, 
                                      phBuffer as handle).
   
    // called from actions so returns Object
    method public Object ValidateChildRow(
        input pDelegator as IBusinessEntityDelegator, 
        input pManager as IDatasetUpdateManager,
        input pChildResource as character,
        input pKey as IKey,
        input poUpdateDataRequest as IUpdateDataRequest, 
        input phDataset as handle).
     
      // called from actions so returns Object
     method public Object ValidateRow(
        input pDelegator as IBusinessEntityDelegator, 
        input pManager as IDatasetUpdateManager,
        input pKey as IKey,
        input poUpdateDataRequest as IUpdateDataRequest, 
        input phDataset as handle).
    /**                                 
    method public Object Validate(input pDelegator as IBusinessEntityDelegator, 
                                  input pManager as IDatasetUpdateManager,
                                  input pChildResource as character,
                                  input pFields as IFieldValue extent,
                                  input poUpdateDataRequest as IUpdateDataRequest, 
                                  input-output dataset-handle phDataset).
                                  **/                              
end interface.