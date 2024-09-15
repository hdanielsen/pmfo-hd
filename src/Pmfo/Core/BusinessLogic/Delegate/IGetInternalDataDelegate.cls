
/*------------------------------------------------------------------------
    File        : IGetInternalDataDelegate
    Purpose     : delegate for operations that needs to use GetData to repopulate the internal main table
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 03 08:46:02 EST 2024
    Notes       : 
  ----------------------------------------------------------------------*/

 
using Pmfo.Core.BusinessLogic.Delegate.IGetInternalDataDelegator from propath.

interface Pmfo.Core.BusinessLogic.Delegate.IGetInternalDataDelegate:  
    method public logical CompareAndRefreshCurrentSelf(pDelegator as IGetInternalDataDelegator, phBuffer as handle, pcRefreshFields as char, pcTooLateFields as char, output pcRefreshInfo as character).   
    method public void ConvertCreatesToModified(input pDelegator as IGetInternalDataDelegator,phbuffer as handle).
    method public void ConvertCreatesToModified(input pDelegator as IGetInternalDataDelegator,phbuffer as handle,pcQueryExpression as character).
end interface.