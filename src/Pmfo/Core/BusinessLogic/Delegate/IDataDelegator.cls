
/*------------------------------------------------------------------------
    File        : IBusinessEntityDelegator
    Purpose     : Data Delegator (BusinessEntity)
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 25 15:46:21 EDT 2022
    Notes       : Common properties nad methods for IFetchDelegator and ISubmitDelegator
                  Assuming DataAccess is  passed as parameter to the delegate methods
                - BE Dataccess is not public and should preferably remain so. 
                  if we need to we can have it public in a coomon class for this,
                  but not define it in the IBusinessentity                  
  ----------------------------------------------------------------------*/
 
 
 

using Pmfo.Core.BusinessLogic.Delegate.IBusinessEntityDelegator from propath.

interface Pmfo.Core.BusinessLogic.Delegate.IDataDelegator inherits IBusinessEntityDelegator: 
    define public property ClientOnly       as logical       no-undo get.  
end interface.