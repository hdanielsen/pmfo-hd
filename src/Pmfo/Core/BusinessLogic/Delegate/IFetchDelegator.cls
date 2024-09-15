
/*------------------------------------------------------------------------
    File        : IFetchDelegator
    Purpose     : Fetch Delegator (BusinessEntity)
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 25 15:46:21 EDT 2022
    Notes       : The property and methods allow the same Delegate to be used for]
                  all BEs (instead of one for each). 
                  Assuming DataAccess is  passed as parameter to the delegate methods
                - BE Dataccess is not public and should preferably remain so. 
                  if we need to we can have it public in the class for this,
                  but not define it in the IBusinessentity                  
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.BusinessLogic.IReceiveDataEventArgs from propath.
using Pmfo.Core.BusinessLogic.Delegate.IDataDelegator from propath.
 

interface Pmfo.Core.BusinessLogic.Delegate.IFetchDelegator inherits IDataDelegator: 
    define public property DefaultDataset as handle    no-undo get.
    method public void ReceiveData(pReceiveDataArgs as IReceiveDataEventArgs, phDataset as handle).
end interface.