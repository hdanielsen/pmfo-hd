
/*------------------------------------------------------------------------
    File        : IFetchDelegator
    Purpose     : Data Read Delegator (BusinessEntity)
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
 

interface Pmfo.Core.BusinessLogic.Delegate.IFetchDelegator: 
    define public property ClientOnly         as logical       no-undo get.  
   // future if we move GetData to FetchDelegate here 
    define public property Resource       as character no-undo get.                                                                                                                        
    define public property DefaultDataset as handle    no-undo get.
   /*    method public ILifecycleScope PrepareFetch( input poRequest as IGetDataRequest, input phDataset as handle, output pClientInnerJoinTables as character, pCLientSort as IQuerySortEntry).*/
    method public void ReceiveData(pReceiveDataArgs as IReceiveDataEventArgs, phDataset as handle).
    method public void AddWarning(pcMessage as character).        
    method public character GetKeys(). 
end interface.