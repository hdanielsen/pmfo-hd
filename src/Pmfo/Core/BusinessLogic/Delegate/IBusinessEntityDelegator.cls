
/*------------------------------------------------------------------------
    File        : IBusinessEntityDelegator
    Purpose     : Data Delegator (BusinessEntity)
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 25 15:46:21 EDT 2022
    Notes       : Common properties and methods for BusinessEntity Delegators 
                  (DataDelegator, IFetchDelegator and ISubmitDelegator)
                                    
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.BusinessLogic.IUpdateDataEventArgs from propath.

interface Pmfo.Core.BusinessLogic.Delegate.IBusinessEntityDelegator: 
    define public property DefaultDataset as handle    no-undo get.  
    define public property Resource         as character no-undo get.                                                                                                                        
    method public void AddWarning(pcMessage as character).        
    method public character GetKeys().
      
end interface.