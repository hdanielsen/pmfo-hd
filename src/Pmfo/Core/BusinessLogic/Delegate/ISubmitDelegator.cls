
/*------------------------------------------------------------------------
    File        : IFetchDelegator
    Purpose     : Submit Delegator (BusinessEntity)
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 25 15:46:21 EDT 2022
    Notes       : The property and methods allow the same Delegate to be used for]
                  all BEs (instead of one for each). 
                  Assuming DataAccess is  passed as parameter to the delegate methods
                - BE Dataccess is not public and should preferably remain so. 
                                     
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.BusinessLogic.Delegate.IDataDelegator from propath.
using Pmfo.Core.Common.IWarningMessages from propath.
using Pmfo.Core.Service.IInfrastructureService from propath.

interface Pmfo.Core.BusinessLogic.Delegate.ISubmitDelegator inherits IDataDelegator: 
   
    // not part of IBE - only for call back from ISubmitDelegate 
    define public property IsInfrastructureServiceStarted as logical no-undo get. set.
    // not part of IBE - only for call back from ISubmitDelegate 
    define public property InfrastructureService as IInfrastructureService no-undo get.
   
   define public property WarningMessages    as IWarningMessages no-undo get. set.
    
end interface.