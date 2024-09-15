
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

interface Pmfo.Core.BusinessLogic.Delegate.IValidateDelegator inherits IBusinessEntityDelegator: 
  //  define public property CreateOnlyFields       as character no-undo get. set.
  //  define protected property ReadOnlyFields         as character no-undo get. set.
    define public property ValidateRequiredFields as logical   get. set.
    define public property ServerFields           as character get. set. // allow ? on client - used for keys and create date and such 
    // use for fields that have init ? to enforce a value from UI (otherwise init ? bypasses ValidateRequiredFields) 
    define public property RequiredFields         as character no-undo get. set.
     
end interface.