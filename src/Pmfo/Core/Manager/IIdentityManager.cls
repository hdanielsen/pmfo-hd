
/*------------------------------------------------------------------------
    File        : IIdentityManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Oct 22 13:34
    Notes       :   
  ----------------------------------------------------------------------*/


using Ccs.Common.IManager from propath.
using Progress.Lang.Enum from propath.

interface Pmfo.Core.Manager.IIdentityManager inherits IManager:  
    method public int64 NextId(pNextIdEnum as Enum).  
    
    // allow retrieval of multiple idents in one request
    method public int64 extent NextIds(pNextIdEnum as Enum, piNum as int).  
    
    // may throw NotImplementedError or UnsupportedOperationError 
    method public int64 CurrentId(pNextIdEnum as Enum).
    
    /* reset an id based on data - this requires dataaccess code specifically for the sequence
     - throw NotImplementedError when it in theory can be implementeded
     - throw UnsupportedOperationError when not possible to implement */  
    method public int64 ResetCurrentId(pNextIdEnum as Enum).
end interface.