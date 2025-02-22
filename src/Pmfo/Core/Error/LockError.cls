 
 /*------------------------------------------------------------------------
    File        : LockError
    Purpose     : Throw if locked after no-wait.  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Jun 28 22:58:05 EDT 2019
    Notes       : As of current it is treated as a regular error, but 
                  DataAccess could have logic to retry if this is thrown
                  The transaction should be undone, but that is standard behavior
                  for any thrown error in the DataAccess or DataSource  
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.LockError inherits ApplicationError: 
    define variable cMsgTemplate as character init "'&1' is locked and cannot be updated. Try again." no-undo. 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public LockError (pTableName as character):
        super (subst(cMsgTemplate,pTableName)).
        /* What can we possibly do to handle bad use of an error
        It could be newed in a catch block based on error number 
        if not transaction then.   
        */    
    end constructor.
   
    // Use of another table peventa update on a trelated/child table 
    constructor public LockError (pcLockedTableName as character, pcUpdateTableName as character):
        super (subst("The &1 is locked. The &2 cannot be updated while the &1 is lcoked.","")).
        /* What can we possibly do to handle bad use of an error
        It could be newed in a catch block based on error number 
        if not transaction then.   
        */    
    end constructor.
    
    // the inner error os likely not very useful, but if needed the constructor is here
    constructor public LockError (pTableName as character, pError as Error):
        super (subst(cMsgTemplate,pTableName),pError).
        /* What can we possibly do to handle bad use of an error
        It could be newed in a catch block based on error number 
        if not transaction then.   
        */    
    end constructor.
    
    method protected override StatusCodeEnum GetStatusCodeEnum():
        return StatusCodeEnum:Locked.
    end method.
    
    
end class.