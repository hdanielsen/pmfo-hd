 
 /*------------------------------------------------------------------------
    File        : LockTimeOutError
    Purpose     : Wrapper for a Progress.Lang.LockConflict 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat May 16 06:51:11 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Pmfo.Core.Error.RequestTimeOutError from propath.
using Progress.Lang.LockConflict from propath.
using Progress.Lang.Stop from propath.

class Pmfo.Core.Error.LockTimeOutError inherits RequestTimeOutError: 
    define protected variable ReasonTemplate as character no-undo 
           init " waiting for a record lock on table '&1' held by &2 on device '&3'". 
    
    constructor public LockTimeOutError (pInnerStop as LockConflict ):
        super (input "update",  input pInnerStop).
    end constructor.
    
    constructor public LockTimeOutError (pcType as char,pInnerStop as LockConflict ):
        super (input pctype,  input pInnerStop).
    end constructor.
    
    // Called from super constructor to be passed to its super 
    // override to add reason/info text
    method protected override character GetMessage(pToken as char,pInnerStop as Stop):
        return GetMessage(pToken,cast(pInnerStop,LockConflict)).
    end method.
    
    method protected character GetMessage(pToken as char,pLockConflict as LockConflict):
        return subst(MessageTemplate,pToken,subst(ReasonTemplate,pLockConflict:TableName,GetLockUserText(pLockConflict),pLockConflict:Device)).
    end method.
    
    method protected character GetLockUserText(pLockConflict as LockConflict):
        return subst("user id '&1'&2",entry(1,pLockConflict:User),if num-entries(pLockConflict:User) > 1 then subst(" user no &1",entry(2,pLockConflict:User)) else "").
    end method.    
end class.