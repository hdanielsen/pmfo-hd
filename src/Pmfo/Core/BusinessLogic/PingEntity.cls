 
 /*------------------------------------------------------------------------
    File        : PingEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Apr 29 12:08:41 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.ServiceEntity.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.PingEntity inherits ServiceEntity: 

    define override public property Resource as character init "Ping" no-undo 
    get.
    set. 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public PingEntity (  ):
        super ().
    end constructor.

    method public void Execute():
        // the default behavior is to return 204, so no logic or code is needed
    end method.    

end class.