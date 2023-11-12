 
 /*------------------------------------------------------------------------
    File        : LifecycleScope
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Jan 09 12:20:09 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.ServiceManager.ILifecycleScope.
using OpenEdge.Core.WidgetHandle from propath.

block-level on error undo, throw.

class Pmfo.Core.Common.LifecycleScope implements ILifecycleScope: 

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    define protected property Scope as Object no-undo get. set. 
       
    constructor public LifecycleScope (pScope as Object):
        super ().
        Scope = pScope.
    end constructor.
    
    constructor public LifecycleScope (pScope as handle):
        super ().
        Scope = new WidgetHandle(pScope).
    end constructor.
    
    method override public logical Equals( input objectRef as Progress.Lang.Object ):
        if not super:Equals(input objectRef) then 
        do:
            if type-of(objectRef,ILifecycleScope) then 
                return cast(objectRef,ILifecycleScope):getScope():Equals(getScope()).  
            else 
                return false. 
        end.
        return true.    
    end method.

    method public Progress.Lang.Object getScope(  ):
        return Scope.  
    end method.

end class.