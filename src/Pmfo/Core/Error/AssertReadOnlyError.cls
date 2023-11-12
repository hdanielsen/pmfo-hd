 
 /*------------------------------------------------------------------------
    File        : Error.AssertReadOnlyError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sept 2019
    Notes       : To be called from Assert method so assumes call stack is 4 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Error.StackError from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.AssertReadOnlyError inherits StackError: 
    constructor public AssertReadOnlyError ( input pmsg as character ):
        super (input subst("&1 &2",GetMethodName(4),pMsg)).
    end constructor.
    
    constructor public AssertReadOnlyError ( input pmsg as character, piStack as integer ):
        // add 1 for this constructor and 1 for getmethod
        super (input subst("&1 &2",GetMethodName(piStack + 2),pMsg)).
    end constructor.
    
    constructor public AssertReadOnlyError (  ):
        this-object(GetMethodName(4) + "is read only" ).
    end constructor.
    
    
end class.