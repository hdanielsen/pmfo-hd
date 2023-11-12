 
 /*------------------------------------------------------------------------
    File        : QueryTimeOutError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jul 17 11:05:54 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.RequestTimeOutError.

block-level on error undo, throw.

class Pmfo.Core.Error.QueryTimeOutError inherits RequestTimeOutError: 
    define private property RequestName as character init "query" no-undo get.
    define private property RequestInfo as character init "Use filter and/or paging to limit the query request time" no-undo get.
    
    constructor public QueryTimeOutError ( ):
        super (input RequestName).
    end constructor.

    constructor public QueryTimeOutError ( input piTime as integer ):
        super (input RequestName, input piTime).
    end constructor.
    
    constructor public QueryTimeOutError (poStop as StopAfter, input piTime as integer ):
        super (input RequestName, input piTime).
        InnerStop = poStop.    
    end constructor.
    
    // override to remove 'request' 
    method protected override character GetRequestName(pToken as char):
        return pToken. 
    end method.
    
    // Called from super constructor to be passed to its super 
    // override to add info text
    // woriks also for the case with no ptime 
    method protected override character GetMessage(pToken as char, piTime as integer):
        return subst("&1 &2",super:GetMessage(pToken,piTime),RequestInfo).
    end method.
end class.