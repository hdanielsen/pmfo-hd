 
 /*------------------------------------------------------------------------
    File        : GetFunctionDataTableRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu May 30 10:00:54 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.GetDataTableRequest.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.IQueryEntry from propath.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.GetFunctionDataTableRequest inherits GetDataTableRequest: 
    // we use querypredicates to carry the parameters... 
    // we utilize the QueryRequestbuilders 
    // and this also allows the functions to use GetData 
    // the assumption is that the consumer knows the data type (the Psi QueryPreciate only uses String)  
    define public property Parameters   as IQueryEntry no-undo  get. set. 
    define public property FunctionName as character no-undo get. set. 
        
    constructor public GetFunctionDataTableRequest ( input pTableName as character ):
        super (input pTableName).
    end constructor.
    
    constructor public GetFunctionDataTableRequest ( input pTableName as character, pFunction as char, pParameters as IQueryEntry):
        super (input pTableName).
        Parameters = pParameters.
        FunctionName = pFunction.
    end constructor.
      
end class.