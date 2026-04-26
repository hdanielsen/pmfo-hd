 
 /*------------------------------------------------------------------------
    File        : DataSourceNotFoundByPublicNameError
    Purpose     : Throw if CreateDataSource does not find a source and there is a DataSourceError record when 
                  the (potentially) missing source was detected when generating resources.json    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 7/8/2025 
    Notes       : This is assumed to be caused by a change to the name of the CodeTable for cases that 
                  intended to call a lookup table datasource by name or key using a generic datasource  
                  It could also happen if a table is deleted or a DataSource has been deleted                                       
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.DataSourceNotFoundByPublicNameError inherits ApplicationError: 
    define variable cMsgTemplate as character init "DataSource '&1' was not found. Please Contact IT. &2" no-undo.
     
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    constructor public DataSourceNotFoundByPublicNameError (pEntityName as character):
        super (subst(trim(cMsgTemplate,pEntityName))).
    end constructor.
    
    constructor public DataSourceNotFoundByPublicNameError (pEntityName as character, plLogged as logical):
        super (subst(cMsgTemplate,pEntityName,subst("A detailed message has been written to the log at &1 (approx)",now))).
    end constructor.
    
    method protected override StatusCodeEnum GetStatusCodeEnum():
        return StatusCodeEnum:NotFound.
    end method.
end class.