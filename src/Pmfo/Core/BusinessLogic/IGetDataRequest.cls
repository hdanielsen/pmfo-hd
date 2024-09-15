
/*------------------------------------------------------------------------
    File        : IGetDataRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu May 30 20:36:00 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.IGetDataRequest from propath.
using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using OpenEdge.Core.Collections.IList from propath.
using Pmfo.Core.BusinessLogic.IRequest from propath.
using Pmfo.Core.Common.IGetDataQuestions from propath.

interface Pmfo.Core.BusinessLogic.IGetDataRequest inherits IGetDataRequest,  IRequest:  
    define public property HasWarnings  as logical no-undo get.
    define public property Warnings     as IList   no-undo get. set.
    define public property HasQuestions as logical no-undo get.   
    define public property Questions as IGetDataQuestions  no-undo get. 
      
    // set to true for singlerow method call to return empty
    define public property NoError             as logical no-undo get. set. 
    // if true responses (for example list of field names) should have public names 
    define public property External            as logical no-undo get. set.  
    define public property Timeout             as integer no-undo get. set.
     // true if all table requests are for the client
    define public property ClientOnly          as logical no-undo get. 
    
    method public logical HasTableRequest(pName as character).
    method public logical HasTableRequest(pHandle as handle). 
    method public IGetDataTableRequest GetTableRequest(pName as character).
    method public IGetDataTableRequest GetTableRequest(pHandle as handle).
    method public logical AddTableRequest(pTableRequest as IGetDataTableRequest).
    
    method public logical EmptyQuestions().
    
    method public void RemoveNamedQuery().
end interface.