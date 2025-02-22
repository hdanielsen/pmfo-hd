
/*------------------------------------------------------------------------
    File        : IGetDataTableResponse
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Apr 18 21:20:12 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.IGetDataTableResponse from propath.
using Ccs.BusinessLogic.IGetTableResultCountResponse from propath.
using OpenEdge.Core.Collections.ICollection from propath.

interface Pmfo.Core.BusinessLogic.IGetDataTableResponse inherits IGetDataTableResponse,IGetTableResultCountResponse:  
    define public property Public           as logical no-undo get. set.  
    define public property TrimExtentFields as character no-undo get. set.  
       // force return of  nextPageLink as context for NextPagingContext 
    define public property UseNextPageLink       as logical no-undo get. set.  
end interface.