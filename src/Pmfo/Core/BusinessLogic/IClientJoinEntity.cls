
/*------------------------------------------------------------------------
    File        : IClientJoinEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Aug 13 20:28:33 EDT 2020
    Notes       : manage inner join to parent on client 
                 - may default to not implemented for field or entity
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.

interface Pmfo.Core.BusinessLogic.IClientJoinEntity:  
    //may default to not implemented for field or entity 
    // or it may remove criteria and keep track of them and apply them in ReceiveBuffer   
    method public void PrepareInnerJoin(pRequest as IGetDataRequest, phBuffer as handle, output plHasClientpredicates as logical, output poClientSort as IQuerySortEntry extent).
end interface.