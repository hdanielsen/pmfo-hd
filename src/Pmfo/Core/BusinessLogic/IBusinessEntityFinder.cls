
/*------------------------------------------------------------------------
    File        : IBusinessEntityFinder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Oct 20 10:16:35 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.Common.IKey from propath.

interface Pmfo.Core.BusinessLogic.IBusinessEntityFinder:  
    
    define public property ThrowObsoleteError as logical no-undo get.
    define public property BusinessEntity as IBusinessEntity no-undo get.
    
    method public void Bind(output table-handle phTable bind ):
    method public void Find(piKey as integer,output table-handle hTable).
    method public void Find(pcKey as character,output table-handle hTable).
    method public void Find(poKey as IKey,output table-handle hTable).
         
end interface.