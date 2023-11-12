
/*------------------------------------------------------------------------
    File        : IValidationFindService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Nov 23 10:08:35 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.RelationValidationEnum from propath.

interface Pmfo.Core.BusinessLogic.IRelationValidator:  
    
    // default validation is strict - define ValdationRule in the relation to override default   
    method public void ValidateLinks(phBuffer as handle,Links as ICollection).
    method public void ValidateLinks(phBuffer as handle,Links as ICollection, pcLinkNames as character).
    method public void ThrowAnyErrors(). 
    
end interface.