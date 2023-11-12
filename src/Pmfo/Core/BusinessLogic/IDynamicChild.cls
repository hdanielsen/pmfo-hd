
/*------------------------------------------------------------------------
    File        : IDynamicChild
    Purpose     : Allow BE to load methods during rout resolution 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Mar 23 08:00:08 EDT 2022
    Notes       : Typically used together with IDynmicRelation and BE GetFixedChildren
                  The combined usage of these is likely to be refined and improved  
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.

interface Pmfo.Core.BusinessLogic.IDynamicChild:
    // load a single action or function depending on the type of the client request in a case where this is a true child
    method public void LoadMethod(pClientRequest as IClientRequest,phParentBuffer as handle, pcForeignKeys as character extent).
    // load a single action or function depending on the type of the client request 
    // (it is perhaps not necessary to define this as a public method since it is intended for 
    // cases where the dynamic child is not a child so it would most likely be called from 
    // the class' own GetRequestDataset and thus does not need to be public) 
    method public void LoadMethod(pClientRequest as IClientRequest,pcParentName as character).
    
end interface.