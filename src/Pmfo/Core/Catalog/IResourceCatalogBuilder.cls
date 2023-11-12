
/*------------------------------------------------------------------------
    File        : IResourceCatalogBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Apr 27 01:10:52 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.Catalog.ICatalogRequest from propath.

interface Pmfo.Core.Catalog.IResourceCatalogBuilder:  
    method public JsonObject BuildCatalog(pRequest as ICatalogRequest, poEntity as IEntity). 
end interface.