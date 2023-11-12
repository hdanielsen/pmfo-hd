
/*------------------------------------------------------------------------
    File        : ICatalog
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Apr 11 21:39:22 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using OpenEdge.Core.Collections.ICollection from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.Common.IDataService from propath.

interface Pmfo.Core.Catalog.ICatalog inherits IDataService:  
    method public JsonObject GetResources (pcBasePath as character,pcCollection as character extent).
    method public JsonObject GetResources(pcBasePath as character,pcCollections as character extent, pResponseRoot as character).
end interface.