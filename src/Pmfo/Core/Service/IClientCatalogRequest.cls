
/*------------------------------------------------------------------------
    File        : IClientCatalogRequest
    Purpose     :  
    Syntax      : 
    Description : Defines a catalog request   
    Author(s)   : hdaniels
    Created     :  
    Notes       : 
  ----------------------------------------------------------------------*/


using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.Service.IClientRequest from propath.

interface Pmfo.Core.Service.IClientCatalogRequest inherits IClientRequest: 
    define public property SelectProp        as character no-undo get. 
    define public property URIQuery          as IStringStringMap no-undo get. 
    
    define public property Properties as char extent no-undo get. 
    define public property Select         as character  no-undo get.
     
    method public void AssertUriQuery().
    method public void AssertSelect().
     
end interface.