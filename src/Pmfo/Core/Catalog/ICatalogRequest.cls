
/*------------------------------------------------------------------------
    File        : ICatalogRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 24 13:00:10 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Core.Catalog.ICatalogRequest:  
    define public property Path             as character no-undo get.
    define public property ServiceName      as character no-undo get.
end interface.