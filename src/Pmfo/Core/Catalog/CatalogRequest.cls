 
 /*------------------------------------------------------------------------
    File        : CatalogRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 24 11:54:26 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Catalog.ICatalogRequest from propath.

block-level on error undo, throw.

class Pmfo.Core.Catalog.CatalogRequest implements ICatalogRequest: 
    define public property Path             as character no-undo get. protected set.
    define public property ServiceName      as character no-undo get. protected set.
   
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public CatalogRequest (pPath as character, pServiceName as char ):
        super ().
        Path = pPath.
        ServiceName = pServiceName.
    end constructor.
    
end class.