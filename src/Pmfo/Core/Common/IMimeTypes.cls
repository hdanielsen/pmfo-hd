
/*------------------------------------------------------------------------
    File        : IMimeTypes.
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Jun 04 12:13:40 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.Core.ISupportInitialize.

interface Pmfo.Core.Common.IMimeTypes inherits ISupportInitialize:  
    define property ValidMimeTypes  as character get.
    define property ValidExtensions as character get.
    
    method public logical IsValidMimeType(pcType as character).
    method public logical IsValidFileExtension(pcExtension as character).
    method public character GetMimeTypeFileExtension(pcMimetype as character).
    
    
end interface.