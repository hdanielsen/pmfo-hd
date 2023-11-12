
/*------------------------------------------------------------------------
    File        : IDbQuery
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Feb 13 12:48:22 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Core.DataLayer.IDbQuery:  
    
    define public      property BaseQuery         as char            no-undo get. 
    define public      property BufferName        as char            no-undo get. 
    define public      property PhysicalTableName as char            no-undo get.  
    define public      property BufferHandles     as handle   extent no-undo get. 
    define public      property BufferHandle      as handle          no-undo get. 
    define public      property BufferNames       as character       no-undo get.
    define public      property QueryString       as character       no-undo get.
    
    method public logical Prepare ().
    method public logical ResetQuery ().
    
    
end interface.