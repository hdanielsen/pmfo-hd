
/*------------------------------------------------------------------------
    File        : ISourceField
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 09/10/2022
    Notes       : define constant or hidden (non-primary key_ fields that are not mapped to BE temp-table  
  ----------------------------------------------------------------------*/   

interface Pmfo.Core.DataLayer.ISourceField:  
    define property SourceName       as character no-undo get.
    define property SourceBufferName as character no-undo get.
   
     
    method public logical Assign(phDbfield as handle).
end interface.