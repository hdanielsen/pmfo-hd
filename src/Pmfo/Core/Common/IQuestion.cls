
/*------------------------------------------------------------------------
    File        : IQuestion
    Purpose     : 
    Syntax      : 
    Description : Support server side question in business logic or data access
    Author(s)   : hdaniels
    Created     : Mon Aug 19 12:00:54 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

interface Pmfo.Core.Common.IQuestion:  
    define public property Id      as integer   no-undo get.  
    define public property Message as character no-undo get. 
    
end interface.