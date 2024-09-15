
/*------------------------------------------------------------------------
    File        : IFieldProxy
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Sep 26 06:56:59 EDT 2020
    Notes       : 
  ----------------------------------------------------------------------*/   

using Ccs.BusinessLogic.IQueryEntry from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Pmfo.Core.BusinessLogic.IQueryPredicate from propath.

interface Pmfo.Core.DataLayer.IDatabaseFieldProxy:  
     
    define property SourceField      as handle    no-undo get.
    define property IsMandatory      as logical   no-undo get.
    
    define property SourceName       as character no-undo get.
    define property SourceBufferName as character no-undo get.
    define property SourceFullName   as character no-undo get.
    
    define public property ReadOnly  as logical   no-undo get. 
   
    // hopefully temporary - used to keep trakc of use of use of old constructors
    // that defined everything immediately  
    define public property Prepared  as logical   no-undo get. 
   
    method logical Conflict().                   
    method logical Save().                    
    method logical Prepare(phDataBuffer as handle, phDatabaseBuffer as handle).
    
    
end interface.