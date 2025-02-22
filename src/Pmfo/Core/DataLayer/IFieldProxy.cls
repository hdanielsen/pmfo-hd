
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

interface Pmfo.Core.DataLayer.IFieldProxy:  
    define property Name             as character no-undo get.
    define property Field            as handle    no-undo get.
    define property BeforeField      as handle    no-undo get.
    define property SourceField      as handle    no-undo get.
    define property IsMandatory      as logical   no-undo get.
    
    define property SourceName       as character no-undo get.
    define property SourceBufferName as character no-undo get.
    // fake name for ColumnSource
    define property ColumnSourceName as character no-undo get.
    define property SourceFullName   as character no-undo get.
    define property StringValue      as character no-undo get.
    
    define public property ReadOnly  as logical   no-undo get. 
   
    // hopefully temporary - used to keep trakc of use of use of old constructors
    // that defined everything immediately  
    define public property Prepared  as logical   no-undo get. 
   
    method logical Conflict().                   
    method logical Save().        
    method logical NeedsSave().        
                
    method logical Prepare(phDataBuffer as handle, phDatabaseBuffer as handle).
    
   
 // not in use   method void ResetSourceName(input pcName as character).
    method IQueryEntry ColumnExpression(input pPredicate as IQueryPredicate).
    method IQuerySortEntry ColumnSort(input pSort as IQuerySortEntry ).
    
    method void SetBufferValue(phDbBuffer as handle).
    method void SetBufferValue(pValue as character).
    method void SetBufferValue(pValue as integer).
    method void SetBufferValue(pValue as int64).
    method void SetBufferValue(pValue as logical).
    method void SetBufferValue(pValue as decimal).
    method void SetBufferValue(pValue as date).
    method void SetBufferValue(pValue as datetime).
    method void SetBufferValue(pValue as datetime-tz).
    
end interface.