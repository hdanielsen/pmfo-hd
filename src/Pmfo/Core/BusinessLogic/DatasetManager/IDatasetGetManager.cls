
/*------------------------------------------------------------------------
    File        : IDatasetGetManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jan 10 10:16:18 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetManager from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.

interface Pmfo.Core.BusinessLogic.DatasetManager.IDatasetGetManager inherits IDatasetManager:  
    define public property ClientInnerJoinTables as character              no-undo get.
    define public property ClientSort            as IQuerySortEntry extent no-undo get.
    
end interface.