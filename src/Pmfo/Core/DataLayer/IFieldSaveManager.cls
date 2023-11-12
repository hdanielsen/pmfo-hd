
/*------------------------------------------------------------------------
    File        : IFieldSaveManager
    Purpose     : Allow override of conflict check and save for fields that cannot use save row changes
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Feb 13 12:53:43 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.DataLayer.IFieldManager from propath.

interface Pmfo.Core.DataLayer.IFieldSaveManager inherits IFieldManager:  
    define public property FieldNames        as character no-undo get. set. 
end interface.