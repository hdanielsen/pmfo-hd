 
 /*------------------------------------------------------------------------
    File        : DataSourceModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Mar 09 15:27:54 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.ClassModel.

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.DataSourceModel inherits ClassModel: 
    
    define public property FieldMapping as character no-undo get. set.
    define public property BufferNames as character extent no-undo get. set.
    define public property DataTable as character extent no-undo get. set.
    define public property Table as character extent no-undo get. set.
    define public property BaseQuery as character no-undo get. set.
    
    define public property OverridePrepareBuffer as logical no-undo get. set.
    define public property OverridePrepareSaveBuffer as logical no-undo get. set.
    

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public DataSourceModel (pname as char, pInherits as char ):
        super (pname,pinherits).
        
    end constructor.
      
end class.