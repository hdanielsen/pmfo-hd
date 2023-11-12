 
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

class Pmfo.Tools.Gui.Model.BusinessEntityModel inherits ClassModel: 
    
    define public property Resource as character no-undo get. set.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/        
    constructor public BusinessEntityModel ( pcname as char, pcInherits as char):
        super (pcName,pcInherits).
    end constructor.
    
    method public logical overrideBeforeSave():
    end method.
    
    method public logical overrideAfterRead():
    end method.
    
end class.