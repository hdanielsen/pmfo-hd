 
 /*------------------------------------------------------------------------
    File        : CodeModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Feb 22 17:07:46 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.CodeModel: 
    define property name   as char      no-undo get. set.
    define property code   as longchar  no-undo get. set.

    constructor public CodeModel (  ):
        super ().
    end constructor.

end class.