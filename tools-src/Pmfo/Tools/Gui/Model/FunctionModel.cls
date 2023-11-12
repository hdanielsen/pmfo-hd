 
 /*------------------------------------------------------------------------
    File        : ProcedureModel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Feb 22 17:07:46 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Tools.Gui.Model.CodeModel from propath.

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.FunctionModel inherits CodeModel: 

    define public property DataType as character no-undo 
    get.
    set . 

    constructor public FunctionModel (pcname as char  ):
        super ().
        Name = pcname.
    end constructor.


end class.