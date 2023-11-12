 
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
using OpenEdge.Core.DataTypeEnum from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.List from propath.

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.MethodModel inherits CodeModel: 

    define public property DataType as DataTypeEnum no-undo 
    get.
    set. 
    
    define public property Parameters as IList no-undo 
        get():
            if not valid-object(Parameters) then 
                Parameters = new List().
            return Parameters.    
        end.
        set. 
    
    constructor public MethodModel (pcname as char  ):
        super ().
        Name = pcname.
    end constructor.


end class.