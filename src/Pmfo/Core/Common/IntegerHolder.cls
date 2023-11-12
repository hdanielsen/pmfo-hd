 
 /*------------------------------------------------------------------------
    File        : IntegerHolder
    Purpose     : Plain integer (not int64) 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Oct 04 18:03:42 EDT 2021
    Notes       : This is to ensure that PritiveHolderUtil can be used to support
                  all Progress scalar data types bidirectional. 
                  We used NamedQuery:Parameters and their types to SetParameter to a Parameterlist
                  that is then used in :GetClass():GetMethod to find methods dynamically
                  and need to distinguish integer and int64. 
                  So we need to ensure that NamedQuery Paramters can be defdined as plain integer
                - The OpenEdge.Core.Integer inmplements IInt64Holder and there is no
                  OpenEdge.Core class that implements IIntegerHolder 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.Common.Support.IIntegerHolder.

block-level on error undo, throw.

class Pmfo.Core.Common.IntegerHolder implements IIntegerHolder: 

    define public property Value as integer no-undo 
    get.
    set. 
    
    constructor public IntegerHolder(input piValue as integer):
        assign this-object:Value = piValue.
    end constructor.

    method override public character ToString():
        return string(this-object:Value).
    end method.

end class.