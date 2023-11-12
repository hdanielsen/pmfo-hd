 
 /*------------------------------------------------------------------------
    File        : CharacterHolder
    Purpose     : Plain character (not longchar) 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Oct 04 18:03:42 EDT 2021
    Notes       : This is to ensure that PrimitiveHolderUtil can be used to support
                  all Progress scalar data types bidirectional. 
                  We use NamedQuery:Parameters and their types to SetParameter to a Parameterlist
                  that is then used in :GetClass():GetMethod to find methods dynamically
                  and need to distinguish integer and int64. 
                  So we need to ensure that NamedQuery Paramters can be dedined as plain character
                - The OpenEdge.Core.String inmplements ILongcharHolder and there is no
                  OpenEdge.Core class that implements in ICharacterHolder 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.Common.Support.ICharacterHolder.

block-level on error undo, throw.

class Pmfo.Core.Common.CharacterHolder implements ICharacterHolder: 

    define public property Value as character no-undo 
    get.
    set. 
    
    constructor public CharacterHolder(input pcValue as character):
        assign this-object:Value = pcValue.
    end constructor.

    method override public character ToString():
        return string(this-object:Value).
    end method.

end class.