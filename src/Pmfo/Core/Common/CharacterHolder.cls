 
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
using Ccs.Common.Support.ILongcharHolder from propath.

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
    
    method override public logical Equals(input PO as Progress.Lang.Object):
        define variable oString as OpenEdge.Core.String no-undo.
        define variable valCompare as character no-undo.
        if super:Equals(PO) then
             return true.
         
        if type-of(PO, OpenEdge.Core.String) then
        do:
            oString = cast(PO, OpenEdge.Core.String).
            return 
                this-object:Value = oString:Value.
        end.
        else
        if type-of(PO, ICharacterHolder) then
        do:
            valCompare = dynamic-property(PO, 'Value').
            If this-object:Value eq valCompare then 
                return true.
        end.
        else
        if type-of(PO, ILongcharHolder) then
        do on error undo, throw:
            valCompare = dynamic-property(PO, 'Value').
            If this-object:Value eq valCompare then 
                return true.
            catch e as Progress.Lang.Error :
                return false.    
            end catch.   
        end.
        return false.
    end method.
    
end class.