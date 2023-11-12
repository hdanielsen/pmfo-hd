
/*------------------------------------------------------------------------
    File        : ICharacterArrayHolder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdanielsen PSC
    Created     : Tue Jun 07 09:40:26 EDT 2016
    Notes       : OpenEdge.Core does not have implementnion of this  
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

 
using Ccs.Common.Support.ICharacterArrayHolder from propath.

class Pmfo.Core.Common.CharacterArrayHolder     implements ICharacterArrayHolder:
    /** (mandatory) The character array held by this holder class */
    define public property Value as character extent no-undo get. private set.
    
    /* Constructor
       
       @param character[] The character array value */
    constructor public CharacterArrayHolder(input pcValue as character extent):
        assign this-object:Value = pcValue.
    end constructor.

end class.