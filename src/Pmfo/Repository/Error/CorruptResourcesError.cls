 
 /*------------------------------------------------------------------------
    File        : CorruptResourcesError
    Purpose     : 
    Syntax      : 
    Description : The development mode code that automatically refreshes json
                  sometimes creates a corrupt json with lots of nulls in a property. 
                  This error should be thrown to tell the BE to generate the resources from scratch instead 
    Author(s)   : hdaniels
    Created     : Wed Oct 13 03:20:58 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.

block-level on error undo, throw.

class Pmfo.Repository.Error.CorruptResourcesError inherits ApplicationError: 


    constructor public CorruptResourcesError (pcFilename as character, pe as Error ):
        super (subst("Failed to read resources from file &1. The file is corrupt",pcFilename),pe).
    end constructor.
end class.