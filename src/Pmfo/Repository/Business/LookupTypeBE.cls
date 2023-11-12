 
 /*------------------------------------------------------------------------
    File        : LookupTypeBE
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun May 23 11:18:29 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.BusinessEntity.

block-level on error undo, throw.

class Pmfo.Repository.Business.LookupTypeBE abstract inherits BusinessEntity: 
    &scop mode protected
    {Pmfo/Repository/schema/lookupType.i}
    &undefine mode
    
    define dataset dsLookupType for ttLookupType.

    constructor LookupTypeBE ():
        super(dataset dsLookupType:handle).
      //  Public = false.
    end constructor.     
    
end class.
 