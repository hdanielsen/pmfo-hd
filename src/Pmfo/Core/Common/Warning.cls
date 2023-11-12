 
 /*------------------------------------------------------------------------
    File        : Warning
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Aug 08 21:10:23 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Common.IWarning from propath.

block-level on error undo, throw.

class Pmfo.Core.Common.Warning implements IWarning: 
    define public property Message   as character get. protected set.
    define public property FieldName as character get. protected set.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public Warning ( pMessage as char  ):
        super ().
        this-object:Message = pMessage.
    end constructor.
    
    constructor public Warning ( pMessage as char, pcField as character  ):
        this-object(pMessage).
        FieldName = pcField.
    end constructor.
    
    method public override character ToString():
       return this-object:Message.     
    end method.
   
end class.