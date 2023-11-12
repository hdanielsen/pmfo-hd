 
 /*------------------------------------------------------------------------
    File        : JoinPredicate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 10 12:19:22 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.BusinessLogic.IJoinPredicate.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.JoinPredicate implements IJoinPredicate: 

    define public property ChildField as character no-undo get. protected set . 
    define public property ParentField as character no-undo get. protected set .
  
        /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public JoinPredicate (pcParentfield as character,pcChildField as character ):
        super ().
        parentField = pcParentfield.
        ChildField  = pcChildField.
    end constructor.


end class.