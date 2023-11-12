 
 /*------------------------------------------------------------------------
    File        : CancelError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Aug 02 09:31:49 EDT 2020
    Notes       : Used by MonitorTarget
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.

block-level on error undo, throw.

class Pmfo.Tools.Gui.Model.CancelError inherits ApplicationError: 
     
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public CancelError (  ):
        super ("Canceled").
    end constructor.
 
end class.