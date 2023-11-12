 
 /*------------------------------------------------------------------------
    File        : DatasetError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Jun 03 08:11:43 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.core.Error.ApplicationError.

block-level on error undo, throw.

class Pmfo.Core.Error.DatasetError inherits ApplicationError: 
    constructor public DatasetError ( phDataset as handle ):
        super (input subst("Update of dataset '&1' failed",phDataset:serialize-name)).        
    end constructor.
end class.