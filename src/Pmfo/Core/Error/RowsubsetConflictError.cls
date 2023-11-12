 
 /*------------------------------------------------------------------------
    File        : RowsubsetConflictError
    Purpose     : catch conflicts for subset of a row tracking changes to allow caller to collect multiple
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Jun 08 07:41:28 EDT 2023
    Notes       : Use for cases when the databuffer is mapped to many rows
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ConflictError.

block-level on error undo, throw.

class Pmfo.Core.Error.RowsubsetConflictError inherits ConflictError: 
    define public property Changes as character no-undo get. protected set.
    constructor public RowsubsetConflictError ( input pTableName as character, input pcChanges as character ):
        super (input pTableName, input pcChanges).
        Changes = pcChanges.
    end constructor.
end class.