 
 /*------------------------------------------------------------------------
    File        : GetDataTableResponse
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Apr 06 10:39:02 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using OpenEdge.BusinessLogic.GetDataTableResponse.
using Ccs.BusinessLogic.IGetTableResultCountResponse from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableResponse from propath.

block-level on error undo, throw.

class Pmfo.Core.BusinessLogic.GetDataTableResponse inherits GetDataTableResponse implements IGetDataTableResponse: 
    /* Returns is the result is exact (TRUE) or Guessed or Cached (FALSE) */
    define public property Exact  as logical no-undo get. set. 
    
    /* tells service how to create an URI id - if private the parent must be appended */
    define public property Public as logical no-undo 
        get. 
        set(plPublic as logical):
            if plPublic = ?
                then undo, throw new IllegalArgumentError("Parameter passed to set public is ?").
            Public = plPublic. 
        end. 
    
    // CSV of tt extent fields that should be trimmed (removing null) 
    define public property TrimExtentFields as character no-undo get. set.  
    // force use of nextpagelink when nextPagingContext has a value
    define public property UseNextPageLink as logical no-undo  get. set.  
  
    /* Returns the number of results for this table */
    define public property NumResults as int64 no-undo get. set. 
        
    constructor public GetDataTableResponse ( input pTableName as character ):
        super (input pTableName).
    end constructor.
    
    constructor public GetDataTableResponse ( input pTableName as character, pNumResults as int64 ):
        super (input pTableName).
        NumResults = pNumResults.
    end constructor.
    
end class.