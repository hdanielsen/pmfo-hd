 
 /*------------------------------------------------------------------------
    File        : HiddenSourceKey
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Dec 03 09:43:11 EST 2022
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.DataLayer.IHiddenSourceKey.

block-level on error undo, throw.

class Pmfo.Core.DataLayer.HiddenSourceKey abstract implements IHiddenSourceKey: 

    define public property DisplayValue as character no-undo 
    get.
    set. 

    define public property SourceBufferName as character no-undo 
    get.
    set. 

    define public property SourceName as character no-undo 
    get.
    set. 
    
    constructor public HiddenSourceKey (phDbBuffer as handle, pcSourceName as character):
        SourceBufferName = phDBBuffer:name.
        SourceName = pcSourceName.
       // SourceFullName = subst("&1.&2",SourceBufferName,SourceName).
    end constructor.

    // subclass may call this to initiate a key value from  buffer before create
    // if ther buffer is touched in assign after the record is created it will cause error
    // if the value is in conflict ot mandatory because the buffer is validated      
    method public logical Prepare(phDbfield as handle):
    end method.   
    
    method protected character GetPrepareWhereExpression():
        return "".
    end method.   
        
    // hidden source keys are only used for create
    method public abstract logical Assign( input phDbfield as handle ).
    
    // this is here to be called from Prepare override..  
    method protected integer GetNextValue(phDbfield as handle):
        define variable hQuery as handle no-undo.
        define variable hBuffer as handle no-undo.
        define variable inumber as integer no-undo.
        define variable cExpression as character no-undo.
        create buffer hBuffer for table phDbfield:buffer-handle.
        create query hQuery.
        
        hQuery:add-buffer(hBuffer).
        cExpression = GetPrepareWhereExpression().
        hQuery:query-prepare(subst("for each &1&2 by &3 desc",
                                    hBuffer:name,
                                    if cExpression > "" then subst(" where &1",cExpression) else "",
                                    phDbField:name
                                    )
                            ).
        hQuery:query-open().
        hQuery:get-first().
        if hBuffer:avail then   
            iNumber = hBuffer:buffer-field(phDbField:name):buffer-value.
        return iNumber + 1.
        finally:
            delete object hBuffer.
            delete object hQuery.        
        end finally.
    end.
    
end class.