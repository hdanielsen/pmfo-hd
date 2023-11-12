 
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
    
    method public abstract logical Assign( input phDbfield as handle ).
    
end class.