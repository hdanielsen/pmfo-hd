 
 /*------------------------------------------------------------------------
    File        : DynamicDataSource
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 25 19:14:21 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.DataLayer.DataSource.

block-level on error undo, throw.

class Pmfo.Core.DataLayer.DynamicDataSource inherits DataSource: 
    define public override property EntityName as char no-undo get. protected set.
    
    define private variable AssignedFieldMap as character no-undo.
        
    constructor public DynamicDataSource ( input pcBuffer as char, pcFieldmap as char ):
        this-object(pcBuffer).
        AssignedFieldMap = pcFieldMap.
    end constructor.
    
    constructor public DynamicDataSource ( input pcBuffer as char):
        super (GetDbBuffer(pcBuffer)).
        EntityName = BufferHandle:serialize-name. 
    end constructor.
    
    method public override character GetFieldMap():
        return AssignedFieldMap.
    end method.
    
    method protected handle GetDbBuffer(pcname as char):
        define variable hBuffer as handle no-undo.
        create buffer hBuffer for table pcname.
        return hBuffer.
    end method.    
end class.