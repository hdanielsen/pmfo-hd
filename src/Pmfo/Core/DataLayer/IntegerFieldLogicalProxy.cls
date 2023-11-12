 /*------------------------------------------------------------------------
    File        : IntegerFieldLogicalProxy
    Purpose     : Expose a logical field as integer in order to support 3 values (true,false,?)   
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Oct 10 6.52 EDT 2023
    Notes       : Should noirmally be accompanied by a lookup that has text for the 3 values
  ----------------------------------------------------------------------*/

using Pmfo.Core.DataLayer.CustomFieldProxy.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.BufferUtil from propath.

block-level on error undo, throw.

class Pmfo.Core.DataLayer.IntegerFieldLogicalProxy inherits CustomFieldProxy:  

    define override protected property StringExpression as character no-undo 
    get():
        return subst("(if &1 eq true then 1 else if &1 = false then 0 else ?)",SourceFullName).
    end. 
    
    constructor public IntegerFieldLogicalProxy (pcFieldName as character, pcSourceFieldName as character):
        super (pcFieldName,pcSourceFieldName).
    end constructor.
    
    // true means different/changed
    method override public logical Conflict(phField as handle ):
        if phField:buffer-value = 1 then 
            return SourceField:buffer-value <> true. 
        else if phField:buffer-value = 0 then 
            return SourceField:buffer-value <> false. 
        else    
            return SourceField:buffer-value <> ?. 
    end method.

    method override public logical Save(  ):
        define variable iOld as integer no-undo.
        iOld = SourceField:buffer-value.
        if this-object:Field:buffer-value = 1 then
            SourceField:buffer-value = true.
        else if this-object:Field:buffer-value = 0 then
            SourceField:buffer-value = false.
        else if this-object:Field:buffer-value = ? then
            SourceField:buffer-value = ?.
        else 
            undo, throw new IllegalArgumentError(subst("&1 cannot be set to &2",BufferUtil:GetFieldLabel(this-object:Field),this-object:Field:buffer-value )).
        // true if changed
        return iOld <> SourceField:buffer-value .        
    end method.
    
    method override void  SetBufferValue(pValue as logical):
         this-object:field:buffer-value = if pValue = true then 1 else if pValue = false then 0 else ? . 
    end method.     
    
end class.