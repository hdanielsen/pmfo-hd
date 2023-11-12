 
 /*------------------------------------------------------------------------
    File        : FieldValue
    Purpose     : Hoilds a field value detached from data buffer 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 13 12:42:52 EDT 2022
    Notes       : As of current there is no buffer handle
                  It is conceivable that a constgructor or methods may be added to pass a buffer
                  In that case thew buffer must not be stores since this object 
                  is to be used detached after the object that holds the buffer is garbage collected
                  or the buffer is out of scope.
                - This CAN be resolved by creating an internal private dynamic buffer, but that 
                  seems as overkill for this usage 
 ----------------------------------------------------------------------*/
  


block-level on error undo, throw.

using Ccs.Common.Support.IPrimitiveHolder from propath.
using Pmfo.Core.Common.IFieldValue from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.

class Pmfo.Core.Common.FieldValue implements IFieldValue: 

    define public property Name as character no-undo 
    get.
    protected set. 

    define public property Value as IPrimitiveHolder no-undo 
    get.
    protected set. 
    
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    constructor public FieldValue (pcName as character, poValue as IPrimitiveHolder ):
        super ().
        Name = pcName.
        this-object:Value = poValue.
    end constructor.
    
    constructor public FieldValue (pcName as character, pcValue as character ):
        this-object (pcName, PrimitiveHolderUtil:GetHolder(pcValue)).
    end constructor.
    
    constructor public FieldValue (pcName as character, plongValue as longchar):
        this-object (pcName, PrimitiveHolderUtil:GetHolder(plongValue)).
    end constructor.
       
    constructor public FieldValue (pcName as character, piValue as integer ):
        this-object (pcName, PrimitiveHolderUtil:GetHolder(piValue)).
    end constructor.
    
    constructor public FieldValue (pcName as character, piValue as int64 ):
        this-object (pcName, PrimitiveHolderUtil:GetHolder(piValue)).
    end constructor.
    
    constructor public FieldValue (pcName as character, pdValue as decimal):
        this-object (pcName, PrimitiveHolderUtil:GetHolder(pdValue)).
    end constructor.
    
    constructor public FieldValue (pcName as character, plValue as logical):
        this-object (pcName, PrimitiveHolderUtil:GetHolder(plValue)).
    end constructor.
    
    constructor public FieldValue (pcName as character, ptValue as date):
        this-object (pcName, PrimitiveHolderUtil:GetHolder(ptValue)).
    end constructor.
    
    constructor public FieldValue (pcName as character, ptValue as datetime):
        this-object (pcName, PrimitiveHolderUtil:GetHolder(ptValue)).
    end constructor.
    
    constructor public FieldValue (pcName as character, ptValue as datetime-tz):
        this-object (pcName, PrimitiveHolderUtil:GetHolder(ptValue)).
    end constructor.
    
        
end class.