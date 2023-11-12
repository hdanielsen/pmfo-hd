
/*------------------------------------------------------------------------
    File        : IUpdateDataEventArgs
    Purpose     : Parameter to DataUpdate event subscription 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fir Sep 27 12:33 2019
    Notes       : There is only one property, but we use the args 
                  event pattern to allow this to be extended without
                  changing the event signature 
                - and as you can see.... we have added a lot of stuff  
  ----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataEventArgs from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.

class Pmfo.Core.BusinessLogic.UpdateDataEventArgs implements IUpdateDataEventArgs:
    define public property Request          as IUpdateDataRequest  no-undo get. protected set.
    define public property Owner            as IBusinessEntity     no-undo get. protected set.
    define public property Dataset          as handle              no-undo get. protected set.
    define public property Parent           as IBusinessEntity     no-undo get. set.
    define public property ParentName       as character           no-undo get. set.
  // do we need a name for cases where a table is defined like another to be ble to be twice
  // in a dataset? And if you do that would you create a IRelation with this as parent name
  // (which seems to be the only case a child may care  - see NoteBE validateTable)  
  // The curent thinking is that this relation should be defined in the class backed by the table
  //  and not defined in the owning class 
  //  define public property PublicParentName       as character           no-undo get. set.
  //
    define public property PublicName       as character           no-undo get. set.
    define public property Join             as character           no-undo get. set.
      
    constructor public UpdateDataEventArgs(pOwner as IBusinessEntity, pRequest as IUpdateDataRequest, phbuffer as handle):
        if not valid-handle(phBuffer) then 
            undo, throw new IllegalArgumentError("Buffer is not a valid handle").
        
        if phBuffer:type <> "Buffer" then 
             undo, throw new IllegalArgumentError(subst("Invalid buffer type &1",phBuffer:type)).
        Request = pRequest.
        PublicName = phbuffer:serialize-name. // this will be changed for children
        this-object:Dataset = phbuffer:dataset.
        Owner = POwner. 
    end constructor.
        
end class.