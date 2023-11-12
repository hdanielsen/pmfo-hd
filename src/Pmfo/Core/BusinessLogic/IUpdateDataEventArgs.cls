
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
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.

interface Pmfo.Core.BusinessLogic.IUpdateDataEventArgs:
    // the entity that receives the update request from a client  
    define public property Owner            as IBusinessEntity     no-undo get.
    
    // the dataset of the update request  
    define public property Dataset          as handle              no-undo get.
    
    // The parent entitiy in the case of a relation 
    // NOTE: This may not be valid if the parent diesnot have any reason to be
    // started for an update 
    define public property Parent           as IBusinessEntity     no-undo get. set.
    
    // do we need a name for cases where a table is defined like another to be ble to be twice
    // in a dataset? And if you do that would you create a IRelation with this as parent name
    // (which seems to be the only case a child may care  - see NoteBE validateTable)  
    // The curent thinking is that this relation should be defined in the class backed by the table
    //  and not defined in the owning class 
    // The parent public name - the parent may be defioned like table in which case this 
    // is the name that is use in the relation 
    // dataset    
    //   define public property ParentPublicName as character           no-undo get. set.
 
    // the name of the parent - this is the tabl handle serialize-name that can be used 
    // to start the entoty if Parent object is not valid 
    define public property ParentName       as character           no-undo get. set.
    // the subscriber's name in the url and buffer name in dataset 
    define public property PublicName       as character           no-undo get. set.
    define public property Join             as character           no-undo get. set.
    define public property Request          as IUpdateDataRequest  no-undo get.
    
end interface.