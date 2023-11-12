
/*------------------------------------------------------------------------
    File        : IReceiveDataEventArgs
    Purpose     : Parameter to DataReceived event subscription 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Jul 27 11:18:12 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.

interface Pmfo.Core.BusinessLogic.IReceiveDataEventArgs:  
    define public property Request      as IGetDataRequest  no-undo get.
    define public property Response     as IGetDataResponse no-undo get.
    define public property Failed       as logical          no-undo get. set.
     
end interface.