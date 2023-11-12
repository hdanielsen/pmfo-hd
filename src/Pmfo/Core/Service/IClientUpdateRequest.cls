
/*------------------------------------------------------------------------
    File        : IClientRequest
    Purpose     : Builds GetDataRequest from dataset handle using public names
    Syntax      : 
    Description : Defines a data request from public names   
    Author(s)   : hdaniels
    Created     : Sun Mar 31 15:46:35 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.

interface Pmfo.Core.Service.IClientUpdateRequest inherits IClientRequest:  
    define public property ConfirmationsProp  as character  no-undo get.  
    define public property Confirmations      as character  no-undo get. 
    // json body with only keys 
    define public property KeysOnly           as logical    no-undo get. 
      
end interface.