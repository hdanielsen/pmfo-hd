
/*------------------------------------------------------------------------
    File        : IManagedDataValidationMessages
    Purpose     : Collects validation messages and keys with the purpose of  
                  showing them in the UI and allowing them to be persisted
                  as approved and asserted or completed later 
                  
                  The goal is to ensure that the same error data is recreated
                  each time, so that the message and keys returned to the UI
                  and stored on approval can be compared when the same conditions 
                  are checked on POST requests to assert only or assert and commit
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu May 02 06:28:34 EDT 2024
    Notes       :  
  ----------------------------------------------------------------------*/

 
using OpenEdge.Core.Collections.ISet from propath.
using Pmfo.Core.BusinessLogic.IRequest from propath.
using Pmfo.Core.BusinessLogic.ManagedDataValidationRequestTypeEnum from propath.
using Pmfo.Core.Service.IManagedDataValidationLogService from propath. 

interface Pmfo.Core.BusinessLogic.IManagedDataValidationMessages:  
    define public property DataValidationId as integer   no-undo get.  
    define public property EntityName       as character no-undo get.  
    define public property KeyName          as character no-undo get.  
    define public property PublicKeyName    as character no-undo get.  
    
    define public property MessageFooter as character no-undo get. set.
    define public property MessageHeader as character no-undo get. set. 
    define public property Keys          as ISet      no-undo get.  
        
    define public property HasMessage as logical no-undo get.  
    
    method public void ProcessRequest(
        input pValidationRequestType as ManagedDataValidationRequestTypeEnum, 
        input poRequest              as IRequest,
        input pcAction               as character,
        input pValidationLogService  as IManagedDataValidationLogService,
        input phHandle               as handle):
    
    method public void Add(pKey as integer,pMessage as character).
    // allow adding one or more keys after 
    method public void Add(pKey as integer).
    
    // use when keys does not need to be logged since they will be extracted from the buffer passed to ProcessSuperOverride
    method public logical Add(pMessage as character).
    
    // full message with header and footer   
    method public character GetMessage(). 
    // no explanatory footer when thrown as error 
    method public character GetErrorMessage().  
    
end interface.