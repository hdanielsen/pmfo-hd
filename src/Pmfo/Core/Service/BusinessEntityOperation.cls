 
 /*------------------------------------------------------------------------
    File        : BusinessEntityOperation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Oct 13 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Net.HTTP.MethodEnum from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.Service.MappedOperation from propath.

class Pmfo.Core.Service.BusinessEntityOperation inherits MappedOperation: 
    
    constructor public BusinessEntityOperation ( pServiceURI as character, pMethod as MethodEnum, pTargetName as char):
        super (input pServiceURI, 
               input pMethod, 
               input get-class(IBusinessEntity),
               input pTargetName).
       
    end constructor.
    
  
end class.