 
 /*------------------------------------------------------------------------
    File        : DataObjectService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Mar 16 20:46:08 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.SemanticVersion from propath.
using OpenEdge.Net.HTTP.MethodEnum from propath.
using OpenEdge.Web.DataObject.DataObjectService from propath.
using OpenEdge.Web.DataObject.MappedOperation from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.BusinessEntityOperation from propath.
using Pmfo.Core.Service.CatalogOperation from propath.
using Pmfo.Core.Service.ServiceEntityOperation from propath.

class Pmfo.Core.Service.DataObjectService inherits DataObjectService: 

    constructor public DataObjectService ( input pName as character ):
        super (input pName, new SemanticVersion()).    
    end constructor.
        
    constructor public DataObjectService ( input pName as character, input pVersion as SemanticVersion ):
        super (input pName, input pVersion).
    end constructor.

    method override public MappedOperation GetOperation( input pServiceURI as character, input pMethod as MethodEnum ):
        define variable targetName as character no-undo.
        
        targetName = GetTargetName(pServiceURI).
        if targetName = "catalog" then 
            return new CatalogOperation(pServiceURI,pMethod,targetName).
        else if cast(Application:ServiceManager,IServiceManager):CanFindPublicServiceEntity(input targetName) then
            return new ServiceEntityOperation(pServiceURI,pMethod,targetName).
        
        // the BusinessEntityOperationHandler:execute will throw error if this target is not supported 
        // If support for file or .gen or .map is needed add a check for IBusinessEntity before
        // returning this and call super for all other cases 
        return new BusinessEntityOperation(pServiceURI,pMethod,targetName).
    end method.
    
    method protected character GetTargetName(input pServiceURI as char):  
        define variable cTargetName as character no-undo.
        define variable iLeftParenthesis as integer no-undo.
        assign 
            cTargetName      = entry(1,pServiceURI,"/")
            iLeftParenthesis = index(cTargetName,"(")
            .
        if iLeftParenthesis > 0  then 
            return substr(cTargetName,1,iLeftParenthesis - 1).
        else   
            return cTargetName.
    end method. 

end class.