
/*------------------------------------------------------------------------
    File        : IValidateUpdateRequestPlugin
    Purpose     : Intgerface for plugin for IUpdateDataRequest validation    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 12 20:13:11 EDT 2023
    Notes       : Added as plugin instead of overriable hook in case we get default behavior so that subclasses does not neen to call super 
    
                  Called after DatasetUpdateManager has started BE so that 
                  we can use GEtEx
                   
  ----------------------------------------------------------------------*/

 
interface Pmfo.Core.BusinessLogic.IValidateUpdateRequestPlugin:  
    method public void Validate (poRequest as Pmfo.Core.BusinessLogic.IUpdateDataRequest, phDataset as handle).
end interface.