
/*------------------------------------------------------------------------
    File        : UpdateDataResponse
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : 2/19/2024
    Notes       : 
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.BusinessLogic.IDataResponse from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataResponse from propath.
using Pmfo.Core.Common.GetDataMessages from propath.
using Pmfo.Core.Common.IGetDataMessages from propath.

class Pmfo.Core.BusinessLogic.UpdateDataResponse implements IUpdateDataResponse:  
    define public property Messages as IGetDataMessages  no-undo 
        get():
            if not valid-object(Messages) then 
                Messages = new GetDataMessages().
            return Messages.    
        end.    
        protected set.      
end class.