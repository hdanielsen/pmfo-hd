 
 /*------------------------------------------------------------------------
    File        : SuccessMessage
    Purpose     : Pseudo collection of sucess messages for use by BusinessEntiy during Update request
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri May 13 15:43:33 EDT 2022
    Notes       : Very targeted and short time usage so did not use List or Collection 
                  This can be changed. 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.Common.ISuccessMessage from propath.
using Pmfo.Core.Common.ISuccessMessages from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Json.ObjectModel.JsonArray from propath.

class Pmfo.Core.Common.SuccessMessages implements ISuccessMessages: 

    define protected property Messages as ISuccessMessage extent get. set.
     
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    constructor public SuccessMessages ():
        super ().
    end constructor.
    
    method public logical Add(posuccess as ISuccessMessage):
        extent(Messages) = (if extent(Messages) = ? then 0 else extent(Messages)) + 1.
        Messages[extent(Messages)] = poSuccess. 
    end method.      
    
    method public JsonArray ToJsonArray( ):
        define variable oJsonArray as JsonArray no-undo.
        define variable i as integer no-undo.
        oJsonArray = new JsonArray().
        do i = 1 to extent(Messages): 
           oJsonArray:add(Messages[i]:ToJson()).
        end.
        return oJsonArray.
    end method.      
 
     
end class.