 
 /*------------------------------------------------------------------------
    File        : DatasetCrudRelation
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Feb 9 2023
    Notes       : Extends DatasetRelation to disallow any function or action
                  and only use the DeactivateMethods to control CRUD
                        
 ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.DatasetRelation from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.

class Pmfo.Core.BusinessLogic.DatasetCrudRelation inherits DatasetRelation: 
    
    constructor public DatasetCrudRelation (phRelation as handle, pcDeactivateMethods as character extent):
        super(phRelation,pcDeactivateMethods).      
    end constructor. 
    
    method public override logical Deactivate(pRowState as RowStateEnum, pcServerMethod as character).
        if pcServerMethod > "" then
        do:
            return Deactivate().
        end.    
        else 
            return super:Deactivate(pRowState,pcServerMethod).
            
    end method. 
    
end class.    
    