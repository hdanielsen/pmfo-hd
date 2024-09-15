 
 /*------------------------------------------------------------------------
    File        : ResourceReader
    Purpose     : Incomplete
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Jan 22 16:02:42 EST 2024
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using Pmfo.Repository.Business.ResourceBE from propath.
using Pmfo.Core.Common.IWarningMessageTableHolder from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Ccs.Common.Application from propath.
 
using Progress.Lang.Object from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.IIterator from propath.

block-level on error undo, throw.

class Pmfo.Tools.Util.ResourceReader: 
   
    {pmfo/Repository/schema/entity.i}
    
     define protected property ServiceManager as IServiceManager no-undo 
       get():
            return cast(Application:ServiceManager,IServiceManager).
       end.
 
        
    method void CheckNotes():  
        define variable hDataset as handle no-undo.
        define variable oResourceBe as ResourceBE   no-undo.
        
        oResourceBe = new ResourceBE().
        oResourceBE:GenerateAndGetData(output dataset-handle hDataset). 
    end.    
    
    method protected void CheckNotes(input table ttEntity):
        define variable oBE as IBusinessEntity no-undo.
        define variable oObject as Object no-undo.
         
        for each ttentity: 
            oBE = ServiceManager:CreateBusinessEntity(ttentity.Resource).
            oBE:Links.
            
            
        end.
    end.    
    
    
     
   
end class.