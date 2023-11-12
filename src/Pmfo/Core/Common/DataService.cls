 
 /*------------------------------------------------------------------------
    File        : DataService
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 25 19:05:04 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Application from propath.
using Pmfo.Core.Common.IDataService from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.AssertReadOnlyError from propath.
using Pmfo.Core.Manager.StartupManager from propath.
using Pmfo.Util.AssertUtil from propath.

class Pmfo.Core.Common.DataService abstract implements IDataService : 
    
	constructor static DataService (  ):
        if not valid-object(Application:StartupManager) then 
        do on error undo, throw:
            StartupManager:Instance. /* sets Application:StartupManager in contructor */
            // an error here is rather confusing, so clarifiy a little
            catch e as Progress.Lang.Error :
                undo, throw new ApplicationError(subst("DataService static constructor error from StartupManager:Instance - &1",e:GetMessage(1)),e).    
            end catch.
        end.        
        
	end constructor.
    
    method public void dispose(  ).
        Destroy().
    end.
    
    // throw error if property is not called from constructor or its own getter
    method public void AssertConstructorProperty ():
        AssertUtil:AssertConstructorProperty().
    end method.
    
    method public abstract void Destroy(  ).
    method public abstract void Initialize(  ).
 
end class.