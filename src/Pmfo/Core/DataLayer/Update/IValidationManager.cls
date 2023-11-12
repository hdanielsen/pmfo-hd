
/*------------------------------------------------------------------------
    File        : IValidationManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Aug 04 16:23:02 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.DataLayer.Update.IDatasetUpdateManager from propath.
using Pmfo.Core.DataLayer.ValidationHookEventType from propath.

interface Pmfo.Core.DataLayer.Update.IValidationManager inherits IDatasetUpdateManager:  
    define public property EventType as ValidationHookEventType no-undo get. 
        
end interface.