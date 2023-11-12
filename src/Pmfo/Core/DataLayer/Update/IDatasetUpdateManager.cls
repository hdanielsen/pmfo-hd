
/*------------------------------------------------------------------------
    File        : IDatasetUpdateManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Aug 04 11:38:13 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Pmfo.Core.DataLayer.Update.IUpdateTargetManager from propath.

interface Pmfo.Core.DataLayer.Update.IDatasetUpdateManager inherits IUpdateTargetManager:  
    /* Set to true to only delete the current processed table (in ProcessTable)
       - This must be set to true for CommitScopeEnum:Table to work 
         Note that this option prevents cascading deletes to be done in 
         the dataset buffers, since the parent is processed before the child.
       False (default) do all deletions top-down before any otherD
       operations 
       
       */
    define public property ShallowDelete as logical   no-undo get.
end interface.