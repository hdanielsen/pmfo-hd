/*------------------------------------------------------------------------
    File        : IUpdateTargetManager
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Aug 04 11:23:43 EDT 2019
    Notes       : Common properties for validation and transaction managers
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.CommitScopeEnum from propath.
using OpenEdge.Core.Collections.IMap from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.

interface Pmfo.Core.DataLayer.Update.IUpdateTargetManager:  
    define public property UpdateTargets as IHandleDataSourceMap no-undo get. 
    define public property CommitScope   as CommitScopeEnum no-undo get.   
    
  
end interface.