
/*------------------------------------------------------------------------
    File        : IUpdateDataRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Jun 02 09:33:31 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IRequest from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.Common.IGetDataMessages from propath.

interface Pmfo.Core.BusinessLogic.IUpdateDataRequest inherits IUpdateDataRequest, IRequest :  
    define public property Confirmations  as character no-undo get.  
    define public property Messages       as IGetDataMessages  no-undo get. 
    /* Optional comma separated list of tables that do not need or do not have
       an update target - read-only - yse Add- and RemoveNoTargetTable to edit */
    define public property NoTargetTables      as character no-undo get.
    
    /* Optional comma separated list of tables that need optimistic locking 
       if submitted with no changes
       read-only - yse Add- and Remove to edit */
    define public property CheckReadOnlyTables      as character no-undo get.
    
    // mainly for actions with no Content reponse, but can be set manually 
    // when an updarte is called from an action with no content response and the 
    // action is not passed to the server or othjer cases where the data response is irrelevant  
    define public property NoRefresh           as logical no-undo get. set.
    
    /* convenience properties to sim0plify checking for action in hooks
      (An action is stored as a CustomRequeast  Object)    */
    define public property ActionName          as character no-undo get. 
    define public property Action              as IAction   no-undo get. 
      
    // add a table name to the no target table list
    method public logical AddNoTargetTable (pTablename as character).  
    // remove a table name from the no target table list
    method public logical RemoveNoTargetTable (pTablename as character).  
    
    // add a table name to the CheckReadOnlyTables list
    method public logical AddCheckReadOnlyTable (pTablename as character).  
    
    // remove a table name from the CheckReadOnlyTables list
    method public logical RemoveCheckReadOnlyTable (pTablename as character).  
    
    /* Return true if the buffer or table is an action target   
    */ 
    method public logical IsActionTarget(phHandle as handle).
    
    method public void RemoveAction().
   
    method public void ConvertActionToNamedQuery().
    method public void ConvertActionToNamedQuery(pcName as character).
    
    // get the numbers from the Confirmations for a specific entity name 
    method public character GetConfirmedQuestions  ( pcSourceEntityname as character).
    
    // Call by whoever (BE UpdateData) calls GetClientMethod that may create an ABL reflection parameterlist
    // in the ACtion (if this is an action request). The client parameterlist has a reference 
    // to this object that prevents garbage collection. 
    // The Action itself is added to this object in GetUpdateRequestDataset and whoever calls this
    // may need to use the action also after UpdataData, so we cannot delete the action itself
    // at that point, but we cannot rely on a caller that creates and used this-object and its action
     // does the clean up so whoever calls GetClientMethod must also call this
     method public void RemoveClientMethod().
end interface.