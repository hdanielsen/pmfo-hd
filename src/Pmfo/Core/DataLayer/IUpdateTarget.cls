
/*------------------------------------------------------------------------
    File        : IUpdateTarget
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Jul 31 20:29:05 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.DataLayer.ValidationHookEventType from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.

interface Pmfo.Core.DataLayer.IUpdateTarget inherits IDataSource:  
    /* Prepare by attaching the data source to the DataSourceHandle 
    */  
   // method public logical PrepareSave(phBuffer as handle).
    method public logical PrepareSave(phBuffer as handle, pUpdateRequest as IUpdateDataRequest).
    
    /* public api to allow a TransactionManager fire all the Pre and Post hooks
       before and after the transaction 
       Also used internally to fire the Begin and End hooks in the transaction
    */
    method public void  ValidateRow(phBeforeBuffer as handle, pEventType as ValidationHookEventType).
    method public void  ValidateRowAction(phBuffer as handle, pAction as IAction, pEventType as ValidationHookEventType).
    
    /* public api to save the data - called from a TransactionManager  */
    method public logical SaveRow(phBeforeBuffer as handle).
    
    /* DataAccess subscribes this to the Beforeupdate event fired before the update starts
       This fires outside the transaction
       Subclasses overrides this if they need to do something that cannot 
       be handled in the row level events 
       The override must do a for each to get the data   
    */ 
    method public void  BeforeUpdate (input dataset-handle phDataset).
    
    /* DataAccess subscribes this to the Afterupdate event fired when the update ends
       but before refresh of data
       This fires outside the transaction
       Subclasses overrides this if they need to do something that cannot 
       be handled in the row level events 
       The override must do a for each to get the data   
    */ 
    method public void  AfterUpdate (input dataset-handle phDataset).
    
    // TransactionManager will fire this if it finds an override in 
    // the concrete class
    // An error will be thrown if the CommitScopeEnum is not ALL 
    method public void DatasetTransactionEnd( input dataset-handle phDataset ).
    
    method public Method GetActionMethod(pAction as IAction,input phDataset as handle,output pParameterList as ParameterList).
    
    // allow target to defien action with table scope to be called by ExecuteActionAsTransaction
    // needed for actions that may have infrastructure data (must be saved in same transaction)
    method public character GetTransactionTableActions().
    
    method public void FindSourceForUnmodifiedRow().
     
    method public void FindSource(plock as integer).
    method public void FindSource(phbuffer as handle,pDbBuffer as handle,plock as integer).
    
    method public void FindAndCompareSource().
    method public void FindAndCompareSource(phBuffer as handle).
    method public void FindAndCompareSource(phbuffer as handle,pFields as character).
    
        
end interface.
