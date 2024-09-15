
/*------------------------------------------------------------------------
    File        : IActionScope
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Aug 17 08:52:50 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/
 
using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.BusinessLogic.ActionScopeEnum from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.

interface Pmfo.Core.BusinessLogic.IAction inherits IMethod:  
    /*
     Row - calls the action for each posted row/record
     Table - calls the action once assuming the action does a for each
    */
    define public property ActionScope as ActionScopeEnum no-undo get. set.
    
    // Actions data currently have row-state row-created, but it is assumed to contain 
    // existing data that will be checked accordingly (record exists and optimistic lock check)  
    // Set this to true for actions that have new data that is to be created   
    define public property IsCreate        as logical no-undo get. set.
    
    // Set to true for actions that is completely handled by the BE - no json and no key in url    
    define public property IsEmpty as logical no-undo get. set.
    
    // optional CSV of temp-table (entity) field names to use to check optimistic locking. 
    // This allows request to post a subset of data  
    define public property CompareFields as character no-undo get. set. 
    
    // optional set to true if the action requires data in before image format
    // for example for action with child data with uodates    
    define public property BeforeImageRequired as logical no-undo get. set. 
    
    //  return @success array if successful. Row type is currently just used to create a default message text
    //  NoContentResponse defaults to true if this is defined.  
    define public property SuccessRowState as RowStateEnum no-undo get. set.
    
    // Return true if the action is nested to another action - the table must be in the dataset
 //   define public property HasChild        as logical     no-undo get. 
    // An action can be defined to nest another action - the table must be in the dataset
//    define public property Children        as ICollection no-undo get.
    // identify the target of the action with the unique table name in the submitted dataset
    // There can be more than one table to the same resource (in theory)  
    define public property TargetTableName as character   no-undo get. 
    
    // current request parameters 
    define public property ParameterList    as ParameterList     no-undo get. set.
    
    // prepare - Calcalate RequestYpe - AssertUriQuery in the request and setPAramterExpression   
    method public RequestTypeEnum PrepareClientRequest(pcTargetTableName as character, plCientRequest as IClientRequest ).
        
    method public void SetParameterExpression(pcTargetTableName as character, pcExpression as character).
    method public void SetParameters(pcTargetTableNAme as character, pParameters as IStringStringMap).
     
    method public Method GetClientMethod(pObject as Object, poUpdateDataRequest as IUpdateDataRequest, phDataset as handle, output pParameterList as ParameterList ).
    // the client method ClientParamterList will have reference to the UpdateDataRequest so we must allow it to be removed to ensure GBC working as expected
    // Both the UpdateRequest and the Action (may) need to exist for a while after and it is difficult to have safe cleanup thayt works in allcases
    // so this ensures that whoever calls and uses GetClientMethod can get rid of this to not prevent future GBC      
    method public void RemoveClientMethod():
    // allow control of row-state and field values after find for keys in url   
    method public void PostFindUnique(pRequestedBuffer as handle).
    
    
        
 end interface.