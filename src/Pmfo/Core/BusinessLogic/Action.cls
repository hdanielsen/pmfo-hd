 
 /*------------------------------------------------------------------------
    File        : Action
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Aug 17 08:47:23 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.DataTypeHelper from propath.
using Pmfo.Core.BusinessLogic.ActionScopeEnum from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Common.ISignature from propath.
using Pmfo.Core.Common.Method from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Service.IClientUpdateRequest from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.BusinessLogic.MethodBindingEnum from propath. 
using OpenEdge.Core.Assert from propath.

class Pmfo.Core.BusinessLogic.Action inherits Method implements IAction: 
    define public override property BaseTypeName  as character no-undo init "Action" get.
    
    //  return @success if successful. Row type is currently used to create message text
    define public property SuccessRowState as RowStateEnum no-undo get. set.
    define variable mIsCreateFalse as logical no-undo. // log if actively set to false  
    define variable mIsCreateTrue  as logical no-undo. // log if actively set to true  
    define public property ActionScope as ActionScopeEnum no-undo
        get():
            if not valid-object(ActionScope) then
                return ActionScopeEnum:Row.
            return ActionScope.
        end.
        set. 
    
    define public override property Binding as MethodBindingEnum no-undo 
        get():
            if not valid-object(Binding) then 
                // Default any - both single entoty and collection
                return MethodBindingEnum:Any.
            return Binding.    
        end. 
        set(pBinding as MethodBindingEnum):
            if isCreated then 
                undo, throw new IllegalOperationError("Cannot set Binding after the action has been invoked").
            //  as of current no check for this
            //  - Could possibly be sued to disallow keys in URL if that has any value
            //    but we cannot really require more than one record in the body 
            //   (very rare (if ever?) that an action requires more than one recordand so probably no point in handling this way)
            //    disallowing an object probably does not make sense either if thearray can have only one      
            if MethodBindingEnum:Collection:Equals(pBinding) then 
                undo, throw new NotImplementedError("Action Binding to Collection").
                
            if mIsCreateTrue then
                 undo, throw new IllegalOperationError("Cannot set Binding to Entity when isCreate has been actively set to true"). 
            
            Binding = pBinding.    
        end set.     
        
    // optional CSV of temp-table (entity) field names to use to check optimistic locking. 
    // This allows request to post a subset of data  
    define public property CompareFields as character no-undo get. set. 
    
     // optional set to true if the action requires data in before image format
    // for example for action with child data with uodates    
    define public property BeforeImageRequired as logical no-undo get. set. 
   
     // Set to true for actions that have new data that is to be created (default false)   
    define public property IsCreate     as logical no-undo 
        get. 
        set(plCreate as logical):
          
            // throw error if setting to false ans isEmpty is true
            mIsCreateFalse = plCreate = false.
            mIscreateTrue  = plCreate = true.
            isCreate =plCreate.
            AssertIsCreate().
            
        end.
        
     // Set to true for actions that is completely handled by the BE - no json and no key in url    
    define public property IsEmpty as logical no-undo 
        get. 
        set(plEmpty as logical):
            if plEmpty then
            do:
                if mIsCreateFalse then
                     undo, throw new IllegalOperationError("Cannot set IsEmpty true when isCreate has been actively set to false"). 
                IsCreate = true.
            end.
            IsEmpty = plEmpty.
        end.
        
    define public property ParameterList    as ParameterList     no-undo get. set.
    
    // det to true for primitive response
    define public override property IsPrimitive as logical no-undo  get. set.
     
     // Return true if the action is nested to another action - the table must be in the dataset
  //  define public property HasChild        as logical     no-undo get. 
    // An action can be defined to nest another action - the table must be in the dataset
 //   define public property Children        as ICollection no-undo get.
    
    // identify the target of the action with the unique table name in the submitted dataset
    // There can be more than one table to the same resource (in theory)  
    define public property TargetTableName as character   no-undo get. protected set.
    
    // for the BE we have one more parameter for the updateRequest   
    define protected property ClientParameterList    as ParameterList no-undo get. set.
    
    define protected property ClientMethod        as Progress.Reflect.Method  no-undo get. protected set.
    
    constructor public Action ( input pName as character, input pSignatures as ISignature extent ):
        super (input pName, input pSignatures).
    end constructor.
        
    constructor public Action ( input pName as character, input pSignatures as ISignature extent, input plObject as logical ):
        super (input pName, input pSignatures, input plObject).
    end constructor.

    constructor public Action ( input pName as character ):
        super (input pName).
    end constructor.
    
    constructor public Action ( input pName as character, pServerType as ServerTypeEnum ):
        super (input pName, pServerType).
    end constructor.

    constructor public Action ( input pName as character, pScope as ActionScopeEnum):
        this-object(input pName).
        ActionScope = pScope.
    end constructor.
    
    constructor public Action ( input pName as character, input pParams as IParameter extent):
        super (input pName, input pParams).
       // AssertParameters(pParams).
    end constructor.
    
    constructor public Action ( input pName as character, input pParams as IParameter extent,pServerType as ServerTypeEnum):
        super (input pName, input pParams, pServerType).
    //    AssertParameters(pParams).
    end constructor.
    
    constructor public Action ( input pName as character, input pParams as IParameter extent, pScope as ActionScopeEnum):
        this-object (input pName, input pParams).
        ActionScope = pScope.
    end constructor.
    
    constructor public Action ( input pName as character, input pParam as IParameter):
        define variable cMask as character no-undo.
        super (input pName, input pParam).
       // AssertParameter(pParam).
    end constructor.
    
    // override to make NoContentResponse default to true when SuccesRowState is defined and NoContentResponse is not set 
    method protected override logical GetNoContentResponseDefault ():
        return valid-object(SuccessRowState). 
    end method.
    
    method protected void AssertIsCreate():
        if IsCreate = false and IsEmpty = true then 
            undo, throw new IllegalOperationError("Cannot set IsCreate false when IsEmpty is true").
        // the Binding to Entity is used to allow key in URL and intended for existing records 
        // if we ever need to limit the number of records to ONE for create we shoild use another property 
        if IsCreate = true and MethodBindingEnum:Entity:Equals(Binding) then 
            undo, throw new IllegalOperationError("Cannot set IsCreate true when Binding to a single Entity").
    end method.
    
    /*   
    method protected void AssertParameter(input pParam as IParameter) : 
        if pParam:IsArray then
            undo, throw new UnsupportedOperationError(subst("Define of Action parameter &1 as extent",pParam:name)).
    end method.    
    
    method protected void AssertParameters(input pParams as IParameter extent) : 
        define variable i as integer no-undo.
        do i = 1 to extent(pParams): 
            AssertParameter(pParams[i]).
        end.    
    end method.    
    */
    method protected override void AssertSignature(input pSignature as ISignature ) : 
        define variable i as integer no-undo.
        super:AssertSignature(pSignature).
      //  if extent(pSignature:Parameters) > 0 then 
        //    AssertParameters(pSignature:Parameters).  
    end method.    
    
    constructor public Action ( input pName as character, input pParam as IParameter,pServerType as ServerTypeEnum):
        super (input pName, input pParam, pServerType).
    end constructor.
    
    constructor public Action ( input pName as character, input pParams as IParameter, pScope as ActionScopeEnum):
        this-object (input pName, input pParams).
        ActionScope = pScope.
    end constructor.
    
    method public RequestTypeEnum PrepareClientRequest(pcMainTable as character, pClientRequest as IClientRequest ):
        define variable oRequestType as RequestTypeEnum no-undo.
        define variable cUsedALiases as character no-undo.    
        oRequestType = if pClientRequest:KeyWheres[extent(pClientRequest:KeyWheres) - 1] = ""
                       then RequestTypeEnum:SingleTopTable  
                       else RequestTypeEnum:SingleRow. 
     
        pClientRequest:AssertUriQuery(oRequestType).
       // throws BadRequestError if isPAramtersRequired and keywheres = ""     
       // must be called also with no parameters to set TargetTableName 
        cUsedALiases = SetParameterExpression(pcMainTable,pClientRequest:KeyWheres[extent(pClientRequest:Collections)], if pClientRequest:HasAlias then pClientRequest:Aliases else ?).  
                
        return oRequestType.        
    end method.
    
    method protected character SetParameterExpression(pcTargetTableName as character, pcExpression as character, pAliasMap as IStringStringMap ):
         define variable cUsedAliases as character no-undo.
     // TODO thows error immediately     
/*        if valid-object(ParameterList) then*/
/*            undo, throw new IllegalOperationError(subst("The Parameter values are already set in Action &1",name)).*/
        
        if pcExpression > "" then
            cUsedAliases = SetParameterExpression(pcExpression,pAliasMap).
        else if isParametersRequired() then
            undo, throw new BadRequestError(subst("Missing parameter(s) for action &1",name)).    
        else 
            PostSetValues(""). // need to signal done for no parameters       
        TargetTableName = pcTargetTableName.
        return cUsedAliases. 
    end method. 
    
    method public void SetParameterExpression(pcTargetTableName as character, pcExpression as character):
        SetParameterExpression(pcTargetTableName,pcExpression,?).    
    end method. 
    
    method public void SetParameters(pcTargetTableName as character, pParameters as IStringStringMap).
    // TODO
/*        if valid-object(ParameterList) then                                                                        */
/*            undo, throw new IllegalOperationError(subst("The Parameter values are already set in Action &1",name)).*/
/*                                                                                                                   */
         
        if pParameters:Size > 0 then
            SetParameters(pParameters,false). // false -not quoted - todo add data type checks?? 
                                              // Since it is used internally it is probably not needed 
         else if isParametersRequired() then
            undo, throw new BadRequestError(subst("Missing parameter(s) for action &1",name)).        
       
        TargetTableName = pcTargetTableName. 
        
    end method.
    
    method protected override integer PrepareParameters(piNum as integer).
        if valid-object(ParameterList) then delete object ParameterList.
        // add one extra for the dataset 
        if piNum = ? then piNum = 0.    
        ParameterList = new ParameterList(piNum + 1).
        // one more for the client for the updateRequest
        ClientParameterList = new ParameterList(piNum + 2). 
        return 1. // start adding params from here 
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as character):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as longchar):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue). 
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).       
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as integer):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as int64):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).  
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as decimal):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as logical):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as date):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as datetime):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValue as datetime-tz):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValue).       
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as character extent):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as longchar extent):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues). 
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).       
    end method.
    
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as integer extent):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as int64 extent):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).  
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as decimal extent):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as logical extent):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as date extent):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as datetime extent):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).       
    end method.
 
    method protected override void SetValue(pParameter as IParameter, piPosition as integer, pValues as datetime-tz extent):
        ParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).
        ClientParameterList:SetParameter(piPosition, DataTypeHelper:GetMask(pParameter:DataType), "input", pValues).       
    end method.
    
    method public Progress.Reflect.Method GetClientMethod(pObject as Object, poUpdateDataRequest as IUpdateDataRequest, phDataset as handle,output pClientParameterList as ParameterList):
        define variable poClientParameterList as ParameterList no-undo.
        if not valid-object(ClientMethod) then
        do:
            if not valid-object(ClientParameterList) then
                ClientParameterList = new ParameterList(2).
            
            ClientParameterList:SetParameter(ClientParameterList:NumParameters - 1, get-class(IUpdateDataRequest):TypeName, "input", poUpdateDataRequest).
            ClientParameterList:SetParameter(ClientParameterList:NumParameters, "dataset-handle", "input by-reference", phDataset).
             
            ClientMethod = pObject:GetClass():GetMethod(Name,Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance, ClientParameterList).
        end.              
        if valid-object(ClientMethod) then
            pClientParameterList = ClientParameterList.
   
        return ClientMethod.
        
    end method.    
    
    // allow control of row-state and field values after find using keys in url   
    method public void PostFindUnique(pRequestedBuffer as handle).
        define variable ifld as integer no-undo.
        define variable hFLs as handle no-undo. 
        define variable hFld as handle no-undo.
        
        if MethodBindingEnum:Entity:Equals(Binding) = false then
        do:
            // the default is for consistency with action behavior and may not make sense
            // override to change this behavior   
            do iFld = 1 to pRequestedBuffer:num-fields:
                hFld = pRequestedBuffer:buffer-field(iFLd).
                if lookup(hFld:name,CompareFields) = 0 then
                    hFld:buffer-value() = hFld:default-value. 
            end.
            pRequestedBuffer:mark-row-state (row-created).
        end.
        
    end method.
    
    // see interface 
    method public void RemoveClientMethod():
        // ClientParamterList will have reference to the UpdateDataRequest.
        // This is called after the method returned from GetClientMethod has been used to ensure GBC working as expected 
        if valid-object(ClientParameterList) then
            delete object ClientParameterList.
        if valid-object(ClientMethod) then
            delete object ClientMethod.
        
    end method.
     
end class.