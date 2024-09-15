 /*------------------------------------------------------------------------
    File        : GetDataRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Apr 01 15:59:51 EDT 2019
    Notes       : Override in order to add GetTableRequest
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.INamedQuery from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.IList from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Common.GetDataQuestions from propath.
using Pmfo.Core.Common.IGetDataQuestions from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Ccs.BusinessLogic.INamedQueryParameter from propath.
using Pmfo.Util.StringUtil from propath.

class Pmfo.Core.BusinessLogic.GetDataRequest  implements IGetDataRequest: 
     define public property HasWarnings as logical no-undo get. protected set.
      
     define public property Warnings as IList no-undo 
         get():
             return Warnings.    
         end.     
         set(pColl as IList):
             if valid-object(Warnings) and Warnings:Size > 0 then 
                  undo, throw new IllegalArgumentError("Warnings collection is already set and has data"). 
             Warnings = pColl.
             HasWarnings = valid-object(pColl).
         end.   
   
    define private property mQuestions as IGetDataQuestions no-undo
        get.
        set.
    
    define public property HasQuestions as logical no-undo 
        get():
            return valid-object(mQuestions).
        end.       
    
    define public property Questions as IGetDataQuestions  no-undo  
        get():
            if not valid-object(mQuestions) then 
                 mQuestions = new GetDataQuestions().
            return mQuestions.    
        end.     
        protected set(pquestions as IGetDataQuestions):
            if valid-object(mQuestions) and mQuestions:Size > 0 then 
                undo, throw new IllegalOperationError("Questions collection is already set and has data"). 
            mQuestions = pquestions.
        end. 
    
     /* Returns the custom parameter object */
    define public property CustomParameter as Progress.Lang.Object no-undo get. set. 
    
    /* Returns the named query instance */
    define public property NamedQuery as INamedQuery no-undo get. set. // not public in interface 
    
    define public property TableRequests as IGetDataTableRequest extent no-undo get. protected set. // not public in interface   
    
    define public property RequestType         as RequestTypeEnum no-undo get. set. 
    define public property External            as logical no-undo get. set.  
      
     // set to true for singlerow method call to return empty
    define public property NoError             as logical no-undo get. set. 
    define public property MainRequestedBuffer as character no-undo get. set. 
    // tracks timout for request - the default is assumed to be in SessuinManager
    // so do not add a init value here as the query timeout message may use 
    // the presence of a value here to also sugggest increasing timeout 
    // in addition to the change filter and use paging advice 
    define public property Timeout             as integer no-undo get. set.
    
    define public property ClientOnly          as logical no-undo 
        get(): 
            define variable i as integer no-undo.
            do i = 1 to extent(TableRequests):
                if type-of(TableRequests[i],IGetDataRequest) then 
                do: 
                    // assigned in DatasetGetManager:PrepareTableRequest based on 
                    // BE definition and parent state (mainly disallowing false for child of true)  
                    if cast(TableRequests[i],IGetDataRequest):ClientOnly = false then
                        return false. 
                end.
                else 
                    return false.   
            end.    
            return true.
        end.
     
    /* Default constructor */
    constructor public GetDataRequest (  ):
        super ().
    end constructor.

    constructor public GetDataRequest(input pRequests as IGetDataTableRequest extent, pRequestType as RequestTypeEnum):
        this-object(pRequests).
        RequestType = pRequestType.
    end constructor.
    
    constructor public GetDataRequest(input pRequest as IGetDataTableRequest):
        this-object(cast(Array:ToArray(pRequest),IGetDataTableRequest)).
    end constructor.
    
    constructor public GetDataRequest(input pQuery as INamedQuery):
        super().
        
        Assert:NotNull(pQuery, 'Named query').
        assign this-object:NamedQuery = pQuery.
    end constructor.
        
    
    /* Constructor
       @param IGetDataTableRequest[] The table requests */
    constructor public GetDataRequest(input pRequests as IGetDataTableRequest extent):
        define variable i as integer no-undo.
        
        super().
        
        Assert:HasDeterminateExtent(pRequests, 'Table requests').
          
        assign this-object:TableRequests = pRequests.
        
    end constructor.
   
    method public logical HasTableRequest(pName as character).
        return valid-object(GetTableRequest(pName)).
    end method.
    
    method public logical HasTableRequest(pHandle as handle).
        case pHandle:type:
            when "buffer" then
                return HasTableRequest(pHandle:table-handle:name).
            when "temp-table" then
                return HasTableRequest(pHandle:name).
        end.     
    end method.
    
    method public IGetDataTableRequest GetTableRequest(pHandle as handle):
        case pHandle:type:
            when "buffer" then
                return GetTableRequest(pHandle:table-handle:name).
            when "temp-table" then
                return GetTableRequest(pHandle:name).
        end.    
    end.
    
    method public IGetDataTableRequest GetTableRequest(pName as character):
        define variable i as integer no-undo.
        do i = 1 to extent(TableRequests):
            if TableRequests[i]:TableName = pname then
                return TableRequests[i].      
        end.         
    end.
    
    method public logical ReplaceTableRequests(pTableRequest as IGetDataTableRequest extent):
        extent(TableRequests) = ?.
        TableRequests = pTableRequest.
    end method. 
    
    method public logical AddTableRequest(pTableRequest as IGetDataTableRequest):
        extent(TableRequests) = (if extent(TableRequests) = ? then 0 else extent(TableRequests)) +  1.
        TableRequests[extent(TableRequests)] = pTableRequest.
        return true.
    end method.
     
    method public logical EmptyQuestions().
        define variable oQuestions as Object no-undo.   
        if HasQuestions then
        do:
            // avoid lazy load from delete
            oQuestions = Questions.
            Questions:Clear().
            delete object oQuestions.
        end.     
        Questions = ?.
    end method.
    
    method override public character ToString(  ):
        define variable i as integer no-undo.
        define variable cString as character no-undo.
        define variable cDLm as character no-undo.
        do i = 1 to extent(TableRequests):
            cString = trim(subst("&1&2 &3",
                            cString,
                            cDlm,
                            TableRequests[i]:ToString())
                            ).
            cDlm = ",". 
        end.     
        if cString > "" then
            return cString.
        else 
            return super:ToString().  
    end method.
    
    method public Progress.Reflect.Method GetClientMethod(pObject as Object, phDataset as handle,output pClientParameterList as ParameterList):
        define variable oTableRequest as IGetDataTableRequest no-undo.
        define variable oNamedQuery as INamedQuery no-undo.
        define variable oMethod as Progress.Reflect.Method no-undo.
        if MainRequestedBuffer > "" then
        do: 
            oTableRequest = GetTableRequest(phDataset:get-buffer-handle(MainRequestedBuffer)).
            if valid-object(oTableRequest) 
            and type-of(oTableRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest) then
            do:
                oNamedQuery = cast(oTableRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest):NamedQuery.
                if valid-object(oNamedQuery)  then
                    oMethod = GetClientMethod(oNamedQuery,pObject,phDataset, output pClientParameterList).
            end.
        end.
        else do:
           oNamedQuery = NamedQuery.
           if valid-object(oNamedQuery)  then
               oMethod = GetClientMethod(oNamedQuery,pObject,phDataset, output pClientParameterList).
        end.   
        // if client only then there must be a method in the BE
        if valid-object(oNamedQuery) and not valid-object(oMethod) then 
        do:  
            if type-of(pObject,IBusinessEntity) and cast(pObject,IBusinessEntity):ClientOnly then
            do: 
                undo, throw new IllegalStateError(subst("Function &1&2 was not found in client only resource &3",
                                                         oNamedQuery:Name,
                                                         if extent(oNamedQuery:Parameters) > 0 then
                                                            subst("(&1)",GetParametersAsString(oNamedQuery:Parameters))  
                                                         else "",   
                                                         cast(pObject,IBusinessEntity):Resource
                                                        )
                                                   ). 
            end.   
        end. 
        return oMethod.    
    end method.
    
    method protected character GetParametersAsString(pParameters as INamedQueryParameter extent):
        define variable i as integer no-undo.
        define variable cSignatureAsString as character no-undo.
        do i = 1 to extent(pParameters):
           cSignatureAsString = StringUtil:AppendEntry(cSignatureAsString,
                                                       pParameters[i]:name  
                                                       ). 
        end. 
        return cSignatureAsString.     
    end method.    
    
    
    method protected Progress.Reflect.Method GetClientMethod(pNamedQuery as INamedQuery, pObject as Object, phDataset as handle,output pClientParameterList as ParameterList): 
        define variable i as integer no-undo. 
        define variable cDataType as character no-undo.
        define variable oMethod as Progress.Reflect.Method no-undo.
       
        // first check if there is one for parameters in the request
        if extent(pNamedQuery:Parameters) > 0 then
        do:
            // not found with request and dataset then check if there is one with paramters   
            pClientParameterList = new ParameterList((if extent(pNamedQuery:Parameters) > 0 then extent(pNamedQuery:Parameters) else 0) + 2).
            
            // The Parameters have been ordered according to the Function definition     
            do i = 1 to extent(pNamedQuery:Parameters):
                if not valid-object(pNamedQuery:Parameters[i]) then 
                    undo, throw new IllegalStateError(subst("NamedQuery '&1' Parameter[&2] is invalid",pNamedquery:Name,i)). 
                
                if valid-object(pNamedQuery:Parameters[i]:Values) then 
                do: 
                    cDataType = PrimitiveHolderUtil:GetDataType(pNamedQuery:Parameters[i]:Values).  
                    case entry(1,cDataType," "):
                        when "character" then
                            pClientParameterList:SetParameter(i, cDataType ,"input", PrimitiveHolderUtil:GetCharacterValues(pNamedQuery:Parameters[i]:values)).
                        when "longchar" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetLongcharValues(pNamedQuery:Parameters[i]:values)).
                        when "integer" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetIntegerValues(pNamedQuery:Parameters[i]:values)).
                        when "int64" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetInt64Values(pNamedQuery:Parameters[i]:values)).
                        when "decimal" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetDecimalValues(pNamedQuery:Parameters[i]:values)).
                        when "logical" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetLogicalValues(pNamedQuery:Parameters[i]:values)).
                        when "date" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetDateValues(pNamedQuery:Parameters[i]:values)).
                        when "datetime" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetDatetimeValues(pNamedQuery:Parameters[i]:values)).
                        when "datetime-tz" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetDatetimetzValues(pNamedQuery:Parameters[i]:values)).
                    end.
                end.
                else do:
                    cDataType = PrimitiveHolderUtil:GetDataType(pNamedQuery:Parameters[i]:Value).  
                  
                    case cDataType:
                        when "character" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetCharacterValue(pNamedQuery:Parameters[i]:value)).
                        when "longchar" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetLongcharValue(pNamedQuery:Parameters[i]:value)).
                        when "integer" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetIntegerValue(pNamedQuery:Parameters[i]:value)).
                        when "int64" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetInt64Value(pNamedQuery:Parameters[i]:value)).
                        when "decimal" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetDecimalValue(pNamedQuery:Parameters[i]:value)).
                        when "logical" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetLogicalValue(pNamedQuery:Parameters[i]:value)).
                        when "date" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetDateValue(pNamedQuery:Parameters[i]:value)).
                        when "datetime" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetDatetimeValue(pNamedQuery:Parameters[i]:value)).
                        when "datetime-tz" then
                            pClientParameterList:SetParameter(i, cDataType,"input", PrimitiveHolderUtil:GetDatetimetzValue(pNamedQuery:Parameters[i]:value)).
                    end.
                end.
            end.
            pClientParameterList:SetParameter(pClientParameterList:NumParameters - 1, get-class(IGetDataRequest):TypeName, "input", this-object).
            pClientParameterList:SetParameter(pClientParameterList:NumParameters, "dataset-handle", "input by-reference", phDataset).
            oMethod = pObject:GetClass():GetMethod(pNamedQuery:Name,Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance, pClientParameterList).
            if valid-object(oMethod) then 
            do: 
               return oMethod.  
            end.
            
        end.
        // now check if  there is a general one (no paramters or it may get paramtwrs from the request 
        pClientParameterList = new ParameterList(2).
        
        pClientParameterList:SetParameter(pClientParameterList:NumParameters - 1, get-class(IGetDataRequest):TypeName, "input", this-object).
        pClientParameterList:SetParameter(pClientParameterList:NumParameters, "dataset-handle", "input by-reference", phDataset).
        
        oMethod  = pObject:GetClass():GetMethod(pNamedQuery:Name,Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance, pClientParameterList). 
        if valid-object(oMethod) then
        do: 
            return oMethod.  
        end.
         
    end method.   
    
    method public void RemoveNamedQuery().
        NamedQuery = ?.
    end method. 
    
    // allow subclasses to create correct type from Clone 
    method protected GetDataRequest CreateNewForClone():
        return new GetDataRequest().        
    end method. 
        
    method public override Object Clone(): 
        define variable oClientRequest as GetDataRequest no-undo.
        assign
            oClientRequest = CreateNewForClone()         
            oClientRequest:TableRequests       = this-object:TableRequests 
            oClientRequest:CustomParameter     = this-object:CustomParameter
            oClientRequest:External            = this-object:External
            oClientRequest:MainRequestedBuffer = this-object:MainRequestedBuffer
            oClientRequest:NamedQuery          = this-object:NamedQuery
            oClientRequest:NoError             = this-object:NoError
            oClientRequest:RequestType         = this-object:RequestType
            oClientRequest:Timeout             = this-object:Timeout
            . 
         // this will set HasWarnings also   
         if this-object:HasWarnings then 
             oClientRequest:Warnings = this-object:Warnings. 
        
        return oClientRequest. 
    end method.         
    
end class.