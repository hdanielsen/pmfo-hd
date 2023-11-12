
 /*----------------------------------------------------------------------
    File        : BusinesseEtity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 11 00:13:46 EST 2019R
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using Ccs.BusinessLogic.CommitScopeEnum from propath.
using Ccs.BusinessLogic.IGetDataRequest from propath.
using Ccs.BusinessLogic.IGetDataResponse from propath.
using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.IGetDataTableResponse from propath.
using Ccs.BusinessLogic.IGetResultCountResponse from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Ccs.Common.Application from propath.
using Ccs.ServiceManager.ILifecycleScope from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.Collection from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.List from propath.
using OpenEdge.Core.Collections.Map from propath.
using OpenEdge.Core.Collections.Set from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.BusinessLogic.Action from propath.
using Pmfo.Core.BusinessLogic.BeforeImageReadEnum from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.DatasetInnerJoinManager from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetGetManager from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetManager from propath.
using Pmfo.Core.BusinessLogic.DatasetManager.IDatasetUpdateManager from propath.
using Pmfo.Core.BusinessLogic.DatasetRelation from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchDelegate from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchDelegator from propath.
using Pmfo.Core.BusinessLogic.Delegate.IFetchFunctionDelegate from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataResponse from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IDatasetRelation from propath.
using Pmfo.Core.BusinessLogic.IFindService from propath.
using Pmfo.Core.BusinessLogic.IJsonReadHandler from propath.
using Pmfo.Core.BusinessLogic.IJsonReadHandlerProvider from propath.
using Pmfo.Core.BusinessLogic.IJsonWriter from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.IQueryGroup from propath.
using Pmfo.Core.BusinessLogic.IReceiveDataEventArgs from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataEventArgs from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.MethodBindingEnum from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Pmfo.Core.BusinessLogic.QueryPredicate from propath.
using Pmfo.Core.BusinessLogic.ReceiveDataEventArgs from propath.
using Pmfo.Core.BusinessLogic.Relation from propath.
using Pmfo.Core.BusinessLogic.RelationTypeEnum from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.Router.DatasetRouter from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.BusinessLogic.UpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.UpdateDataRequestAdapter from propath.
using Pmfo.Core.Common.DataService from propath.
using Pmfo.Core.Common.Function from propath.
using Pmfo.Core.Common.IFunction from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.Common.IParameter from propath.
using Pmfo.Core.Common.IQuestion from propath.
using Pmfo.Core.Common.ISignature from propath.
using Pmfo.Core.Common.ISuccessMessages from propath.
using Pmfo.Core.Common.IWarningMessageTableHolder from propath.
using Pmfo.Core.Common.IWarningMessages from propath.
using Pmfo.Core.Common.LifecycleScope from propath.
using Pmfo.Core.Common.Question from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.Common.Signature from propath.
using Pmfo.Core.Common.SuccessMessage from propath.
using Pmfo.Core.Common.SuccessMessages from propath.
using Pmfo.Core.Common.WarningMessages from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.ForbiddenRequestError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Core.Error.TransactionError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Error.ValidationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Service.ClientGetRequest from propath.
using Pmfo.Core.Service.ClientUpdateRequest from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Core.Service.IClientUpdateRequest from propath.
using Pmfo.Core.Service.JsonDatasetErrorProcessor from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.ErrorStringUtil from propath.
using Pmfo.Util.PrimitiveHolderUtil from propath.
using Pmfo.Util.ServiceInfo from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
using Pmfo.Core.BusinessLogic.IPrepareGetRequestPlugin from propath.
using Pmfo.Core.Common.IGetDataQuestion from propath.
using Pmfo.Core.Common.GetDataQuestion from propath.
using OpenEdge.Core.WidgetHandle from propath.
using Pmfo.Core.Service.IInfrastructureService from propath.
using Pmfo.Core.Common.INamedArrayHolder from propath.
using Pmfo.Core.Common.NamedArrayNameEnum from propath.
using Ccs.BusinessLogic.JoinEnum from propath.
using Pmfo.Core.BusinessLogic.RequestBuilder.FindManyRequestBuilder from propath.
using Pmfo.Core.BusinessLogic.IValidateUpdateRequestPlugin from propath.
 
class Pmfo.Core.BusinessLogic.BusinessEntity abstract inherits DataService implements IBusinessEntity, IFetchDelegator:
    
    
    define public event DataReceived signature void (pParent as IBusinessEntity,pReceiveDataArgs as IReceiveDataEventArgs, phDataset as handle).
    
    define public property ClientOnly         as logical       no-undo get. protected set.
      
    define public property Datasets as handle no-undo extent get. protected set.
    // allow control of which dataset to use for linking - useful if main dataset is recursive, perhops  also if  dataset has lots of tables for update  
    define public property LinkDataset    as handle no-undo  
         get():
             if not valid-handle(LinkDataset) then 
                 return Datasets[1].
             else 
                 return LinkDataset.     
         end.
         protected set(hDataset as handle).
             if extent(Datasets) = ? then 
                  undo, throw new IllegalStateError("Cannot set LinkDataset before The Datasets are defined").
             if LinkDataset:serialize-name <> Datasets[1]:serialize-name then 
                 undo, throw new IllegalStateError("LinkDataset must have the same serialize-name as the Datasets[1]").
             LinkDataset = hDataset.      
         end.
    // not in interface and not really in use, but allowed -  see comments for GetKeys 
    define public property Keys     as character no-undo 
        get():
            return GetKeys().
        end. 
    
    define public property WarningMessages    as IWarningMessages no-undo 
        get. 
        set(pWarningMessages as IWarningMessages):
           if valid-object(WarningMessages) and WarningMessages:HasData then 
               undo, throw new IllegalOperationError("Cannot set WarningMessages when the existing WarningMessages has data").
           WarningMessages = pWarningMessages.         
        end. 
    
    define public property HasSuccessMessages  as logical       no-undo get.  protected set.
    
    define public property SuccessMessages    as ISuccessMessages no-undo 
        get():
            if not valid-object(SuccessMessages) then 
                SuccessMessages = new SuccessMessages().    
            return SuccessMessages.             
        end.    
        protected set(pSuccessMessages as ISuccessMessages): 
            SuccessMessages = pSuccessMessages.
            HasSuccessMessages = valid-object(pSuccessMessages). // setting it to ? makes has false  
        end.    
    
       define public property Resource  as character no-undo 
        get. 
        protected set(pResource as char):
            if compare(substr(pResource,1,2),"<>",lc(substr(pResource,1,2)),"case-sensitive") then 
               undo, throw new IllegalArgumentError(subst("Resource name '&1'. The name should start with at least two lower case characters",pResource)).
            Resource = pResource.   
        end.
    
    define private property IsInfrastructureServiceStarted as logical no-undo get. set.
  
    define protected property InfrastructureService as IInfrastructureService no-undo
        get ():
            // the application service manager is expected to manage and cache it so don't cache it here 
            // there is currently no service for this in Pmfo   
            InfrastructureService = cast(Application:ServiceManager:getService(get-class(IInfrastructureService)),IInfrastructureService).
            return InfrastructureService.
        end.    
        set (pService as IInfrastructureService):
            IsInfrastructureServiceStarted = valid-object(pservice).
            InfrastructureService = pService. 
        end.      
       
     
    define public property NextIds        as IMap          no-undo get. protected set.    
    
    // see interface 
    define public property UseJsonReaderOnPost as logical   no-undo init ? 
        get().
             if UseJsonReaderOnPost = ? then
                 return valid-object(NextIds) 
                        or type-of(this-object,IJsonReadHandler)
                        or type-of(this-object,IJsonReadHandlerProvider). 
             else        
                 return UseJsonReaderOnPost.     
        end.
        protected set. 
    
    // this may be defaulted to allow after-image to only have changes, but as of current 
    // this requires that calculated fields are defined as read-onlyfields    
    define public property BeforeImageReadOption as BeforeImageReadEnum  no-undo 
        get().
             if not valid-object(BeforeImageReadOption) then 
                 return BeforeImageReadEnum:None.
             else       
                return BeforeImageReadOption. 
        end.
        set.
    
    define public property UseJsonReaderOnPut as logical   no-undo init ? 
        get().
            if UseJsonReaderOnPut = ? then 
            do: 
                // the Operation Handler will throw illegalstate if UseJsonReaderOnPut is false in this case 
                if BeforeImageReadEnum:None:Equals(BeforeImageReadOption) = false then 
                    return true.
            end.
            return UseJsonReaderOnPut.     
        end. 
        set. 
    
    define public  property InheritsResource    as character  no-undo get. protected set.
    
    define protected property FetchDelegate  as IFetchDelegate  no-undo 
        get():
            return cast(cast(Application:ServiceManager,IServiceManager):getService(get-class(IFetchDelegate)),IFetchDelegate).
        end.     
    
    define protected property FetchFunctionDelegate  as IFetchFunctionDelegate  no-undo 
        get():
            return cast(cast(Application:ServiceManager,IServiceManager):getService(get-class(IFetchFunctionDelegate)),IFetchFunctionDelegate).
        end.     
    
    
    define private variable mFunctionsCreated   as logical no-undo.
    define private variable mActionsCreated     as logical no-undo.
    define private variable mLinksCreated       as logical no-undo.
    define private variable mRedirectCreated    as logical no-undo.
     
    define protected property SubscribeName        as character no-undo get. set.
    
     // we increase the numbers to separate from DS and BE numbers (0 - 100 DS 101 - 200 BE 
    define protected property ReservedConfirmationNumbers as integer init ? no-undo 
        get():
            define variable iMax as integer no-undo.
            define variable iMin as integer no-undo.
            if ReservedConfirmationNumbers = ? then 
            do: 
                ErrorStringUtil:GetConfirmationRange("BusinessLayer", output iMin, output iMax).
                ReservedConfirmationNumbers = iMin - 1.
            end.    
            return ReservedConfirmationNumbers.
        end.   
        private set.
    
        
    
    // keep track of GetAllData  
    // NOTE there can be cases where we have all data and this is not set to true
    // it is mainly a helper when using GetData(no parameters) in public methods to avoid fetching
    // every time the public method is called
    
    /* typical usage in subclass method    
       --- 
        if not AllDataIsFetched then
        do:
            this-object:Clear().  
            GetData().
        end.
      ---  
    */        
    define protected property AllDataIsFetched  as logical no-undo get. set.
    
    define public property Links as ISet no-undo 
        get():
            if not valid-object(Links) then 
                Links = new Set().
            return Links.    
        end. 
        set(pLinks as ISet):
            Links = pLinks.
            mLinksCreated = true. // allow us to check for link without newing the class
        end.
  
    define public property Functions as IMap no-undo 
        get():
            if not valid-object(Functions) then 
                Functions = new Map().
            return Functions.    
        end. 
        set(pFunctions as IMap):
            Functions = pFunctions.
            mFunctionsCreated = true. // allow us to check for function in GetdataSet without newing the class
        end.    
    
    define public property Actions as IMap no-undo 
        get():
            if not valid-object(Actions) then 
                Actions = new Map().
            return Actions.    
        end. 
        set(pActions as IMap):
            Actions = pActions.
            mActionsCreated = true. // allow us to check for action without newing the class
        end.    
    
    define public property RedirectMap as IStringStringMap no-undo 
        get():
            if not valid-object(RedirectMap) then 
                RedirectMap = new StringStringMap().
            return RedirectMap.    
        end. 
        set(pRedirectMap as IStringStringMap):
            RedirectMap = pRedirectMap.
            mRedirectCreated = true. // allow us to check for action without newing the class
        end.    
    
    
    define public property HasLink as logical init ? no-undo 
        get().
           if mLinksCreated then  
                return Links:Size > 0.
            else 
                return false.
     
        end.
    define public property HasAction as logical init ? no-undo 
        get().
            if mActionsCreated then  
                return Actions:Size > 0.
            else 
                return false.
        end.
    
    define public property HasFunction as logical init ? no-undo 
        get().
            if mFunctionsCreated then  
                return Functions:Size > 0.
            else 
                return false.
        end.
    
    define public property HasRedirectMap as logical init ? no-undo 
        get().
            if mRedirectCreated then  
                return RedirectMap:size > 0.
            else 
                return false.
        end.
    
    // for IFetchDelegator NOT currently in IBusinessEntity (Can be added since Datasets is public )  
     define public property DefaultDataset as handle no-undo 
        get():
            return Datasets[1].
        end. 
    
    define protected property Questions as IList no-undo
        get().
            if not valid-object(this-object:Questions) then
                this-object:Questions = new List().
            return Questions.
        end.
        protected set.
    
    define protected property DataAccess as IDataAccess no-undo 
        get():
            if not valid-object(DataAccess) then 
                DataAccess = CreateDataAccess().
            return DataAccess.     
        end.     
        set.
   
    define public property ServerType as ServerTypeEnum no-undo 
        get():
            if not valid-object(ServerType) then 
                ServerType = ServerTypeEnum:Default.
            return ServerType.   
        end.
        protected set(pType as ServerTypeEnum):
            AssertConstructorProperty().
            ServerType = pType. 
        end. 
     
     // exposed in resources cache -     
    define public property Public   as logical init true no-undo 
        get. 
        protected set(plPublic as logical):
            AssertConstructorProperty().
            Public = plPublic.
        end.
        
    define public property ReadOnly  as logical   no-undo init ?   
        get().  
            if ReadOnly = ? then do: 
                if DatasetUtil:GetNumTopBuffers(DefaultDataset) = 1 then
                    ReadOnly = not valid-handle(DefaultDataset:get-top-buffer(1):table-handle:before-table).
                else     
                    ReadOnly = true.
            end.    
            return ReadOnly.
        end get.    
        protected set(plReadOnly as logical):
            AssertConstructorProperty().
            ReadOnly = plReadOnly.
        end.
    
    define public property CanDelete as logical   no-undo init ? 
        get().
            if CanDelete = ? then
                CanDelete = not ReadOnly.
            return CanDelete.    
        end get.    
        protected set(plCanDelete as logical):
            AssertConstructorProperty().
            CanDelete = plCanDelete.
        end.
    
    define public property CanCreate as logical   no-undo init ? 
        get().
            if CanCreate = ? then
                CanCreate = not ReadOnly.
            return CanCreate.    
        end get.    
        protected set(plCreate as logical):
            AssertConstructorProperty().
            CanCreate = plCreate.
        end.
        
    define public property CanUpdate as logical   no-undo init ? 
        get().
            if CanUpdate = ? then
               return not ReadOnly.
            return CanUpdate.    
        end get.    
        protected set(plCanUpdate as logical):
            AssertConstructorProperty().
            CanUpdate = plCanUpdate.
        end.    
    
    // set to false for classes that only supports public update - (quite uncommon)
    define public property CanRead as logical   no-undo init ? 
        get().
            if CanRead = ? then
               return true.
            return CanRead.    
        end get.    
        protected set(plCanRead as logical):
            AssertConstructorProperty().
            CanRead = plCanRead.
        end.    
    
    define protected property CreateOnlyFields       as character no-undo get. set.
    define protected property ReadOnlyFields         as character no-undo get. set.
    define protected property ValidateRequiredFields as logical   get. set.
    define protected property ServerFields           as character get. set. // allow ? on client - used for keys and create date and such 
    // use for fields that have init ? to enforce a value from UI (otherwise init ? bypasses ValidateRequiredFields) 
    define protected property RequiredFields         as character no-undo get. set.
     
    
    // avoid memory leak by keeping only one requested dynamic dataset (set in GetRequestDataset) 
    // and deleting previous and providing a handle for the destructor to delete when done   
    define protected property RequestDatasetHandle as handle get. 
        set(ph as handle):
            if valid-handle(RequestDatasetHandle) and RequestDatasetHandle:dynamic and RequestDatasetHandle <> ph then
                delete object RequestDatasetHandle no-error.
            RequestDatasetHandle = ph.
        end.
    
     
    /*--------------------------------------------------------------------------
     Purpose: Constructor for entities with one dataset with one top buffer
              Pass the buffer, temp-table or dataset as handle 
     Notes:  A single top buffer defines the resource name 
             if more than one top buffer the dataset serialize-name is used 
    --------------------------------------------------------------------------*/
    
    constructor public BusinessEntity ( phHandle as handle ):
        define variable iNumTop  as integer no-undo.
        define variable lAsserted as logical no-undo.
        define variable cMsg      as character no-undo 
            init "Handle passed to BusinessEntity Constructor".
        
        super ().
        Assert:NotNull(phHandle,cMsg).
        extent(Datasets) = 1.
        case phHandle:type:
            when "buffer" then 
            do:
                create dataset DataSets[1].
                DataSets[1]:name = "ds" + substr(phHandle:table-handle:name,2).
                /* for single top -table we keep the public names the same */
                DataSets[1]:serialize-name = phHandle:serialize-name.
                DataSets[1]:add-buffer(phHandle).
            end. // buffer
            when "temp-table" then 
            do:
                create dataset DataSets[1].
                DataSets[1]:name = "ds" + substr(phHandle:name,2).
                /* for single top-table we keep the public names the same */
                DataSets[1]:serialize-name = phHandle:serialize-name.
                DataSets[1]:add-buffer(phHandle:default-buffer-handle).
            end. // temp-table   
            when "dataset" then 
            do:  
                iNumTop = DatasetUtil:GetNumTopBuffers(phHandle).          
                if iNumTop > 1  then
                do:
                    if phHandle:serialize-name <> phHandle:name then 
                    do:
                        AssertDatasetName(phHandle).
                        lAsserted = true.
                    end.    
                    else 
                        undo, throw new IllegalArgumentError(subst("Constructor without only dataset parameter used with dataset with more than one top buffer and no serialize-name in '&1",this-object:GetClass():TypeName)).
                end.    
                /* for single top-table the public names should be the same 
                   if the serialize-name is not specified just set it 
                   - the AssertDatasetName will throw error if a different serialize-name was specified 
                */
                if iNumTop = 1 and phHandle:serialize-name = phHandle:name then 
                do:
                    phHandle:serialize-name = DatasetUtil:GetTopBuffer(phHandle,1):serialize-name.
                end.    
                DataSets[1] = phHandle.
            end. //dataset
            otherwise do:
                undo, throw new IllegalArgumentError(Cmsg + " must be buffer, temp-table or dataset").
            end.     
        end case.
        Resource = DataSets[1]:serialize-name. 
        if not lAsserted then 
            AssertDatasetName(DefaultDataset).    
    end constructor.
    
    /** 
    Replaces use of BufferUtil:GetKeys 
    The use of BufferUtil(buffer is a violation of OO encapsulation, but buffers are  
    managed as pseudo-objects throughout so we allowed this for practical reasons .
    
    (BufferUtil:GetKeys is/was intended as a language extension required due to the fact 
     that buffer:keys cannot be used since we have the sequence as primary index)  
    **/     
    
    method public character GetKeys():
        if DatasetUtil:GetNumTopBuffers(Datasets[1]) = 1 then 
            return BufferUtil:GetInternalKeys(Datasets[1]:get-buffer-handle(1)).
        return ?.     
    end.     
    
   method public character GetPublicKeys():
       define variable cKeys as character no-undo.
       if DatasetUtil:GetNumTopBuffers(Datasets[1]) = 1 then 
       do: 
           cKeys = GetKeys().
        
           return BufferUtil:GetPublicFieldNames(Datasets[1]:get-buffer-handle(1), cKeys).
       end.
       return ?.    
   end.     
    
    method public character GetPublicTextField():
        define variable hFld as handle no-undo.
        define variable i as integer no-undo.
        define variable cTextFieldIdx as character extent no-undo.
        define variable hbuffer as handle no-undo.
        
        if DatasetUtil:GetNumTopBuffers(Datasets[1]) = 1 then 
        do:
            hBuffer = Datasets[1]:get-buffer-handle(1).      
            hFld = hBuffer:buffer-field("name") no-error.
            if valid-handle(hFld) then
                return hFld:serialize-name.
            hFld = hBuffer:buffer-field("description") no-error.
            if valid-handle(hFld) then
                return hFld:serialize-name.
            hFld = hBuffer:buffer-field("descr") no-error.
            if valid-handle(hFld) then
            do:
                if hFld:serialize-name = "description" or hFld:serialize-name = "name" then
                    return hFld:serialize-name.
                else do i = 1 to hBuffer:num-fields:
                    hFld = hBuffer:buffer-field(i).
                    if hFld:serialize-name = "description" then
                        return hFld:serialize-name.
                end.       
            
            end. 
        // check if there is an index defined with name textfield
        // (convention - the assumtion is that it is ok to have an index of such a field) 
            cTextFieldIdx = BufferUtil:GetFieldsInIndex(hBuffer,"TextField",false).
            if extent(cTextFieldIdx) = 1 then 
                return cTextFieldIdx[1].
        end.        
        return "".          
    end.     
  
    method protected IDatasetGetManager GetExistingDatasetGetManager(phDataset as handle):
        return cast(cast(Application:ServiceManager,IServiceManager):getExistingService(get-class(IDatasetGetManager),new LifecycleScope(phDataset)),IDatasetGetManager).
    end method.
    
    method protected IDatasetUpdateManager GetExistingDatasetUpdateManager(phDataset as handle):
        return cast(cast(Application:ServiceManager,IServiceManager):getExistingService(get-class(IDatasetUpdateManager),new LifecycleScope(phDataset)),IDatasetUpdateManager).
    end method.
    
    method protected IDatasetGetManager GetDatasetGetManager(phDataset as handle):
        return GetDatasetGetManager(new LifecycleScope(phDataset)).
    end method.
    
    method protected IDatasetGetManager GetDatasetGetManager(pScope as ILifeCycleScope):
        return cast(cast(Application:ServiceManager,IServiceManager):getService(get-class(IDatasetGetManager),pScope),IDatasetGetManager).
    end method.
    
    method protected IDatasetUpdateManager GetDatasetUpdateManager(phDataset as handle):
        return GetDatasetUpdateManager(new LifecycleScope(phDataset)).
    end method.
    
    method protected IDatasetUpdateManager GetDatasetUpdateManager(pScope as ILifecycleScope):
        return cast(cast(Application:ServiceManager,IServiceManager):getService(get-class(IDatasetUpdateManager),pScope),IDatasetUpdateManager).
    end method.
    
    /**
    Constructor that defined resource name for an entity with more than two top-buffers in a single dataset
    Only to be used in when the dataset has more than one buffer
    **/
    constructor public BusinessEntity (pcResource as char, phDataset as handle):
        define variable i as integer no-undo.
        define variable j as integer no-undo.
        define variable cMsg as character no-undo 
            init "Handle passed to BusinessEntity Constructor".
        
        super ().
        Assert:NotNull(phDataset,cMsg).
        Assert:isTrue(phDataset:type = "dataset",cMsg + " is not a dataset").
        
        if DatasetUtil:GetNumTopBuffers(phDataset) = 1 then 
            undo, throw new IllegalArgumentError("Constructor with parameter for Resource used with dataset that has one top buffer").
            
        AssertDatasetName(phDataset).
        extent(Datasets) = 1.
        Resource = pcResource.  
        
        Datasets[1] = phDataset.   
        
        do i = 1 to phDataset:num-top-buffers:
            if pcResource = phDataset:get-top-buffer (i):serialize-name then
                undo, throw new IllegalOperationError(subst("Resource cannot have the same name as top table[&1] serialize-name '&2'",i,phDataset:get-top-buffer (i):serialize-name)).
        end.
        
         // not really important what the dataset name is when more than one buffer, but
        if Datasets[1]:serialize-name = Datasets[1]:name then
            Datasets[1]:serialize-name = Resource.
        
            
    end constructor.
    
    /* Constructor for an entity with more than one dataset 
       The main (first) dataset must have only one buffer, 
       Yhis buffer will define the resouce name   
    */  
    constructor public BusinessEntity (phDatasets as handle extent):
        define variable i           as integer no-undo.
        define variable j           as integer no-undo.
        define variable lAsserted   as logical no-undo. 
        define variable iTopBuffers as integer no-undo.
        define variable cMsg as character no-undo 
            init "Handle number &1 passed to BusinessEntity Constructor".
        
        super ().
        if extent(phDatasets) = 1  then 
             undo, throw new IllegalArgumentError("Use the constructor with a non extent handle parameter for a single dataset").
        
        do i = 1 to extent(phdatasets): 
            Assert:NotNull(phDatasets[i],subst(cMsg,i)).
            Assert:isTrue(phdatasets[i]:type = "dataset",subst(cMsg,i) + " is not a dataset").
        end.
        
        do i = 1 to extent(phDatasets):
            iTopBuffers = DatasetUtil:GetNumTopBuffers(phDatasets[i]). 
            if iTopBuffers = 1  then 
            do:
                /* for single top-table the public names should be the same 
                    if the dataset serialize-name is not specified just set it 
                    - the AssertDatasetName will throw error if a different serialize-name was specified 
                */
                if phDataSets[i]:serialize-name = phDataSets[i]:name then 
                    phDataSets[i]:serialize-name = DatasetUtil:GetTopBuffer(phDatasets[i],1):serialize-name.
                    
                if i = 1 then 
                    Resource = phDataSets[i]:serialize-name.    
            end.       
            else if i = 1 then do:
                if phDatasets[1]:serialize-name <> phDatasets[1]:name then
                do: 
                    AssertDatasetName(phDatasets[1]).
                    Resource = phDataSets[1]:serialize-name.
                    lAsserted = true.
                end.
                else 
                    undo, throw new IllegalArgumentError(subst("Constructor with dataset extent parameter used with dataset[&1] with more than one top buffer and no serialize-name in '&2",i,this-object:GetClass():TypeName)).
            end.  
            if not lAsserted then 
                AssertDatasetName(phDatasets[i]).
        end.
        Datasets = phDatasets.   
    end constructor.
    
    /**
    Constructor that defined resource name for an entity with more than one dataset
    Only to be used in when the main (first) dataset has more than one buffer
    **/
    constructor public BusinessEntity (pcResource as char, phDatasets as handle extent):
        define variable i           as integer no-undo.
        define variable j           as integer no-undo.
        define variable iTopBuffers as integer no-undo.
        define variable cMsg        as character no-undo 
            init "Handle number &1 passed to BusinessEntity Constructor".
        
        super ().
        if extent(phDatasets) = 1  then 
             undo, throw new IllegalArgumentError("Use the constructor with a non extent handle parameter for a single dataset").
       
        do i = 1 to extent(phdatasets): 
            Assert:NotNull(phDatasets[i],subst(cMsg,i)).
            Assert:isTrue(phdatasets[i]:type = "dataset",subst(cMsg,i) + " is not a dataset").
        end.
        
        do i = 1 to extent(phDatasets): 
            iTopBuffers = DatasetUtil:GetNumTopBuffers(phDatasets[i]). 
            if iTopBuffers = 1 then 
            do:
                if i = 1 then
                    undo, throw new IllegalArgumentError("Constructor with parameter for Resource used when datasets[1] has one top buffer").
            
                 /* for single top-table the public names should be the same 
                    if the serialize-name is not specified just set it 
                   - the AssertDatasetName will throw error if a different serialize-name was specified 
                */
                if phDataSets[i]:num-top-buffers = 1 and phDataSets[i]:serialize-name = phDataSets[i]:name then 
                    phDataSets[i]:serialize-name = phDataSets[i]:get-top-buffer[i]:serialize-name.
                    
            end.       
            if pcResource = phDataSets[i]:serialize-name then 
                undo, throw new IllegalOperationError(subst("Dataset number &1 cannot have the same serialize-name as the resource",i)).
            if phDataSets[i]:serialize-name = phDataSets[i]:name then
                undo, throw new IllegalArgumentError(subst("Datasets[&1] &2 must have a serialize-name",i,phDataSets[i]:name)).
          
            AssertDatasetName(phDatasets[i]).
        end.
        Resource = pcResource.    
        Datasets = phDatasets.   
    end constructor.
    
    method protected IDataAccess CreateDataAccess():
        return cast(Application:ServiceManager,IServiceManager):CreateDataAccess(Resource).
    end method.

    method protected void AssertForeignKey(pcResource as character,phBuffer as handle):
        define variable oRelation as IRelation no-undo.
        define variable oFindService  as IFindService no-undo.
        define variable oQueryentry as IQueryentry no-undo.
        
        Assert:NotNull(phBuffer,"Buffer"). 
        if not phBuffer:avail then 
            undo, throw new IllegalStateError(subst("Assert Foreign Key called with no available &1 record",BufferUtil:GetRowName(phBuffer, true))).
             
        oRelation = GetLink(pcResource).
        
        if not valid-object(oRelation) then 
            undo, throw new IllegalArgumentError(subst("No Link found for '&1'",pcResource)).
        
        if oRelation:Cardinality = RelationTypeEnum:OneToMany then 
            undo, throw new IllegalArgumentError(subst("Relation &1 is a one to many relation",pcResource)).
        
        oQueryEntry = RelationQuery(phBuffer, oRelation).
        do on error undo, throw:
            oFindService = cast(Application:ServiceManager:getService(get-class(IFindService)),IFindService).
            oFindService:AssertFind(oRelation:name,oQueryentry).
            catch eNotFound as TableNotFoundError:
                if phbuffer:row-state = row-modified or phbuffer:row-state = row-created then 
                   ThrowBufferError(phbuffer, eNotFound:GetStrippedMessage(),"").
                else   
                   undo, throw new BadRequestError(eNotFound:GetStrippedMessage()).    
            end catch. 
        end.
    end method.     
    
    method protected void FillChildRows(pQueryEntry as IQueryEntry, pcChild as character, phDataset as handle):
        define variable hChildBuffer    as handle no-undo.
        define variable hChildTable     as handle no-undo.
        define variable oBe             as IBusinessEntity no-undo.
        define variable oDatasetManager as IDatasetUpdateManager no-undo.
        define variable oTableRequest   as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        
        Assert:NotNull(phDataset, "dataset").
        
        hChildBuffer = DatasetUtil:GetBufferBySerializeName(phDataset,pcChild).
        
        if valid-handle(hChildBuffer) then
        do: 
            hChildTable = hChildBuffer:table-handle.
         //   if hChildTable:has-records then
           //     undo,throw new UnsupportedOperationError(subst("FetchChildTable for '&1' with data",hChildTable:name)).       
            
            oDatasetManager = GetDatasetUpdateManager(phDataset).
            oBe             = oDatasetManager:GetBusinessEntity(hChildBuffer).            
            oTableRequest   = new GetDataTableRequest(hChildTable:name).
            oTableRequest:AddQueryEntry(pQueryEntry).  
            oBe:GetData(oTableRequest, output table-handle hChildTable append).
        end.
        else 
            undo,throw new IllegalArgumentError(subst("Buffer with serialize-name '&1' not found in dataset",hChildTable:name)).       
    end method.
    
    
    method protected void FillChildRows(phBuffer as handle, pcChild as character, phDataset as handle):
        define variable hChildBuffer    as handle no-undo.
        define variable hChildTable     as handle no-undo.
        define variable oBe             as IBusinessEntity no-undo.
        define variable oDatasetManager as IDatasetUpdateManager no-undo.
        define variable oRelation       as IRelation no-undo.
        define variable oTableRequest   as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        
        Assert:NotNull(phDataset, "dataset").
        
        hChildBuffer = DatasetUtil:GetBufferBySerializeName(phDataset,pcChild).
        oRelation    = GetLink(pcChild).
        if not valid-object(oRelation) then 
            undo, throw new IllegalArgumentError(subst("No Link found for '&1'",pcChild)).
        
        // we are appending so we can only fetch true children with no conflicting keys 
        // TODO - this may still fail at runtime 
        // - can possibly support use of table defined like and table copy with merge in TableUtil
        // - another option  is to refine relation definition for one to many s to support 
        // true contains versus reference - as in UML relations 
        
        if oRelation:Cardinality = RelationTypeEnum:ManyToOne then 
            undo, throw new IllegalArgumentError(subst("Relation &1 is a many to one relation",pcChild)).
        
        if valid-handle(hChildBuffer) then
        do: 
            hChildTable = hChildBuffer:table-handle.
         //   if hChildTable:has-records then
           //     undo,throw new UnsupportedOperationError(subst("FetchChildTable for '&1' with data",hChildTable:name)).       
            
            oDatasetManager = GetDatasetUpdateManager(phDataset).
            oBe             = oDatasetManager:GetBusinessEntity(hChildBuffer).            
            oTableRequest   = new GetDataTableRequest(hChildTable:name).
            oTableRequest:AddQueryEntry(RelationQuery(phBuffer,oRelation)).  
            oBe:GetData(oTableRequest, output table-handle hChildTable append).
        end.
        else 
            undo,throw new IllegalArgumentError(subst("Buffer with serialize-name '&1' not found in dataset",hChildTable:name)).       
    end method.
    
    method protected IQueryEntry RelationQuery(phBuffer as handle,pRelation as IRelation):
        define variable cFieldPairs  as character no-undo.
        define variable oFindService as IFindService no-undo.
        define variable oQueryGroup  as IQueryGroup no-undo.
        define variable i            as integer no-undo.
        cfieldPairs = pRelation:PairsList(). 
        if num-entries(cFieldPairs) = 2 then 
        do on error undo, throw: 
            return new QueryPredicate(entry(2,cFieldPairs),QueryOperatorEnum:Eq,phBuffer:buffer-field(entry(1,cFieldPairs)):buffer-value). 
            catch e as Progress.Lang.Error :
                undo, throw new IllegalStateError(subst("Couild not get value for relation field &1 in &2 record",entry(2,cFieldPairs),BufferUtil:GetRowName(phBuffer, true)),e).
            end catch. 
        end.
        else do : 
            oQueryGroup = new QueryGroup(). 
            do i = 1 to num-entries(cFieldPairs) by 2 on error undo, throw: 
                oQueryGroup:AddEntry(entry(i + 1,cFieldPairs) ,QueryOperatorEnum:Eq, phBuffer:buffer-field(entry(i,cFieldPairs)):buffer-value).
                catch e as Progress.Lang.Error :
                    undo, throw new IllegalStateError(subst("Couild not get value for relation field &1 in &2 record",entry(i + 1,cFieldPairs),BufferUtil:GetRowName(phBuffer, true)),e).
                end catch. 
            end. 
            return oQueryGroup.   
        end.         
    end method.    
    
    method protected void AssertDatasetName(phDataset as handle):
        define variable i as integer no-undo.
        /* The dataset:serialize-name is exposed as resource name, to be used in 
           the url to locate this business entity. 
           For cases where the dataset has more than one top level we use 
           the resource name as the table in GetDataTableRequest as a request 
           for all top level tables. This ensures that the data service never need 
           to know the exact schema to build a request. (Most ABL Data services 
           either caches the schema or requires a getDataset call to build the
           request or they delegate the request building to tht entity )    
           So to manage ambiguity between dataset and buffer in the table request 
           management with the ability to support direct requests for the top 
           single buffer serialize-name in the url (most requests)
           and also support using resouce name to retriece all top level 
           tables for rare cases with more than one top level table the following 
           rules are enforced (to everyones benefit...) 
           */  
        if DatasetUtil:GetNumTopBuffers(phDataset) = 1  then do:
            if phDataset:serialize-name <> DatasetUtil:GetTopBuffer(phDataset,1):serialize-name then
                undo, throw new IllegalOperationError(subst('The dataset and the single top table "&1" must have the same serialize-name in order to expose the top table as the entity~'s Resource',phDataset:get-top-buffer (1):name)).
        end.
        else do i = 1 to phDataset:num-top-buffers:
            if phDataset:serialize-name = phDataset:get-top-buffer (i):serialize-name then
                undo, throw new IllegalOperationError(subst("Datasets[&1] &2 must have a different serialize-name than the top table serialize-name '&3'",i,phDataset:name,phDataset:get-top-buffer (i):serialize-name)).
        end.
    end method.     
    
    method protected void SetInheritedResource (pcResourceName as character):
        SetInheritedResource(pcResourceName, true).   
    end method.
    
    method protected void SetInheritedResource (pcResourceName as character, plInherited as logical):
        define variable cMsgTmpl as character init "Cannot set inherited resource for '&1'" no-undo.
        define variable hTT as handle no-undo.
        
        
        if this-object:Datasets[1]:serialize-name = Resource then
        do: 
            do on error undo, throw: 
                hTT = DatasetUtil:GetTopBufferBySerializeName(Datasets[1], Datasets[1]:serialize-name):table-handle.
                catch e as Progress.Lang.Error :
                    undo, throw new IllegalOperationError(subst(cMsgTmpl,Resource),e).    
                end catch.
            end.         
            if plInherited then 
                InheritsResource = hTT:serialize-name. // for catalog 
            Datasets[1]:serialize-name = pcResourceName.
            hTT:serialize-name = pcResourceName.
            Resource = pcResourceName.
        end.
        else 
            undo, throw new UnsupportedOperationError(subst(cMsgTmpl,Resource)).    
        
    end method.
    
    method protected IFunction AddFunction(pFunction as IFunction):
        Functions:Put(new String(pFunction:Name), pFunction).
        return pFunction.
    end method.
    
    method protected IFunction AddFunction(pcname as character):
        return AddFunction(CreateFunction(pcName)).
    end method. 
    
    method protected IFunction AddFunction(pcname as character, pParameter as IParameter):
        return AddFunction(CreateFunction(pcName,pParameter)).
    end method.  
    
    method protected IFunction AddFunction(pcname as character, pParameters as IParameter extent):
        return AddFunction(CreateFunction(pcName,pParameters)).
    end method.
    
    method protected IFunction AddFunction(pcname as character, pParameters1 as IParameter extent,pParameters2 as IParameter extent):
        define variable oSignatures as ISignature extent no-undo.
        oSignatures = cast(Array:ToArray(new Signature(pParameters1 ),new Signature(pParameters2 )),ISignature) .
        return AddFunction(CreateFunction(pcName,oSignatures)).
    end method.    
    
    method protected IFunction AddFunction(pcname as character, pParameters1 as IParameter extent,pParameters2 as IParameter extent,pParameters3 as IParameter extent):
        define variable oSignatures as ISignature extent no-undo.
        oSignatures = cast(Array:ToArray(new Signature(pParameters1 ),new Signature(pParameters2 ),new Signature(pParameters3 )),ISignature) .
        return AddFunction(CreateFunction(pcName,oSignatures)).
    end method.    
    
    method protected IFunction CreateFunction(pcname as character):
        return new Function(pcname).
    end.
       
    method protected IFunction CreateFunction(pcname as character, pParameter as IParameter):
        return new Function(pcname, pParameter).
    end method.    
    
    method protected IFunction CreateFunction(pcname as character, pParameters as IParameter extent):
        return new Function(pcname, pParameters).
    end method.
    
    method protected IFunction CreateFunction(pcname as character, pSignatures as ISignature extent):
        return new Function(pcname, pSignatures).
    end method.
    
    method protected IAction AddAction(pcname as character):
        return AddAction(new Action(pcName)).
    end method.    
    
    method protected IAction AddAction(pcname as character,pServerType as ServerTypeEnum):
        return AddAction(new Action(pcName,pServerType)).
    end method.    
    
    method protected IAction AddAction(pcname as character, pParameter as IParameter):
        return AddAction(new Action(pcName,pParameter)).
    end method.    
    
    method protected IAction AddAction(pcname as character, pParameter as IParameter, pServerType as ServerTypeEnum):
        return AddAction(new Action(pcName,pParameter,pServerType)).
    end method.    
    
    method protected IAction AddAction(pcname as character, pParameters as IParameter extent):
        return AddAction(new Action(pcName,pParameters)).
    end method.    
    
    method protected IAction AddAction(pcname as character, pParameters as IParameter extent, pServerType as ServerTypeEnum):
        return AddAction(new Action(pcName,pParameters,pServerType)).
    end method.    
    
    method protected IAction AddAction(pcname as character, pParameters1 as IParameter extent,pParameters2 as IParameter extent):
        define variable oSignatures as ISignature extent no-undo.
        oSignatures = cast(Array:ToArray(new Signature(pParameters1 ),new Signature(pParameters2 )),ISignature) .
        return AddAction(new Action(pcName,oSignatures)).
    end method.    
    
    method protected IAction AddAction(pcname as character, pParameters1 as IParameter extent,pParameters2 as IParameter extent,pParameters3 as IParameter extent):
        define variable oSignatures as ISignature extent no-undo.
        oSignatures = cast(Array:ToArray(new Signature(pParameters1 ),new Signature(pParameters2 ),new Signature(pParameters3 )),ISignature) .
        return AddAction(new Action(pcName,oSignatures)).
    end method.    
    
    method protected IAction AddAction(pAction as iAction):
        Actions:Put(new String(pAction:Name), pAction).
        return pAction.
    end method.
    
    method protected logical AddLink(oRelation as IRelation):
        if Links:Contains(oRelation) then
        do:
            undo, throw new IllegalOperationError(subst("Link to '&1' already exists (&2)",oRelation:PublicName,oRelation:ToString())). 
        end.  
        return Links:Add(oRelation).
    end method.     
    
    /** single field methods */
    method protected IRelation AddManyToOneLink(pcRelatedResource as character,pcForeignKey as character ):
        define variable oRelation as IRelation no-undo.
        AssertDefaultDatasetTopBufferSameName().
        AssertLink(Datasets[1]:serialize-name,pcRelatedResource,Array:ToArray(pcForeignKey)).
        oRelation = new Relation(Datasets[1]:serialize-name,pcRelatedResource,pcForeignKey).
        AddLink(oRelation).
        return oRelation.   
    end method.    
    
    method protected IRelation AddManyToOneLink(pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character ):
        return AddLink(pcRelatedResource,pcParentFieldName,pcChildFieldName,RelationTypeEnum:ManyToOne).
    end method.
    
    method protected IRelation AddManyToOneLink(pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character, pcAlias as character ):
        return AddLink(pcRelatedResource,pcParentFieldName,pcChildFieldName,RelationTypeEnum:ManyToOne, pcAlias).
    end method.
    
    method protected IRelation AddManyToOneLink(pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent ):
        return AddLink(pcRelatedResource,pcParentFieldNames,pcChildFieldnames,RelationTypeEnum:ManyToOne).
    end method.
    
    method protected IRelation AddManyToOneLink(pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent, pcAlias as character ):
        return AddLink(pcRelatedResource,pcParentFieldNames,pcChildFieldnames,RelationTypeEnum:ManyToOne, pcAlias).
    end method.
    
    method protected IRelation AddOneToManyLink(pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character):
        return AddLink(pcRelatedResource,pcParentFieldName,pcChildFieldName,RelationTypeEnum:OneToMany).
    end method.
        
    method protected IRelation AddOneToManyLink(pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character, pcAlias as character ):
        return AddLink(pcRelatedResource,pcParentFieldName,pcChildFieldName,RelationTypeEnum:OneToMany, pcAlias).
    end method.
    
    method protected IRelation AddOneToManyLink(pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent):
        return AddLink(pcRelatedResource,pcParentFieldNames,pcChildFieldnames,RelationTypeEnum:OneToMany).
    end method.    
    
    method protected IRelation AddOneToManyLink(pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent, pcAlias as character):
        return AddLink(pcRelatedResource,pcParentFieldNames,pcChildFieldnames,RelationTypeEnum:OneToMany,pcAlias).
    end method.    
    
    method protected IRelation AddOneToOneLink(pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character ):
        return AddLink(pcRelatedResource,pcParentFieldName,pcChildFieldName,RelationTypeEnum:OneToOne).
    end method.
    
    method protected IRelation AddOneToOneLink(pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character, pcAlias as character ):
        return AddLink(pcRelatedResource,pcParentFieldName,pcChildFieldName,RelationTypeEnum:OneToOne, pcAlias).
    end method.
    
    method protected IRelation AddOneToOneLink(pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent ):
        return AddLink(pcRelatedResource,pcParentFieldNames,pcChildFieldnames,RelationTypeEnum:OneToOne).
    end method.
    
    method protected IRelation AddOneToOneLink(pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent, pcAlias as character ):
        return AddLink(pcRelatedResource,pcParentFieldNames,pcChildFieldnames,RelationTypeEnum:OneToOne, pcAlias).
    end method.
    
                                                                                                                                                                                                            
/*    method protected IRelation AddLink(pcParentEntity as character,pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character,pCardinality as RelationTypeEnum ):                          */
/*        return AddLink(pcParentEntity,pcRelatedResource,pcParentFieldName,pcChildFieldname,pCardinality,"").                                                                                                                */
/*    end method.                                                                                                                                                                                                             */
/*                                                                                                                                                                                                                            */
/*    method protected IRelation AddLink(pcParentEntity as character,pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character,pCardinality as RelationTypeEnum, pcAliasName as character ):*/
/*        define variable pcParents as character extent 1  no-undo.                                                                                                                                                           */
/*        define variable pcChildren  as character extent 1  no-undo.                                                                                                                                                         */
/*        pcParents[1]  = pcParentFieldName.                                                                                                                                                                                  */
/*        pcChildren[1] = pcChildFieldname.                                                                                                                                                                                   */
/*        return AddLink(pcParentEntity,pcRelatedResource,pcParents,pcChildren,pCardinality,pcAliasName).                                                                                                                     */
/*    end method.                                                                                                                                                                                                             */
/*                                                                                                                                                                                                                            */
    method protected IRelation AddLink(pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character,pCardinality as RelationTypeEnum ):
        return AddLink(pcRelatedResource,Array:ToArray(pcParentFieldName),Array:ToArray(pcChildFieldName),pCardinality).
    end method.    
    
    method protected IRelation AddLink(pcRelatedResource as character,pcParentFieldName as character,pcChildFieldname as character,pCardinality as RelationTypeEnum, pcAliasName as character ):
        return AddLink(pcRelatedResource,Array:ToArray(pcParentFieldName),Array:ToArray(pcChildFieldName),pCardinality,pcAliasName).
    end method. 
    
    method protected IRelation AddLink(pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent, pCardinality as RelationTypeEnum):
        define variable oRelation as IRelation no-undo.
        AssertDefaultDatasetTopBufferSameName().
        AssertLink(Datasets[1]:serialize-name,pcRelatedResource,pcParentFieldNames).
        oRelation = new Relation(Datasets[1]:serialize-name,pcRelatedResource,pcParentFieldNames,pcChildFieldnames,pCardinality).
        AddLink(oRelation). 
        return oRelation.
    end method.    
    
    method protected IRelation AddLink(pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent, pCardinality as RelationTypeEnum, pcAliasName as character):
        define variable oRelation as IRelation no-undo.
        AssertDefaultDatasetTopBufferSameName().
        AssertLink(Datasets[1]:serialize-name,pcRelatedResource,pcParentFieldNames).
        oRelation = new Relation(Datasets[1]:serialize-name,pcRelatedResource,pcParentFieldNames,pcChildFieldnames,pCardinality,pcAliasName).
        AddLink(oRelation). 
        return oRelation.
    end method.    
    
/*    method protected IRelation AddLink(pcParentEntity as char, pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent, pCardinality as RelationTypeEnum):                          */
/*        define variable oRelation as IRelation no-undo.                                                                                                                                                                                 */
/*        AssertLink(pcParentEntity,pcRelatedResource,pcParentFieldNames).                                                                                                                                                                */
/*                                                                                                                                                                                                                                        */
/*        oRelation = new Relation(pcParentEntity,pcRelatedResource,pcParentFieldNames,pcChildFieldnames,pCardinality).                                                                                                                   */
/*        Links:Add(oRelation).                                                                                                                                                                                                           */
/*        return oRelation.                                                                                                                                                                                                               */
/*    end method.                                                                                                                                                                                                                         */
/*                                                                                                                                                                                                                                        */
/*    method protected IRelation AddLink(pcParentEntity as char, pcRelatedResource as character,pcParentFieldNames as character extent,pcChildFieldnames as character extent, pCardinality as RelationTypeEnum, pcAliasName as character):*/
/*        define variable oRelation as IRelation no-undo.                                                                                                                                                                                 */
/*        AssertLink(pcParentEntity,pcRelatedResource,pcParentFieldNames).                                                                                                                                                                */
/*        oRelation = new Relation(pcParentEntity,pcRelatedResource,pcParentFieldNames,pcChildFieldnames,pCardinality,pcAliasName).                                                                                                       */
/*        Links:Add(oRelation).                                                                                                                                                                                                           */
/*        return oRelation.                                                                                                                                                                                                               */
/*    end method.                                                                                                                                                                                                                         */
/*                                                                                                                                                                                                                                        */
    
     method protected IDatasetRelation AddLink(phRelation as handle, pCardinality as RelationTypeEnum):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,pCardinality).
        Links:Add(oRelation). 
        return oRelation.       
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, pcDisableWebMethod as char):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,pcDisableWebMethod).
        Links:Add(oRelation). 
        return oRelation.       
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, pcDisableWebMethod as char, pCardinality as RelationTypeEnum):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,pcDisableWebMethod,pCardinality).
        Links:Add(oRelation). 
        return oRelation.       
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, pcDisableWebMethods as char extent):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,pcDisableWebMethods).
        AddLink(oRelation).
        return oRelation.      
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, pcDisableWebMethods as char extent, pCardinality as RelationTypeEnum):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,pcDisableWebMethods,pCardinality).
         AddLink(oRelation).
        return oRelation.      
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, poMethod as IMethod):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,poMethod).
         AddLink(oRelation).
        return oRelation.       
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, poMethods as IMethod extent):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,poMethods).
        Links:Add(oRelation). 
        return oRelation.       
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, poMethod as IMethod, plDeactivation as logical):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,poMethod,plDeactivation).
        AddLink(oRelation).
        return oRelation.       
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, poMethods as IMethod extent, plDeactivation as logical):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,poMethods,plDeactivation).
        Links:Add(oRelation). 
        return oRelation.       
    end method.
    
    
    method protected IDatasetRelation AddLink(phRelation as handle, poMethod as IMethod,pcDisableWebMethod as char):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,poMethod,pcDisableWebMethod).
        AddLink(oRelation).   
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, poMethod as IMethod,pcDisableWebMethods as char extent):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,poMethod,pcDisableWebMethods).
        AddLink(oRelation).
        return oRelation.       
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, poMethods as IMethod extent,pcDisableWebMethods as char extent):
        define variable oRelation as IDatasetRelation no-undo.
        oRelation = new DatasetRelation(phRelation,poMethods,pcDisableWebMethods).
        AddLink(oRelation).
        return oRelation.       
    end method.
    
    method protected IDatasetRelation AddLink(phRelation as handle, poMethods as IMethod extent,pcDisableWebMethods as char extent,pCardinality as RelationTypeEnum ):
        define variable oRelation as IDatasetRelation no-undo.
        
        oRelation = new DatasetRelation(phRelation,poMethods,pcDisableWebMethods,pCardinality).
        AddLink(oRelation).
        return oRelation.       
    end method.
    
    method protected void AssertDefaultDatasetTopBufferSameName():
        if Datasets[1]:serialize-name <> DatasetUtil:GetTopBuffer(Datasets[1], 1):serialize-name then
             undo, throw new IllegalOperationError(subst("Dataset[1] serialize-name '&1' does not have a single top buffer with same serialize-name. Top buffer serialize-name '&2'",
                                                   Datasets[1]:serialize-name,
                                                   DatasetUtil:GetTopBuffer(Datasets[1], 1):serialize-name      
                                                         )
                                                   ). 
    end method.
    
    method protected handle GetAndAssertRelationParent(pcParent as char):
        define variable i       as integer no-undo.
        define variable hParent as handle no-undo.       
        
        do i = 1 to extent(Datasets):
            hParent = DatasetUtil:GetBufferBySerializeName(Datasets[i], pcParent).
            if valid-handle(hParent) then
                leave. 
            // the child relation is validated agains the service in the Relation constructor  
        end.
        
        if not valid-handle(hParent) then 
            undo, throw new IllegalArgumentError(subst("Parent name '&1' in relation is not a valid internal entity reference",pcParent)).   
        return hParent.
    end.    
    
    method protected void AssertLink(pcParent as character,pcChild as character,pcFields as character extent):
        define variable hParent as handle no-undo.
        hParent = GetAndAssertRelationParent(pcParent).
        AssertLink(hParent,pcChild,pcFields).    
    end method.
    
    method protected void AssertLink(phParent as handle,pcChild as character,pcFields as character extent):
        define variable i       as integer no-undo.
        define variable hFld    as handle no-undo.
        
        define variable cBadFieldTempl as character no-undo
            init "Field '&1' in relation from parent '&2' to '&3' is not a valid field in the parent".
        
        if pcChild = Resource then 
            undo, throw new IllegalArgumentError("Cannot add link to self"). 
        
        do i = 1 to extent(pcFields):
            hFld = phParent:buffer-field(pcFields[i]) no-error.
            if not valid-handle(hFld) then
                undo, throw new IllegalArgumentError(subst(cBadFieldTempl,pcFields[i],phParent:serialize-name,pcChild)).   
        end.                    
            
    end method.
    
    method protected void BeforeFetchData(input-output poRequest as Pmfo.Core.BusinessLogic.IGetDataRequest, input-output dataset-handle phDataset):
    end method.    
    
    method protected void AfterFetchData(input-output pResponse as Pmfo.Core.BusinessLogic.IGetDataResponse, input-output dataset-handle phDataset):
    end method.    
    
    method protected IBusinessEntity GetBusinessEntityForDataset(phDataset as handle, pcResource as character):
        define variable oDatasetManager   as IDatasetUpdateManager no-undo.
        define variable hBuffer           as handle no-undo.
        oDatasetManager = GetDatasetUpdateManager(phDataset).
        hBuffer = DatasetUtil:GetBufferByTableSerializeName(phDataset, pcResource).
        return oDatasetManager:GetBusinessEntity(hBuffer).  
    end method.    
    
    method protected IBusinessEntity GetBusinessEntityForDataset(phBuffer as handle):
        define variable oDatasetManager   as IDatasetUpdateManager no-undo.
        oDatasetManager = GetDatasetUpdateManager(phBuffer:dataset).
        return oDatasetManager:GetBusinessEntity(phBuffer).  
    end method.    
    
    // use to fill the BE when the tt or ds is not needed immediately/directly
    method public void GetData():
        define variable hDataset as handle no-undo.
        hdataset = DefaultDataset.
        GetData(output dataset-handle hdataset by-reference).
    end method.
    
    // use to fill the BE when the tt or ds is not needed immediately/directly
    method protected logical GetData( input pQueryEntry as IQueryEntry):
        define variable oTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        define variable oRequest as Pmfo.Core.BusinessLogic.IGetDataRequest  no-undo.
        oTableRequest = new GetDataTableRequest(Datasets[1]:get-buffer-handle(1):table-handle:name).
        oTableRequest:AddQueryEntry(pQueryEntry).
        oRequest = new GetDataRequest(oTableRequest).
        return GetData(oRequest).
    end method.
    
    // for ABL requests for just the table
    method public logical GetData( input pTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest, output table-handle phTable):
        
        define variable oRequest as Pmfo.Core.BusinessLogic.IGetDataRequest  no-undo.
        if valid-handle(phTable) then 
            // The BE is dataset oriented - Use FindService  or one of the GetData with Dataset output  
            undo, throw new UnsupportedOperationError("GetData with IGetTableRequest parameter and valid handle in output table by-reference parameter. Use FindService:FindTable or one of the GetData methods with Dataset output if by-reference is needed.").  
        if pTableRequest:TableName <> Datasets[1]:get-buffer-handle(1):table-handle:name then 
            undo, throw new IllegalArgumentError(subst("TableRequest for table '&1'passed to entity for table '&2'",pTableRequest:TableName,Datasets[1]:get-buffer-handle(1):table-handle:name)).
        oRequest = new GetDataRequest(pTableRequest).
        
        this-object:Clear(). // clear for request for table 
        GetData(oRequest). 
        
        phTable = Datasets[1]:get-buffer-handle(1):table-handle.
        return phTable:has-records.     
    end method.
    
    method public logical GetData( input pQueryEntry as IQueryEntry, output table-handle phTable): 
        define variable oTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        define variable oRequest as Pmfo.Core.BusinessLogic.IGetDataRequest  no-undo.
        oTableRequest = new GetDataTableRequest(Datasets[1]:get-buffer-handle(1):table-handle:name).
        oTableRequest:AddQueryEntry(pQueryEntry).
        oRequest = new GetDataRequest(oTableRequest).
        return GetData(oTableRequest, output table-handle phTable by-reference).
    end method.
     
    // use to fill the BE when the tt or ds is not needed immediately/directly
    method protected logical GetData( input pTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest):
        define variable oRequest as Pmfo.Core.BusinessLogic.IGetDataRequest  no-undo.
        if pTableRequest:TableName <> Datasets[1]:get-buffer-handle(1):table-handle:name then 
            undo, throw new IllegalArgumentError(subst("TableRequest for table '&1' to entity for table '&2'",pTableRequest:TableName,Datasets[1]:get-buffer-handle(1):table-handle:name)).
        oRequest = new GetDataRequest(pTableRequest).
    
        return GetData(oRequest).
    
    end method.
    
    // use to fill the BE when the tt or ds is not needed immediately/directly
    method public logical GetData( input poRequest as Pmfo.Core.BusinessLogic.IGetDataRequest):
        define variable hDataset as handle no-undo.
        hdataset = DefaultDataset.
        GetData(poRequest,output dataset-handle hdataset by-reference).
      
        return hDataset:get-buffer-handle(poRequest:TableRequests[1]:tablename):table-handle:has-records.
    end method.
    
    // Only retrieve the unique records specified in the INamedArrayHolder 
    method public Pmfo.Core.BusinessLogic.IGetDataResponse GetRows(poArrayValues as INamedArrayHolder,  output dataset-handle phDataset ):
        return GetData(poArrayValues,true, output dataset-handle phDataset by-reference).  
    end method.
    
    // Retrieve the unique records specified in the INamedArrayHolder, but fill the dataset if it has other tables 
    method public Pmfo.Core.BusinessLogic.IGetDataResponse GetData(poArrayValues as INamedArrayHolder,  output dataset-handle phDataset ):
        return GetData(poArrayValues,false, output dataset-handle phDataset by-reference).  
    end method.
    
    method protected Pmfo.Core.BusinessLogic.IGetDataResponse GetData(poArrayValues as INamedArrayHolder, plOnlyRequstedTables as logical, output dataset-handle phDataset ):
        define variable oRequestBuilder        as FindManyRequestBuilder no-undo.
        define variable oRequest               as Pmfo.Core.BusinessLogic.IGetDataRequest no-undo.
        define variable oResponse as Pmfo.Core.BusinessLogic.IGetDataResponse no-undo. 
        define variable i as integer no-undo.
        define variable j as integer no-undo.
        define variable oTableRequest as character no-undo.
        define variable oQuerySelection as Ccs.BusinessLogic.IQueryGroup no-undo.
        define variable oQuery as QueryGroup no-undo.
        define variable hbuffer as handle no-undo.
        define variable hResolvedrelations as handle extent no-undo.
        
        oRequestbuilder = new FindManyRequestBuilder(Datasets[1]).
        
        oRequest = oRequestbuilder:Build(poArrayValues,plOnlyRequstedTables,phDataset,output hResolvedrelations).
        
        phDataset:empty-dataset ().
        // all parent keys are added to the request in order to use OR 
        // so no need to use relations  
        // TODO - there is an alternative approch to this which can be acieved in the datalayer by using before-fill 
        //        and appending the requested keys for each call from the parent
        //        This would require adding OR requests to a top level parent if more than one key is specified     
        do i = 1 to extent(hResolvedrelations):
            hResolvedrelations[i]:active = false.
        end.    
        oResponse = GetData(oRequest, output dataset-handle phDataset by-reference).
        do i = 1 to extent(hResolvedrelations):
            hResolvedrelations[i]:active = true.
        end.    
        
        // The first criteria is not in a group so build a query from those and use this to find
        do i = 1 to extent(oRequest:TableRequests) on error undo, throw:
            if valid-object(oRequest:TableRequests[i]:QueryDefinition) then
            do:
                oQuerySelection = cast(oRequest:TableRequests[i]:QueryDefinition:QuerySelection,Ccs.BusinessLogic.IQueryGroup).   
                hBuffer = DatasetUtil:GetBufferByTableName(phDataset,oRequest:TableRequests[i]:tablename).
                
                oQuery = new QueryGroup().
                do j = 1 to extent(oQuerySelection:Entries):
                    if oQuerySelection:Entries[j]:Join = JoinEnum:None then
                        oQuery:AddEntry(oQuerySelection:Entries[j]). 
                end.
                  
                do on error undo, throw:
                    hbuffer:find-unique(subst("where &1",oQuery:ToString())).
                    catch e as Progress.Lang.Error :
                        
                        if e:GetMessageNum(1) = 138 then
                            undo, throw new TableNotFoundError(hBuffer,oQuery ).
                        else 
                            undo, throw e.     
                    end catch.
                end.
                do j = 1 to extent(oQuerySelection:Entries):
                    if oQuerySelection:Entries[j]:Join = JoinEnum:Or then
                    do on error undo, throw:
                        hbuffer:find-unique(subst("where &1",oQuerySelection:Entries[j]:ToString())). 
                        catch e as Progress.Lang.Error :
                            if e:GetMessageNum(1) = 138 then
                                undo, throw new TableNotFoundError(hbuffer,oQuerySelection:Entries[j]). 
                            else 
                                undo, throw e.     
                        end catch.
                    end.
                end.
            end.
        end.
        return oResponse.       
    end method.      
    
    method public Pmfo.Core.BusinessLogic.IGetDataResponse GetData(  output dataset-handle phDataset ):
        define variable oRequest as Pmfo.Core.BusinessLogic.IGetDataRequest no-undo.
        define variable oResponse as Pmfo.Core.BusinessLogic.IGetDataResponse no-undo. 
         
        define variable oTableRequests as Pmfo.Core.BusinessLogic.IGetDataTableRequest extent no-undo.
        define variable i as integer no-undo.
        
        if not valid-handle(phDataset) then
        do:
            phdataset = DefaultDataset.
        end.    
        extent(oTableRequests) = phDataset:num-buffers.
        do i = 1 to phDataset:num-buffers:
            oTableRequests[i] = new GetDataTableRequest(phDataset:get-buffer-handle(i):table-handle:name).
        end.
            
            
        oRequest = new GetDataRequest(oTableRequests).
        this-object:Clear().
        oResponse = GetData(oRequest, output dataset-handle phDataset by-reference).
        if phdataset = DefaultDataset then
            AllDataIsFetched = true.
        return oResponse.
    end method. 
    
     
  
    method public final Object GetData( input poRequest as Pmfo.Core.BusinessLogic.IGetDataRequest, input dataset-handle phDataset ):
        define variable oGetResponse     as Pmfo.Core.BusinessLogic.IGetDataResponse no-undo.
        define variable oMethod          as Method no-undo.
        define variable oScope           as ILifecycleScope no-undo.
        define variable oDatasetManager  as IDatasetManager no-undo.
        define variable oParameterList   as ParameterList no-undo.
        define variable oObject          as Object no-undo.
        define variable cFunctionName    as character no-undo.
        oDatasetManager = PrepareFetch(poRequest, output oScope, input-output phDataset).
        oMethod = poRequest:GetClientMethod(this-object, phDataset, output oParameterList).
       
        if valid-object(oMethod) then
        do:
            if oMethod:ReturnTypeName = ? and Progress.Reflect.DataType:Void <> oMethod:ReturnType then
            do:
                // invoke
                if oMethod:ReturnExtent = 0 then 
                    oObject = PrimitiveHolderUtil:GetHolder(oMethod:Invoke(this-object, oParameterList)).
                else 
                    oObject = PrimitiveHolderUtil:GetArrayHolder(oMethod:Invoke(this-object, oParameterList)).
                
                return oObject.
                
            end.
            else do: 
                if oMethod:ReturnTypeName > "" then 
                    undo, throw new IllegalStateError(subst("The &1.&2 public function is defined as primitive and must return a primitive data type. The implementation returns &3",
                                                             Resource,
                                                             oMethod:name,
                                                             oMethod:ReturnTypeName   
                                                            )
                                                      ).
                else if Progress.Reflect.DataType:Void = oMethod:ReturnType then 
                    undo, throw new IllegalStateError(subst("The &1.&2 public function is defined as primitive and must return a primitive data type. The implementation is defined as void.",
                                                             Resource,
                                                             oMethod:name    
                                                        )
                                                      ).
                else if oMethod:ReturnExtent <> 0 then 
                    undo, throw new IllegalStateError(subst("The &1.&2 public function is defined as primitive and must return a single primitive data type. The implementation is defined as extent.",
                                                            Resource,
                                                           oMethod:name    
                                                      )
                                                   ).
                else // there should nopt be any more cases, but just to have catch all  
                     undo, throw new IllegalStateError(subst("The &1.&2 public function is defined as primitive and must return a single primitive data type",
                                                             Resource,
                                                             oMethod:name    
                                                      )
                                                   ).
                                             
            end.
                                                              
        end.
        else 
        do:
            if valid-object(poRequest:NamedQuery) then 
                cFunctionName =  poRequest:NamedQuery:name .
            else 
            do: 
                cFunctionName = cast(poRequest:GetTableRequest(phDataset:get-buffer-handle(1)),Pmfo.Core.BusinessLogic.IGetDataTableRequest):NamedQuery:name.     
            end.
            undo, throw new IllegalStateError(subst("The primitive &1/&2 public function is defined, but the physcial method was not found. Primitive functions must be implemented in the Business Entity.",
                                                     Resource,
                                                     cFunctionName  
                                                   )
                                              ).
        end.                                      
        catch e as Progress.Lang.Error :
            if e:GetMessageNum(1) = 13844 then 
            do:
                undo, throw new NotImplementedError(subst("Primitive support for return of datatype '&1' &2. Please Contact IT.",oMethod:ReturnType:ToString(), if oMethod:ReturnExtent > 0 then "as extent" else "")).  
                 
            end.
            else 
                undo, throw e.        
        end catch.     
        finally:
            cast(Application:ServiceManager,IServiceManager):stopServices(oScope,get-class(IDatasetGetManager)). 
        end finally. 
    end method.    
     
    method public Pmfo.Core.BusinessLogic.IGetDataResponse GetData( input poRequest as Pmfo.Core.BusinessLogic.IGetDataRequest, output dataset-handle phDataset ):
        define variable oGetResponse         as Pmfo.Core.BusinessLogic.IGetDataResponse no-undo.
        define variable oMethod              as Method no-undo.
        define variable oScope               as ILifecycleScope no-undo.
        define variable oDatasetManager      as IDatasetGetManager no-undo.
        define variable oParameterList       as ParameterList no-undo.
        define variable cClientInnerJoins    as character no-undo.
        define variable oClientSort          as IQuerySortEntry extent no-undo.
        define variable oIter                as IIterator no-undo.
        
        oDatasetManager = PrepareFetch(poRequest, output oScope, input-output phDataset).
        cClientInnerJoins = oDatasetManager:ClientInnerJoinTables.
        oClientSort       = oDatasetManager:ClientSort. 
        
        oMethod = poRequest:GetClientMethod(this-object, phDataset, output oParameterList).
         
        if valid-object(oMethod) then
        do:
            // the client method must return IGetDataResponse or be void 
            if oMethod:ReturnTypeName = get-class(Pmfo.Core.BusinessLogic.IGetDataResponse ):TypeName then
            do:
                oGetResponse = oMethod:Invoke(this-object, oParameterList). 
            end.
            else do:
                if oMethod:ReturnTypeName > "" then 
                      undo, throw new IllegalStateError(subst("The &1.&2 public method returns &3, but is defined as function and must return void or &4",
                                                               Resource,
                                                               oMethod:name,
                                                               oMethod:ReturnTypeName,
                                                               get-class(Pmfo.Core.BusinessLogic.IGetDataResponse ):TypeName   
                                                               )
                                                        ).
                oMethod:Invoke(this-object, oParameterList).
                oGetResponse = CreateDataResponse(poRequest).   
            end.
            // TODO this is messy
            if cast(poRequest,GetDataRequest):HasWarnings then 
            do: 
                oIter = cast(poRequest,GetDataRequest):Warnings:Iterator().
                do while oIter:HasNext():
                    AddWarning(oIter:Next():ToString()). 
                end.         
                cast(poRequest,GetDataRequest):Warnings:Clear().
                cast(poRequest,GetDataRequest):Warnings = ?.
            end.   
                            
        end.
        
        if valid-object(oGetResponse) = false then
        do:
            oGetResponse = FetchData(poRequest, input-output dataset-handle phdataset by-reference). 
        end.
        else if oGetResponse:RequestComplete = false then
        do: 
            oGetResponse:SetRequestProperties(poRequest).
            ReceiveData(new ReceiveDataEventArgs(poRequest,oGetResponse), phDataset).
        end.    
        if cClientInnerJoins > "" or extent(oClientSort) > 0 then
        do: 
            new DatasetInnerJoinManager():ProcessInnerJoin(DatasetUtil:GetBufferByTableName(phDataset,poRequest:TableRequests[1]:TableName), cClientInnerJoins, oClientSort).
        end.     
        
        if valid-object(oGetResponse) then
            AfterFetchData(oGetResponse, input-output dataset-handle phDataset by-reference).
        return oGetResponse.
        finally:
            cast(Application:ServiceManager,IServiceManager):stopServices(oScope,get-class(IDatasetGetManager)). 
        end finally.
    end method.
    
    /** Main preparation for GetData 
     - use interal dataset if not already defined 
     - start and return DatasetGetNanager 
     - call the manager process methiid 
     - call the beforeFetchdata hook
       
    ***/   
    method protected final IDatasetGetManager PrepareFetch( input poRequest as Pmfo.Core.BusinessLogic.IGetDataRequest, output pScope as ILifecycleScope, input-output phDataset as handle):
        define variable oDatasetManager  as IDatasetGetManager no-undo.
        define variable oRequestPlugin   as IPrepareGetRequestPlugin no-undo.
       
        WarningMessages = new WarningMessages(phdataset). 
       
        Assert:NotNull(poRequest,"GetDataRequest passed to GetData()").
        
        AllDataIsFetched = false. // not perfect
   
        if not valid-handle(phDataset) then
            phdataset = DefaultDataset.
        pScope = new LifecycleScope(phDataset).
        oRequestPlugin = GetPrepareRequestPlugin().
        if valid-object(oRequestPlugin) then 
            oRequestPlugin:PrepareGetRequest(poRequest, phDataset).
        PrepareGetRequest(poRequest,phDataset).
        oDatasetManager  = GetDatasetGetManager(pScope).
        oDatasetManager:ProcessDataset(this-object,phDataset, poRequest).
       
        BeforeFetchData(poRequest, input-output dataset-handle phDataset by-reference).
        return oDatasetManager.
    end method.
    
    // allow subclass to provide plugin for prepareRequest - for framework define as final in subclass 
    method protected IPrepareGetRequestPlugin GetPrepareRequestPlugin():
    end method.
    
    // allow subclass to provide plugin for validate of update request - for framework define as final in subclass
    method protected IValidateUpdateRequestPlugin GetValidateUpdateRequestPlugin():
    end method.
    
    // hook for subclass in use in instances - GetPrepareRequestPlugin
    method protected void PrepareGetRequest (poRequest as Pmfo.Core.BusinessLogic.IGetDataRequest, phDataset as handle):
    end method.
   
     
    // Note - do not add any code here! all logic must be in the delegate  
    // the delegate exists ti allow oither delagates to fetch data without call backs to this  
    method protected final Pmfo.Core.BusinessLogic.IGetDataResponse FetchData(poRequest as Pmfo.Core.BusinessLogic.IGetDataRequest,input-output dataset-handle phdataset):
        return FetchDelegate:FetchData(this-object, DataAccess, poRequest, input-output dataset-handle phdataset by-reference).
    end method.
    
     // hook for subclass - NOTE public - will be called in every instance started 
    method public void PrepareGetTableRequest (poRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest, phBuffer as handle):
           
    end method.
    
    method protected Pmfo.Core.BusinessLogic.IGetDataResponse CreateDataResponse(poRequest as IGetDataRequest):
        return FetchDelegate:CreateDataResponse(poRequest).
    end method.       
     
    method protected CommitScopeEnum GetCommitScope(pClientRequest as IClientRequest):
        return CommitScopeEnum:All.
    end method.
    
    method public handle GetDataset(pcCollections as character extent):
        return GetDataset(pcCollections,RowStateEnum:None).
    end method.
    
    // Get expanded dataset from an expand expression 
    // TODO the use of ClientGetRequest is a workaround 
    //     Should provide more direct support for this 
    //     from DatsetRouter and/or LinklExpander 
    //        (possibly by separating the expand logic to a reusable utility unless LinkExpander really is this utility)      
    //    
    method public handle GetExpandDataset(pcExpandExpression as character):
        define variable oClientRequest as IClientRequest no-undo.
        define variable oStringMap     as IStringStringMap no-undo.
        
        oStringMap = new StringStringMap().
        oStringMap:put("expand",pcExpandExpression).
        
        oClientRequest = new ClientGetRequest(Resource,oStringMap).
        
        return GetDataset(oClientRequest).
    end method.    
   
    method protected handle GetDataset(pcCollections as character extent, pRowStateEnum as RowStateEnum):
        define variable oMethod as IMethod no-undo.
        define variable oClientRequest as IClientRequest no-undo.
        define variable cCollectionList as character no-undo.
        cCollectionList = Array:ToString(pcCollections,"/").
        case pRowStateEnum:
            // todo add constrructor withoutr last param -  make sure it passed everywhere needed foirst
            when RowStateEnum:Created then 
               oClientRequest = new ClientUpdateRequest("POST",cCollectionList,new StringStringMap(),false). 
            when RowStateEnum:Modified then 
               oClientRequest = new ClientUpdateRequest("PUT",cCollectionList,new StringStringMap(),false).
            when RowStateEnum:Deleted then 
               oClientRequest = new ClientUpdateRequest("DELETE",cCollectionList,new StringStringMap(),false).
            when RowStateEnum:None then 
               oClientRequest = new ClientGetRequest(cCollectionList,new StringStringMap()).
            otherwise 
               undo, throw new UnsupportedOperationError(subst("GetDataset for RowStateEnum &1. Only plain CRUD states currently supported",pRowStateEnum)).    
        end case.     
        return GetDataset(oClientRequest).
    end method.
    
    // allow iverride ti load actions at request time
    method protected void LoadActions(pClientRequest as IClientRequest):
    end method.      
    
    // allow override to load fucntions at request time
    method protected void LoadFunctions(pClientRequest as IClientRequest):
    end method.      
    
    method public handle GetDataset(pClientRequest as IClientRequest):
        define variable oRouter        as DatasetRouter no-undo.
        define variable hBuildDatasets as handle extent no-undo.
        define variable i              as integer no-undo. 
        LoadActions(pClientRequest).
        LoadFunctions(pClientRequest).
        // Hack - TODO improve - maybe add a getDatasets that can be overridden  
        // LinkDataset was only used in one case where the default dataset is recursive 
        // and is not suited for expand when this was added
        // In the cases where the recirsive dataset is used we still call here 
        // but override GetREquestDataset to disallow expand and not use this 
        // dataset 
        // - a more complete soluition would likely be to allow map of function (method?) and 
        //   dataset to be used as top, Thisa copiuld probably replace spme pf the 
        // getRequestDataset and GetUpdateRequest overrides -but could also be more confusing...      
        
        if type-of(pClientRequest,IClientGetRequest) 
        and LinkDataset <> Datasets[1] 
        and (extent(pClientRequest:Collections) > 1 or cast(pClientRequest,IClientGetRequest):Expand > "") then 
        do:
            extent(hBuildDatasets) = extent(Datasets).
            hBuildDatasets[1] = LinkDataset.
            do i = 2 to extent(Datasets):
                hBuildDatasets[i] = Datasets[i].
            end.    
        end.    
        else 
            hBuildDatasets  = Datasets.
        
        oRouter = new DatasetRouter(Resource, 
                                    hBuildDatasets,
                                    if HasLink then Links else ?,
                                    if HasAction then Actions else ?, 
                                    if HasFunction then Functions else ?,
                                    GetFixedChildren(pClientRequest)). 
        return oRouter:ResolveRequest(pClientRequest).
        
    end method.
    
    method public handle GetRequestDataset(pClientRequest as IClientRequest):
        define variable hDataset as handle no-undo.
        
        hDataset = GetDataset(pClientRequest).
        
        // if this object supports nore than one servertype assert that the request is valid in the serviceManger server type
        // if there is only one servertype the fact that the entity was started in createService means that it is a match 
        //  report type needs to be asserted here since it is not checked in ServiceManger
        if num-entries(ServerType:ToString()) > 1 or valid-object(pClientRequest:ServiceMethod) or ServerTypeEnum:Report:equals(cast(Application:ServiceManager,IServiceManager):ServerType) then
        do:
            
            // The asserion is done after resolveRequest since this adds the serverMethod to the client request
            // The default assert only alows requests for Methods with matching server type in the nn defaul server type  
            AssertServerType(pClientRequest,hDataset).
        end.    
    //    if valid-object(pClientRequest:ServiceMethod)  type-of(IClientGetRequest,pClientRequest)
        // avoids memoryleaks when the returned dataset is dynamic ad the setter deletes the previous and by also deleting it in the destructor
        RequestDatasetHandle = hDataset.
        
        return hDataset.   
    end method. 
    
    method public void GetDataset( output dataset-handle phDataset ):
        phdataset = DefaultDataset.
    end method.
    
    method protected void DeepCopy(phhandle as handle, output table-handle phtable):
         phTable = phHandle:table-handle. 
    end method. 
    
    // 
    method protected handle GetEmptyCopy(phhandle as handle):
         define variable hTable as handle no-undo.
         DeepCopy(phHandle,output table-handle hTable).
         hTable:empty-temp-table ().
         return hTable. 
    end method. 
       
    
    // Return a separated list of resporces that is to be exposed as a common childlink to a BE 
    // Typically overridden in the application level BusinssEntity subclass 
    // For example a reportBE that may be used be any BE (Request Builder and Operation Handler will likely need 
    // dedicated logic for these)  
    // The class that defines the resource must implement IDynamicChild
    method public character GetFixedChildren(pClientRequest as IClientRequest):
        return "".
    end method.     
    
    // return a comma separated list of resources that should 
    // handle the request if referenced as a child in the URL when this BE is referenced without a key   
    // the referenced BE must implement .TBD .. setContext to passs the parent references 
    // it should not need to know the parenting buffer(s) only the name(s)   
   
    method public character GetContainerRedirect(pClientRequest as IClientRequest, output pcParentContext as character). 
        return "".     
    end method.
    
    // Return a separated list of resporces that is to be exposed as a common dataset to a BE for GET requests 
    // Typically overridden in the application level BusinssEntity subclass 
    // For example a reportBE that may be used be any BE  
    method protected character GetFixedDatasets():
        return "".
    end method.     
    
    /**
     Called after the request is resolved and pRequest:serverMethod is added 
     when the BE supports more than one ServerType. Single server type objects 
     are not returned from the ServiceManger:GetService that started this object, 
     so checking for one is unnecessary. 
       
     Default behavior is to allow all regular crud request in the defaukt serverype and 
     only allow actions or functions that has mathing ServerType with
     the ServiceManager server type in a non-default server type 
     
     Subclasses overrides this for more specific behavior
     */   
    method protected void AssertServerType(pRequest as IClientRequest, phDataset as handle):
        define variable ServiceServerType as ServerTypeEnum no-undo.
        
        ServiceServerType =  cast(Application:ServiceManager,IServiceManager):ServerType. 
                
        // regular crud - only allow default - the additional server type is for methods only
        // (override for different rule)
        if not valid-object(pRequest:ServiceMethod) then
        do:
            if not ServerType:IsFlagSet(ServiceServerType)  then
            do:
                undo, throw  new ForbiddenRequestError(subst("'&1' of '&2' is not allowed on &3 Server &4",
                                                              pRequest:Method,
                                                              pRequest:Collections[extent(pRequest:Collections)],
                                                              ServiceServerType:ToString(),
                                                              GetValidServerTypeMethodsMessage(pRequest, ServiceServerType,phDataset, true)
                                                              )
                                                        ).
            end.

        end.
        else
        do:
           
            if not pRequest:ServiceMethod:ServerType:IsFlagSet(ServiceServerType) then
                undo, throw  new ForbiddenRequestError(subst("'&1' of '&2' &3 '&4' is not allowed on &5 server &6",
                                                             pRequest:Method,
                                                             pRequest:Collections[extent(pRequest:Collections) - 1],
                                                             if type-of(pRequest:ServiceMethod,IAction) then "Action" else 'Function',
                                                             pRequest:ServiceMethod:Name,
                                                             ServiceServerType:ToString(),
                                                             GetValidServerTypeMethodsMessage(pRequest, ServiceServerType, phDataset, true)
                                                             )
                                                        ).
        end.
    end method.
    
   
    // helper method for AssertServerType 
    method protected character GetValidServerTypeMethodsMessage(pRequest as IClientRequest,pServerType as ServerTypeEnum, phDataset as handle, plPeriods as logical):
        define variable cNames    as character extent no-undo.
        define variable cTypename as character no-undo.
        define variable cMsg      as character no-undo.
        define variable cResource as character no-undo.
        define variable oBe       as IBusinessEntity no-undo.
        define variable hRequestedbuffer as handle no-undo.
       
        if valid-object(pRequest:ServiceMethod) then 
            cResource = pRequest:Collections[extent(pRequest:Collections) - 1].
        else 
            cResource = pRequest:Collections[extent(pRequest:Collections)].
        
        if cResource <> Resource then
        do:
            hRequestedBuffer = DatasetUtil:GetBufferBySerializeName(phDataset,cResource).
            if valid-handle(hRequestedBuffer) then 
            do on error undo, throw:
                oBe = cast(Application:ServiceManager,IServiceManager):CreateBusinessEntity(hRequestedBuffer:table-handle:serialize-name).
                catch e as Progress.Lang.Error :
                   // this may be valid     
                end catch.
            end.
            // if this is not succesful then just return blank - this may be a valid condition    
            if not valid-object(oBe) then 
                return "".    
        end.        
        else 
            oBe = this-object.
        
        if type-of(pRequest,IClientGetRequest) then
        do: 
            cNames = GetMethodNames(oBe:GetServerTypeFunctions(pServerType)).
            cTypeName = "Function".
        end.
        else do:
            cNames = GetMethodNames(oBe:GetServerTypeActions(pServerType)).
            cTypeName = "Action".
        end.     
        if extent(cnames) = 1 then 
        do:
            cMsg = subst("Only &1 '&2' is allowed on &3 Server",cTypeName,cnames[1],pServerType:toString()).
        end.         
        else if extent(cnames) > 1 then 
        do:
            cMsg = subst("Only &1s &2 are allowed on &3 Server",cTypeName,
                                                               // true = quoter
                                                               StringUtil:GetMessageFromEntries(Array:ToString(cnames),true),
                                                               pServerType:toString()).
        end.
        if cMsg > "" and plPeriods then 
            return(subst(". &1.",cMsg)).
       
        return cMsg.
    end method.
    
    method protected character extent GetMethodNames(pMethods as IMethod extent):
        define variable cNames as character extent no-undo.
        define variable i as integer no-undo.
        extent(cNames) = extent(pMethods).
        do i = 1 to extent(cNames):
            cNames[i] = pMethods[i]:name.
        end.     
        return cnames.
    end method.
    
    method public IAction extent GetServerTypeActions(pServerType as ServerTypeEnum): 
        return cast(GetServerTypeMethods(pServerType, Actions:Values),IAction).
    end method.
    
    method public IFunction extent GetServerTypeFunctions(pServerType as ServerTypeEnum): 
        return cast(GetServerTypeMethods(pServerType, Functions:Values),IFunction).
    end method.
    
    method protected IMethod extent GetServerTypeMethods(pServerType as ServerTypeEnum, pMethods as ICollection): 
        define variable oIter   as IIterator  no-undo.
        define variable oObject as Object no-undo.
        define variable oCollection as ICollection no-undo.
        oCollection = new Collection().
        oIter = pMethods:Iterator().
       
        do while oIter:HasNext():
            oObject = oIter:Next().
            if cast(oObject,IMethod):ServerType:IsFlagSet(pServerType) then
                oCollection:Add(oObject).
        end.
        return cast(oCollection:ToArray(),IMethod).
    end method. 
       
    method public handle GetUpdateRequestDataset(pClientRequest as IClientRequest,output plBeforeImageRequired as logical, output pUpdateRequest as IUpdateDataRequest).
       
        define variable hDataset     as handle no-undo.
        define variable Parameters   as IParameter extent no-undo.
        define variable hMainBuffer  as handle no-undo.
        define variable oRequestType as RequestTypeEnum no-undo.
        define variable hRelation    as handle no-undo.
        define variable iRel         as integer no-undo.
        plBeforeImageRequired = (pClientRequest:Method = "PUT").
        
        hDataset = GetRequestDataset(pClientRequest). 
        if valid-object(hDataset) then
        do: 
       
            if valid-object( pClientRequest:ServiceMethod ) then 
            do:
                hMainBuffer = GetRequestedBuffer(hDataset, pClientRequest:Collections, true).
                oRequestType = cast(pClientRequest:ServiceMethod,IAction):PrepareClientRequest(hMainBuffer:table-handle:name,pClientRequest).
                plBeforeImageRequired = cast(pClientRequest:ServiceMethod,IAction):BeforeImageRequired.
            end.
            else 
                oRequestType = if pClientRequest:KeyWheres[extent(pClientRequest:KeyWheres)] = "" 
                               then RequestTypeEnum:SingleTopTable  
                               else RequestTypeEnum:SingleRow. 
            
            if not valid-handle(hMainbuffer) then
            do:
                hMainBuffer = GetRequestedBuffer(hDataset, pClientRequest:Collections, false).
            end.     
            pUpdateRequest = CreateUpdateDataRequest(pClientRequest,hMainBuffer,oRequestType). 
            
            if valid-object(pClientRequest:ServiceMethod)  then
                cast(pUpdateRequest,UpdateDataRequest):CustomRequest = pClientRequest:ServiceMethod.
            
            cast(pUpdateRequest,UpdateDataRequest):CommitScope = GetCommitScope(pClientRequest).    
            
        end.  
       
        return hDataset.
    
    end method. 
    
    // Use collection path to find buffer - the serialize-name may not be unique in the dataset
    // There may for example be many notePages where the parent is unique   
    method protected handle GetRequestedBuffer(phDataset as handle, pcCollections as character extent, plHasMethod as logical):
        define variable hBuffer as handle no-undo.
        define variable iRequestedLevel as integer no-undo.
        
        hbuffer = DatasetUtil:GetBufferBySerializeName(phDataset,pcCollections[1]).
        
        if not valid-handle(hBuffer) then 
            undo, throw new NotFoundError(subst("Entity &1",pcCollections[1]),phDataset:serialize-name).
        
        iRequestedLevel = extent(pcCollections) - (if plHasMethod then 1 else 0).
        
        if iRequestedLevel > 1 then 
        do: 
            return GetRequestedChildBuffer(hBuffer,pcCollections,2,plHasMethod).
        end.
        else 
            return hBuffer. 
    end.      
    
    method protected handle GetRequestedChildBuffer(phParent as handle, pcCollections as character extent, piStart as integer, plHasMethod as logical):
        define variable i               as integer no-undo.
        define variable iRequestedLevel as integer no-undo.
        define variable hRelation       as handle no-undo.
        define variable iChild          as integer no-undo.
        
        iRequestedLevel = extent(pcCollections) - (if plHasMethod then 1 else 0).
        
        do iChild = 1 to phParent:num-child-relations:
            hRelation = phParent:get-child-relation (iChild).
            if hRelation:child-buffer:serialize-name = pcCollections[piStart] then
            do:
                if iRequestedLevel > piStart then 
                    return GetRequestedChildBuffer(hRelation:child-buffer, pcCollections, piStart + 1, plHasMethod ).
                else 
                    return hRelation:child-buffer.
            end.
        end.
        undo, throw new NotFoundError(subst("Entity &1",pcCollections[piStart]),pcCollections[piStart - 1]).
    end.      
    
    method public IRelation GetLink(pcname as character):
        return GetLink(Resource,pcName).
    end method.    
    
    method public IRelation GetLink(pcParent as character, pcname as character):
        define variable oIterator as IIterator no-undo.
        define variable oRelation as IRelation no-undo.
        if HasLink then
        do:
            oIterator = Links:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                if  oRelation:ParentName = pcParent
                and oRelation:PublicName = pcName then 
                    return oRelation.
            end.
        end.  
        return ?.          
    end method.    
    
    method private void DeepCopy(input phFromDataset as handle, output dataset-handle phDataset):
        phDataset = phFromDataset.
        // the fromdataset will be deleted 
    end method.     
    
    method public IGetResultCountResponse GetResultCount( input poRequest as Ccs.BusinessLogic.IGetDataRequest ):
        undo, throw new NotImplementedError("Method getResultCount").
    end method.
    
    method protected Pmfo.Core.BusinessLogic.IGetDataRequest CreateGetRequestForRefresh(poUpdateRequest as IUpdateDataRequest,phDataset as handle):
        define variable i as integer no-undo.
        define variable oGetTableRequests as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo extent.
        extent(oGetTableRequests) = phDataset:num-buffers. 
        do i = 1 to phDataset:num-buffers:
            oGetTableRequests[i] = new GetDataTableRequest(phDataset:get-buffer-handle(i):table-handle:name).
        end.    
        return new GetDataRequest(oGetTableRequests).
    end method.    
    
    method public Pmfo.Core.BusinessLogic.IGetDataRequest CreateGetRequest():
        return CreateGetRequest(Datasets[1]).
    end method.    
    
    method protected Pmfo.Core.BusinessLogic.IGetDataRequest CreateGetRequest(phDataset as handle):
        define variable i as integer no-undo.
        define variable oGetTableRequests as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo extent.
        
        extent(oGetTableRequests) = phDataset:num-buffers. 
        do i = 1 to phDataset:num-buffers:
            oGetTableRequests[i] = new GetDataTableRequest(phDataset:get-buffer-handle(i):table-handle:name).
        end.    
        return new GetDataRequest(oGetTableRequests).
    end method.
    
    method public Object UpdateData( input-output dataset-handle phDataset):
        return UpdateData(input-output dataset-handle phDataset by-reference,CreateUpdateDataRequest(phDataset)).
    end method.
     
    method public Object UpdateData( input-output dataset-handle phDataset, input poUpdateDataRequest as IUpdateDataRequest ):
        
        define variable oResponse as Object no-undo.
        define variable hWarning   as handle no-undo.
        
        define variable oGetRequest as Pmfo.Core.BusinessLogic.IGetDataRequest no-undo.
        define variable lError as logical no-undo.
        define variable oReceiveError as Error no-undo.
        define variable oError as Error no-undo.
        define variable oMethod as Method no-undo.
        define variable oParameterList as ParameterList no-undo.
        define variable oDatasetManager  as IDatasetUpdateManager no-undo.
        define variable oScope as LifecycleScope no-undo.
        define variable oValidateRequestPlugin as IValidateUpdateRequestPlugin no-undo.
            
        Assert:NotNull(phdataset,"Dataset passed to UpdateData").
        Assert:NotNull(poUpdateDataRequest,"UpdateDataRequest passed to UpdateData").
     
        AllDataIsFetched = false. // as for now any update is assumed to mess with the concept of having all data  
        SuccessMessages = ?. // will be lazy loaded if referenced 
        WarningMessages = new WarningMessages(phdataset). // needs dataset for conversion of field names to public names  
        do on error undo, throw :
            oScope = new LifecycleScope(phDataset).
            /** start the dataset Update manager that fires update validation hooks */
            oDatasetManager = GetDatasetUpdateManager(oScope).
            do on error undo, throw:
                oDatasetManager:ProcessDataset(this-object,phDataset, poUpdateDataRequest).
                catch eval as ValidationError:
                    if phDataset:error then 
                        lError = true.
                    // just throw validation failed if no dataset errors     
                    else do:
                        undo, throw eval.
                    end.           
                end catch.
            end.
            // last check for the update request (after all BEs needed on client have been started) 
            if not lError then
            do:
                oValidateRequestPlugin = GetValidateUpdateRequestPlugin().
                if valid-object (oValidateRequestPlugin) then
                    oValidateRequestPlugin:Validate(poUpdateDataRequest,phDataset). 
            end.
            
            // standard crud call before uopdate (actions are handled in ValidateAction to ensure they are called also from other BEs
            if valid-object(poUpdateDataRequest:CustomRequest) = false or type-of(poUpdateDataRequest:CustomRequest,IAction) = false then
            do: 
                // override by application level subclass - 
                if lError = false then
                do on error undo, throw: 
                    BeforeUpdateDataset( poUpdateDataRequest, phDataset).
                    if phDataset:error then
                        lError = true.
                    catch eval as ValidationError:
                        if phDataset:error then 
                            lError = true.
                         // just throw validation failed if no dataset errors     
                        else 
                            undo, throw eval.        
                    end catch.                  
                end.
            end.
            /* to hook into the events for read after the update we need a get request -  
               we fire these also when error 
            */   
            oGetRequest = CreateGetRequestForRefresh(poUpdateDataRequest,phDataset).
            if not phDataset:error then
            do: 
                oMethod = poUpdateDataRequest:GetClientMethod(this-object, phDataset, output oParameterList).
                if valid-object(oMethod) then
                do on error undo, throw: 
                    if oMethod:ReturnType = Progress.Reflect.DataType:Object then  
                        oResponse = oMethod:Invoke(this-object,oparameterList).
                    else     
                        oMethod:Invoke(this-object,oparameterList).
                    lError = phDataset:error.
                    catch eval as ValidationError:
                        if phDataset:error then 
                            lError = true.
                        else if Datasets[1]:error then 
                        do:
                           phDataset:error = true. 
                           lError = true.    
                        end.   
                        // just throw validation failed if no dataset errors     
                        else 
                           undo, throw eval.        
                    end catch. 
                end.
                else do:
                    oResponse = SubmitData(poUpdateDataRequest, input-output dataset-handle phDataset by-reference).
                end.    
                if valid-object(oResponse) and type-of(oResponse,String) then
                do:
                    // bug worksround - cannot detect error in if caller is by-reference (need more research) 
                    if cast(oResponse,String):Value = "error" then
                    do:
                         phDataset:error = true.
                         lError = true.
                    end.   
                end.
                
            end.
            else 
                lError = true.
            catch e as Progress.Lang.Error :
                oError = e.
                lError = true.     
            end catch.
        end.       
        do on error undo, throw:
         
            // this is called also after an error, but if an error occurs here it will be ignored
            // since lError will be prioritized belew 
            ReceiveData(new ReceiveDataEventArgs(oGetRequest,new GetDataResponse(),lError),phDataset).
            catch e2 as Progress.Lang.Error :
                oReceiveError = e2.
            end catch.
        end. 
        
        if valid-object(oError) then
            undo, throw oError.
        
       
        if phDataset:error then  
            ThrowDatasetError(phDataset).
        
        if not lError then 
        do:
            if valid-object(oReceiveError) then
                AddWarning(subst("Data was saved, but an error occurred while refreshing data. &1",oReceiveError:GetMessage(1))).     
                
            hWarning = cast(WarningMessages,IWarningMessageTableHolder):TableHandle.
             // add child warnings to the warning table 
            
            oDatasetManager:GetChildWarnings(output table-handle hWarning append). 
            if valid-object(poUpdateDataRequest:CustomRequest)
            and type-of(poUpdateDataRequest:CustomRequest,IAction) 
            and valid-object(cast(poUpdateDataRequest:CustomRequest,IAction):SuccessRowState) then
            do:
                PrepareSuccessMessages(cast(poUpdateDataRequest:CustomRequest,IAction),poUpdateDataRequest:MainRequestedBuffer, phDataset).          
            end. 
        end.   
        
        return oResponse.        
        
        finally:
            cast(Application:ServiceManager,IServiceManager):stopServices(oScope,get-class(IDatasetUpdateManager)). 
        end finally.     
    end method.
    
    // allow ooverride for complex relations 
    // as of current this is only used by RequestRouter   
    // - throws error if single key cannot be resolved
    // - return expression as-is if not single key 
    method public character ConvertSingleKeyToExpression (phbuffer as handle, pcExpression as character).
        if pcExpression > "" then
            return ServiceInfo:ConvertSingleKeyToExpression(phbuffer, pcExpression, true, true).
        else 
            return "".    
    end method.
    
    // allow subclasses to check if submit failed taking account for the workaround 
    // to deal with not reliably being able to get dataset:error 
    method protected logical SubmitResponseOk(poResponse as Object, phDataset as handle):
        if valid-object(poResponse) and type-of(poResponse,String) then
        do:
            if cast(poResponse,String):Value = "error" then 
                return false. 
        end.
        if phdataset:error then 
            return false. 
        return true.             
    end method.    
    
    method protected void PrepareSuccessMessages(pAction as IAction, pRequestedBuffer as character, phDataset as handle):
        define variable hBuffer as handle no-undo.
        define variable hQuery  as handle no-undo.
        if valid-object(pAction:SuccessRowState) then
        do:
            hBuffer = phDataset:get-buffer-handle(pRequestedbuffer).
            create query hQuery.
            hQuery:add-buffer(hBuffer).
            hQuery:query-prepare (subst("for each &1",hBuffer:name)).
            hQuery:query-open ().
            hQuery:get-first.
            
            do while hBuffer:avail: 
               SuccessMessages:Add(new SuccessMessage(Resource,ServiceInfo:GetPublicKey(hBuffer),pAction:SuccessRowState,pAction:name)). 
               hQuery:get-next. 
            end.    
        end.
        finally:
            if valid-handle(hQuery) then 
                delete object hQuery. 
        end. 
    end method.    
     
    // separated out so that BE actions can call it without dealing with managing warnings  
    // final - this is absolutely not overridable - it is the single entry point for BE to DA  
    method protected final Object SubmitData( input poUpdateDataRequest as IUpdateDataRequest, input-output dataset-handle phDataset):
        define variable oReturn    as Object no-undo.
        define variable hWarning   as handle no-undo.
        define variable hRelation  as handle no-undo.
        define variable iRel       as integer no-undo.
        define variable iBuffer    as integer no-undo.
        define variable hBuffer    as handle no-undo.
        define variable cNavTables as character no-undo.
        define variable oBe        as IBusinessEntity no-undo.
        define variable hInfrastructureDataset as handle no-undo.
        define variable oDatasetUpdateManager as IDatasetUpdateManager no-undo.
       
        // Use of BufferError outside managed hooks may end here if not managed  
        // (this is considered part of the framework, as it allows BufferError to be used also outside of 
        // the validate hooks
        if phDataset:error then
            ThrowDatasetError(phDataset).

        // do not start DS for inactive relations ir clientonly children
        // TODO - check recursive (difficult when non active) ?  
        do iRel = 1 to phDataset:num-relations:
            hRelation = phDataset:get-relation(IRel).
            if hRelation:active = false then 
                poUpdateDataRequest:AddNoTargetTable(hRelation:child-buffer:table-handle:name).
            else do:
                // et existing - we do not want to start the manager if this is not called from UpdateData (wil cause a leak)
                oDatasetUpdateManager = GetExistingDatasetUpdateManager(phDataset).
                if valid-object(oDatasetUpdateManager) then 
                    // GetBusinessEntity upresses not found from Service if the be is not in its cache and does not exist
                    oBe = oDatasetUpdateManager:GetBusinessEntity(hRelation:child-buffer).
                else do on error undo, throw:
                    oBe = cast(Application:ServiceManager,IServiceManager):CreateBusinessEntity(hRelation:child-buffer:table-handle:serialize-name).
                    catch e as NotFoundError :
                      // this is not considered an error in this case 
                      // there may be tempt tables that does not have a BE     
                    end catch.
                end.      
                if valid-object(oBe) and oBe:ClientOnly then 
                    poUpdateDataRequest:AddNoTargetTable(hRelation:child-buffer:table-handle:name).
            end.         
        end.
          
        if valid-object(poUpdateDataRequest:Action) = false then 
        do:  
            if poUpdateDataRequest:MainRequestedBuffer > "" then 
            do :
                hBuffer = phDataset:get-buffer-handle (poUpdateDataRequest:MainRequestedBuffer).
                // get nav tables (URL parent references with no data in the body ) 
                // as we do not want to check id current on server
                // (may not have all data)
                // don't throw error (for now?) if not valid  
                if valid-handle(hBuffer) then 
                    cNavTables = DatasetUtil:GetParentTableNames(hBuffer). 
            end.
            // add read-only tables to update request list of tables to check are current. 
            // If fhey are part of the submit they are very likely important and need to be current  
            do iBuffer = 1 to phDataset:num-buffers:
                hBuffer = phDataset:get-buffer-handle(iBuffer).
                if valid-handle(hBuffer:before-buffer) 
                and hBuffer:table-handle:has-records
                and hBuffer:before-buffer:table-handle:has-records = false 
                and lookup(hBuffer:table-handle:name,cNavTables) = 0 then 
                    poUpdateDataRequest:AddCheckReadOnlyTable(hBuffer:table-handle:name).
            end.
        end. 
        
        if valid-object(WarningMessages) then
            hWarning = cast(WarningMessages,IWarningMessageTableHolder):TableHandle.
       
        if IsInfrastructureServiceStarted then 
            hInfrastructureDataset = InfrastructureService:dataset.
        
        oReturn = DataAccess:SubmitData(poUpdateDataRequest, input-output dataset-handle phDataset, input dataset-handle hInfrastructureDataset, output table-handle hWarning append).        
        
        return oReturn.
        
        finally:
            if IsInfrastructureServiceStarted then 
                delete object InfrastructureService.
                    
        end finally.
         
         
    end method. 
    
    method public void ThrowDatasetError( phDataset as handle): 
        define variable oJson as JsonObject no-undo.
        define variable oErrorProcessor as JsonDatasetErrorProcessor no-undo.
        define variable oError as Error no-undo.
        define variable iRelation as integer no-undo.
        
        oJson = new JsonObject().
        do iRelation = 1 to phDataset:num-relations:
            // ensure keys are in json so they can be added to error 
            // as of current the dataset is not returne for errors 
            // we may return datasets for 409 in futrure , but the assumption is that it causes no isses since it will always be correct 
            // TODO  use parent relation  
            phDataset:get-relation (iRelation):foreign-key-hidden = false.
        end.       
        phDataset:write-json ("JsonObject",oJson,no,?,no,yes, yes).
        oErrorProcessor = new JsonDatasetErrorProcessor().
            
        if DatasetUtil:HasBeforeState(phDataset,row-deleted) then 
            oError = oErrorProcessor:CreateDeleteError (ojson, phdataset).
        else 
            oError = oErrorProcessor:CreateError (ojson, phdataset).
        
        if valid-object(oError) then
        do:
            undo, throw oError .
        end.
        else do:
            // this should no longer happen since JsonDatasetErrorProcessor now should manage all cases that ended up here before  
            oError = new ApplicationError("Update of data failed, but no error information was returned to the client. Please Contact IT.").
            undo, throw oError. 
        end.   
    end method.    
    
    method protected IUpdateDataRequest CreateUpdateDataRequest(pClientRequest as IClientRequest,pMainRequestedBuffer as handle,pRequestType as RequestTypeEnum):
         define variable cConfirmations as character no-undo.
         if type-of(pClientRequest,IClientUpdateRequest) then 
             cConfirmations = cast(pClientRequest,IClientUpdateRequest):Confirmations.
             
         return new UpdateDataRequest(pMainRequestedBuffer:name,pRequestType,cConfirmations).
    end method.    
    
    // default update request for dataset 
    method protected IUpdateDataRequest CreateUpdateDataRequest(phDataset as handle):
        define variable hMainBuffer as handle no-undo.
        define variable oUpdateRequest as UpdateDataRequest no-undo.
        if DatasetUtil:GetNumTopBuffers(phDataset) > 1 then 
            undo, throw new IllegalArgumentError("Dataset with more than one top table").
        hMainBuffer =  DatasetUtil:GetTopBuffer(phDataset, 1).  
        oUpdateRequest = new UpdateDataRequest(hMainbuffer:name,RequestTypeEnum:MultiTable).
        oUpdateRequest:CommitScope = CommitScopeEnum:All.
        return oUpdateRequest.
    end method.
    
    /**
    Adapt an object to Pmfo and current dataset.  
    **/
    method protected Object Adapt(pObject as Object, phDataset as handle):
        if type-of(pObject,IUpdateDataRequest) then
            return pObject.
        if type-of(pObject,Ccs.BusinessLogic.IUpdateDataRequest) then
            return new UpdateDataRequestAdapter(cast(pObject,Ccs.BusinessLogic.IUpdateDataRequest),phDataset).
        undo, throw new UnsupportedOperationError(subst("Adapt &1",pObject:GetClass():TypeName)).    
    end method.    
    
    method override public void Initialize(  ): 
        define variable oFunction as IFunction no-undo.
        oFunction = Addfunction("Count").
        oFunction:IsPrimitive = true.
        oFunction:Binding = MethodBindingEnum:Collection.
    end method.
    
    method public int64 Count(input pTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest):
        define variable oRequest as Pmfo.Core.BusinessLogic.IGetDataRequest  no-undo.
        define variable hDataset as handle no-undo.
        if pTableRequest:TableName <> Datasets[1]:get-buffer-handle(1):table-handle:name then 
            undo, throw new IllegalArgumentError(subst("TableRequest for table '&1' to entity for table '&2'",pTableRequest:TableName,Datasets[1]:get-buffer-handle(1):table-handle:name)).
        
        hDataset = Datasets[1].
        oRequest = new GetDataRequest(pTableRequest).
        this-object:Clear().
        return this-object:Count(oRequest, input dataset-handle hDataset by-reference). 
    end method.
    
    method public int64 Count():
        define variable oTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest no-undo.
        oTableRequest = new GetDataTableRequest(Datasets[1]:get-buffer-handle(1):table-handle:name).
        return this-object:Count(oTableRequest).
    end method.     
    
    method public int64 Count(input poRequest as Pmfo.Core.BusinessLogic.IGetDataRequest, input dataset-handle phDataset):
        define variable hBuffer as handle no-undo.
        define variable oTableRequest as GetDataTableRequest no-undo.
        define variable cBadOptions as character no-undo.
        define variable hCountField as handle no-undo.
        define variable oResponse as Pmfo.Core.BusinessLogic.IGetDataResponse no-undo.
        define variable oTableResponse as Pmfo.Core.BusinessLogic.IGetDataTableResponse no-undo.
         
        hBuffer = DatasetUtil:GetBufferByTableSerializeName(phdataset,Resource).
        if not valid-handle(hBuffer) then 
            undo, throw new NotImplementedError(subst("&1/Count",Resource)).
        
        // We could in theory use $count as the name, but we do not do this for any of the other odata props  (as is supported n odata v4) 
        // But it seems rather odd if a table should have field with this name (count implies more than one whihc maes no sense on a rfecord level)
        // If there really is a need for a count proeprry then the assumtion is that you do not need to suppirt count as a function
        // (The query option can al;ways be used)    
        
        // it is bit wrong to check the internal field   
        hCountField = hBuffer:buffer-field("count") no-error.
         
        if valid-handle(hCountField) then 
            undo, throw new UnsupportedOperationError("Function &1/Count. The internal schema has a count field and cannot exewcute the count function.").
        
        oTableRequest = cast(poRequest:GetTableRequest(hBuffer:table-handle:name),GetDataTableRequest).
        
        // odata disallows d$top, $skip, $orderby, $expand, and $format with the path suffix /$count. The result of such a request is undefined.
        // in addition we also disallow select and count - they make no sense for ABL and we will hijack them for the serverside count       
        // TODO take inot accont that the name of the query properties in theory are configurable (IClientGetRequest has *Prop properties)  
        if oTableRequest:NumRecords > 0 then 
            StringUtil:AppendEntry(cBadOptions,"top").
        if oTableRequest:Count = true then 
            StringUtil:AppendEntry(cBadOptions,"count").
        if oTableRequest:SelectFields > "" then 
            StringUtil:AppendEntry(cBadOptions,"select").
        if oTableRequest:Skip > 0 then 
            StringUtil:AppendEntry(cBadOptions,"skip").
        if oTableRequest:SortExpression > "" then 
            StringUtil:AppendEntry(cBadOptions,"orderBy").
        if oTableRequest:SortExpression > "" then 
            StringUtil:AppendEntry(cBadOptions,"expand").
        
        if cBadoptions > "" then 
            undo, throw new IllegalOperationError(subst("Query option(s) &1 is/are not allowed with Function &2/Count",
                                                        StringUtil:GetMessageFromEntries(cBadOptions),
                                                        Resource
                                                        )
                                               ).
        oTableRequest:Count = true.
        oTableRequest:SelectFields = "count". // this is useful if the server has optimization to skip logic fir expensive fields
        //  no need for named query on server
        oTableRequest:NamedQuery = ?.
        
        oResponse  = FetchData(poRequest, input-output dataset-handle phdataset by-reference).
        oTableResponse = cast(oResponse:GetTableResponse(hBuffer:table-handle:name),Pmfo.Core.BusinessLogic.IGetDataTableResponse).
        return oTableResponse:NumResults.
       
    end method.     
    
    method public void SubscribeDataReceived(pEntity as IBusinessEntity, pcQualifiedName as character):    
        pEntity:DataReceived:Subscribe(this-object:ReceiveData).  
        SubscribeName = pcQualifiedName.
    end method.
    
    method public void UnsubscribeDataReceived():    
        define variable oReceiveDataEventArgs as IReceiveDataEventArgs no-undo.
        oReceiveDataEventArgs = new ReceiveDataEventArgs(?,?).
        oReceiveDataEventArgs:Failed = true.
        // publish the failes event args thru the subscribed tree 
        ReceiveData(oReceiveDataEventArgs,?).
    end method.
    
    method protected void UnsubscribeDataReceived(pParentEntity as IBusinessEntity):
        pParentEntity:DataReceived:Unsubscribe(this-object:ReceiveData).
        SubscribeName = "".
    end method.     
    
    method protected void OnDataReceived(pReceiveDataArgs as IReceiveDataEventArgs, phDataset as handle).
         this-object:DataReceived:publish (this-object,pReceiveDataArgs, phDataset).
    end method.
    
    // IFetchDelegator - NOT in IBusinessEntity
    method public final void ReceiveData(pReceiveDataArgs as IReceiveDataEventArgs, phDataset as handle):
        ReceiveData(?, pReceiveDataArgs, phDataset). 
    end method.  
    
    // IFetchDelegator - NOT in IBusinessEntity
    method public void AddWarning(pcMsg as character):
        AddWarning(pcMsg,"").
    end method. 
    
    method protected void AddWarning(pcMsg as character, pcField as character):
        if not valid-object(WarningMessages) then 
            undo, throw new UnsupportedOperationError("AddWarning called outside request scope"). 
        WarningMessages:Add(Resource, pcMsg, pcField).
    end method. 
   
    method protected void ReceiveData(pParent as IBusinessEntity, pReceiveDataArgs as IReceiveDataEventArgs, phDataset as handle):
        define variable hBuffer as handle no-undo.
        define variable cTable  as character no-undo.
        define variable Htable  as handle no-undo.
        
        if not pReceiveDataArgs:Failed 
        then
        do:
            if valid-object(pParent) and SubscribeName > "" then
            do:
                hBuffer = DatasetUtil:GetBufferByPathName(phDataset, SubscribeName).
            end.
            else
                // the temp-table serialize-name may be an alias so use the table-handle to identify this from the resource
                hBuffer = DatasetUtil:GetBufferBySerializeName(phDataset, Resource).
             
            if valid-handle(hBuffer) then
            do:
                hTable = hBuffer:table-handle.
                cTable = hTable:name.
                
                ReceiveBuffer(pParent,pReceiveDataArgs:Request:GetTableRequest(cTable),pReceiveDataArgs:Response:GetTableResponse(cTable), hBuffer).
            end.
        end.
        
        this-object:DataReceived:publish (this-object,pReceiveDataArgs, phDataset).
       
        if valid-object(pParent) then
        do:
            UnsubscribeDataReceived(pParent).
        end.
            
    end method.    
    
    // this existsa only to cast to Pmfo.core request/response  
    method protected final void ReceiveBuffer( pParent as IBusinessEntity, pRequest as IGetDataTableRequest,pResponse as IGetDataTableResponse, phBuffer as handle):
        ReceiveBuffer(pParent,cast(pRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest),cast(pResponse,Pmfo.Core.BusinessLogic.IGetDataTableResponse),phBuffer).    
    end method.
    
    method protected void ReceiveBuffer( pParent as IBusinessEntity, pRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest,pResponse as Pmfo.Core.BusinessLogic.IGetDataTableResponse, phBuffer as handle).
    end method.  
    
    // this is called in every entity that maps to a temp-table in the dataset 
    method public final void BeforeUpdateData(pUpdateDataArgs as IUpdateDataEventArgs, phBuffer as handle):
       
        if valid-handle(phBuffer) then
        do:
            // There are two levels of hooks since we don't want to have hooks that must call super
            // The first hook is intended for application level subclass i.e before update behaviuor common for all business entities 
            // of an application that usese the framework   
            BeforeUpdateBuffer(pUpdateDataArgs:Request, phBuffer).
            // the validatebuffer hook intended for a concrete instances
            // application specific subclasses may override this to call hooks with static table
            // do not call for custom request Action (of needed add a ValidateAction)
            ValidateBuffer(pUpdateDataArgs, phBuffer).
        end.
    end method.  
    
    // override by application level subclass 
    method protected void BeforeUpdateBuffer( pUpdateRequest as IUpdateDataRequest, phBuffer as handle):
    end method.
   
    // call validateTable event hook (not really an event as we are not publishing ) 
    method protected final void ValidateBuffer(pRequest as IUpdateDataEventArgs, phBuffer as handle).
        define variable hTable as handle no-undo.    
        define variable cAction as character no-undo.
        define variable lIsAction as logical no-undo.
        define variable hLocalBuffer as handle no-undo.
        hTable = phBuffer:table-handle.
        lisAction = valid-object(pRequest:Request:CustomRequest) and type-of(pRequest:Request:CustomRequest,IAction).
        
        do on error undo, throw:
             /** Call hook for static table   
                 There is no way to define a super method for a static temp-table so we must use dynamic-invoke  
              **/
            if lisAction then
                cAction = "ValidateAction".
            else
                cAction = "ValidateTable".
             
            dynamic-invoke (this-object,cAction,pRequest, table-handle hTable by-reference ).
            
            catch e1 as ValidationError:
                // when ThrowBufferError is used in validationTable the buffer does not have a dataset.
                // sso add the ewerror here
                if phBuffer:error then 
                    phBuffer:dataset:error = true.
                else do:
                    // the error may have been added to the buffer of the defined dataset 
                    if Datasets[1]:error then 
                        phBuffer:dataset:error = true.
                      
                end.        
                undo, throw e1.        
            end.    
            catch e as Progress.Lang.Error :
                // don't throw error if the method does not exist  
                if e:GetMessageNum(1) <> 14457 then
                    undo, throw e.     
            end catch.
        end.
        if  ValidateRequiredFields // defaults to false as of current 
        and lIsAction = false 
        and phbuffer:error = false then 
            ValidateMandatoryFields(pRequest,phbuffer).
            
    end method. 
    
    method protected final void ValidateMandatoryFields(pRequest as IUpdateDataEventArgs, phBuffer as handle).
        define variable hQuery     as handle no-undo.
        define variable hFld       as handle no-undo.
        define variable hBeforeFld as handle no-undo.
        define variable i          as integer no-undo.
        define variable hBefore    as handle no-undo.
        
        hBefore = phbuffer:before-buffer.
        create query hQuery.   
        hQuery:add-buffer(phbuffer).
        hQuery:query-prepare (subst("for each &1 where row-state(&1) = row-created or row-state(&1) = row-modified",phBuffer:name)).
        hQuery:query-open ().
        hQuery:get-first.
        do while phBuffer:avail:
            hBefore:find-by-rowid(phBuffer:before-rowid).
            do i = 1 to phbuffer:num-fields:
                hFld = phBuffer:buffer-field(i).
                if phbuffer:row-state = row-modified then
                    hBeforeFld =  hBefore:buffer-field(hFld:name).
                if phbuffer:row-state = row-created
                or hfld:buffer-value <> hBeforeFld:buffer-value then
                do: 
                    if ServerFields > "" and lookup(hfld:name,ServerFields) > 0 then
                    do:
                        if hFld:default-value = ? and hFld:buffer-value <> ? then
                            BufferError(phBuffer,subst("&1 is not editable. It will be assigned a value on server.",GetLabel(hFLd)),hfld:name). 
                    end.                 
                    else if (hFld:default-value <> ? or lookup(hFld:name,RequiredFields) > 0) and hFld:buffer-value = ? then
                    do:        
                        if hFld:serialize-hidden = true then
                        do:
                            undo, throw new ApplicationError(subst("Entity Field &1 cannot be submitted with null. Please Contact IT.",hFld:name)).  
                        end.
                        else do:
                            case hFld:data-type:
                                when "character" or when "longchar" then
                                    BufferError(phBuffer,subst("&1 is required, please enter a valid value",GetLabel(hFLd)),hfld:name).
                                when "integer" or when "int64" then
                                    BufferError(phBuffer,subst("&1 is required and cannot be empty, please enter a valid integer value",GetLabel(hFLd)),hfld:name).
                                when "decimal" then
                                    BufferError(phBuffer,subst("&1 is required and cannot be empty, please enter a valid number value",GetLabel(hFLd)),hfld:name).
                                otherwise
                                    BufferError(phBuffer,subst("&1 is required and cannot be empty, please enter a valid value",GetLabel(hFLd)),hfld:name).
                               
                            end.
                        end.
                    end.
                end.
                    
            end.    
            hQuery:get-next.
        end.
        finally:
            if valid-object(hQuery) then
                delete object hQuery.
        end.    
    end method. 
    
    method protected character GetLabel(pfld as handle):
        if pFld:label <> pFld:name then 
            return pFld:label.
        else 
            return StringUtil:CamelToWords(pFld:serialize-name).  
    end method.     
    
    // no default behavior - override by application level subclass  
    method protected void BeforeUpdateDataset( pUpdateRequest as IUpdateDataRequest, phDataset as handle):
    end method.
    
    method protected void BufferError(phBuffer as handle,pcMsg as character):
        // the ErrorStringUtil only works  with changed records that has error attributes
        // TODO - add error object and collection that can provide the same capabilities 
        // - cellect many errors and return table, key and field info   
        if phBuffer:row-state = 0 then 
            undo, throw new BadRequestError(pcMsg).
               
        ErrorStringUtil:AddError(phBuffer, pcMsg).
    end method.    
    
    method protected void BufferError(phBuffer as handle,pcMsg as character, pcField as character):
        // the ErrorStringUtil only works  with changed records that has error attributes 
        if phBuffer:row-state = 0 then 
            undo, throw new BadRequestError(pcMsg).
        ErrorStringUtil:AddError(phBuffer, pcMsg, pcField).
    end method.    
    
    method protected void ThrowBufferError(phBuffer as handle,pcMsg as character):
        // the ErrorStringUtil only works  with changed records that has error attributes 
        if phBuffer:row-state = 0 then 
            undo, throw new BadRequestError(pcMsg).
        ErrorStringUtil:AddError(phBuffer, pcMsg).
        undo, throw new ValidationError().
    end method. 
    
    method protected void ThrowBufferError(phBuffer as handle,pcMsg as character, pcField as character):
        // the ErrorStringUtil only works  with changed records that has error attributes 
        if phBuffer:row-state = 0 then 
            undo, throw new BadRequestError(pcMsg).
        ErrorStringUtil:AddError(phBuffer, pcMsg, pcField).
        undo, throw new ValidationError().
    end method. 
    
    method protected integer DefineQuestion (pId as integer):
            // TODO: depretate the APIs with numbers
        // this is to support the old api that used map and the user could pass any number   
        
        if pid <= Questions:Size then 
        do:
            if type-of(Questions:Get(Pid),IQuestion) then 
                undo, throw new IllegalArgumentError(subst("Question is already defined with Id &1",pid)).
            Questions:Add(pid, new Question(pid)).
        end.    
        else do:
            do while questions:size + 1 lt pid:
                Questions:Add(new Object()).   
            end.
            Questions:Add(new Question(pid)).
        end.        
        return Questions:size. 
    end method.
    
    method protected integer DefineQuestion (pId as integer, pcMessage as char):
          // TODO: depretate the APIs with numbers
        // this is to support the old api that used map and the user could pass any number   
        
        if pid <= Questions:Size then 
        do:
            if type-of(Questions:Get(Pid),IQuestion) then 
                undo, throw new IllegalArgumentError(subst("Question is already defined with Id &1",pid)).
            Questions:Add(pid, new Question(pid, pcMessage)).
        end.    
        else do:
            do while questions:size + 1 lt pid:
                Questions:Add(new Object()).   
            end.
            Questions:Add(new Question(pid,pcMessage)).
        end.        
        return Questions:size. 
    end method.
    
    method protected integer DefineQuestion (pcMessage as char):
        Questions:Add(new Question(Questions:size + 1,pcMessage)).
        return Questions:size. 
    end method.
    
    method protected integer DefineQuestion ():
        Questions:Add(new Question(Questions:size + 1)).
        return Questions:size. 
    end method.
    
    method protected void AddAndThrowBufferQuestion(phBuffer as handle, pId as integer, pMessage as character):
        define variable iMin as integer no-undo.
        define variable iMax as integer no-undo.
        if phBuffer:dataset:error   then      
            undo, throw new ValidationError(). 
        if pid < 1 then 
            undo, throw new IllegalArgumentError(subst("Question Id &1 is invalid. The number must be greater than 0.",pId)). 
        
        ErrorStringUtil:AddQuestion(phBuffer, pId + ReservedConfirmationNumbers, pMessage).
         
        undo, throw new ValidationError(). // UpdateData gets the errors added by ErrorString to the dataset  if dataset:error  is true 
    end method.
    
    method protected logical IsQuestionConfirmed(pId as integer,pUpdateRequest as IUpdateDataRequest):
        define variable iMin as integer no-undo.
        define variable iMax as integer no-undo.
        define variable cConfirmationNumbers as character no-undo.
        cConfirmationNumbers = pUpdateRequest:GetConfirmedQuestions(Resource). 
        if cConfirmationNumbers > "" then 
            return lookup(string(pid + ReservedConfirmationNumbers),cConfirmationNumbers) > 0.
        else 
            return false.  
    end.
    
    method protected IQuestion GetQuestion(pId as integer):
        return cast(Questions:Get(Pid),IQuestion).
        catch e as Progress.Lang.Error :
            undo, throw new IllegalOperationError(subst("Question with id &1 is not defined",pId),e).
        end catch.
    end method.    
    
    method protected IGetDataQuestion NewDataQuestion(pId as integer,phBuffer as handle, pcMessage as character):
        return new GetDataQuestion(subst("&1-&2",phBuffer:serialize-name,pid + ReservedConfirmationNumbers),pcMessage).
    end method.
    
   
    // Ask Question using the predefined message
    method protected logical AskQuestion(pId as integer, phbuffer as handle, pUpdateRequest as IUpdateDataRequest):
        define variable oQuestion as IQuestion no-undo.
        
        if IsQuestionConfirmed(pId,pUpdateRequest) then 
            return true.
        // if there already are regular errors then throw them first 
        if phbuffer:dataset:error   then      
            undo, throw new TransactionError().
             
        oQuestion = GetQuestion(pId). 
        if oQuestion:Message = "" then     
            undo, throw new IllegalOperationError(subst("Question &1 has no predefined message. Use AskConfirmation with parameter",pId)).
        AddAndThrowBufferQuestion(phbuffer, pid, oQuestion:Message).  
    end method.
    
    // Ask numbered Question using the specified message parameter
    method protected logical AskQuestion(pId as integer, pMessage as character,phbuffer as handle, pUpdateRequest as IUpdateDataRequest ):
        define variable oQuestion as IQuestion no-undo.
        if IsQuestionConfirmed(pId,pUpdateRequest) then 
            return true.
        if phbuffer:dataset:error   then      
            undo, throw new TransactionError(). 
        oQuestion = GetQuestion(pId). 
        if oQuestion:Message > "" then     
            undo, throw new IllegalOperationError(subst("Use of AskQuestion with message parameter for Question &1 with predefined with message",pId)).
        AddAndThrowBufferQuestion(phbuffer, pid, pMessage).
    end method.
    
    // Ask numbered Question using the character arry as substitute sfor the defined message 
    method protected logical AskQuestion(pId as integer, pcParams as character extent,phbuffer as handle, pUpdateRequest as IUpdateDataRequest):
        define variable oQuestion as IQuestion no-undo.
        define variable cMessage  as character no-undo.
        if IsQuestionConfirmed(pId,pUpdateRequest) then 
            return true.
            
        if phbuffer:dataset:error   then    
            undo, throw new TransactionError(). 
        oQuestion = GetQuestion(pId). 
        if oQuestion:Message = "" then     
            undo, throw new IllegalOperationError(subst("Use of AskQuestion with extent message parameter for Question &1 with no predefined message",pId)).
         
        cMessage = Array:SubstituteArray(oQuestion:Message, pcParams).
        
        AddAndThrowBufferQuestion(phbuffer, pid, cMessage).
    end method.
    
    // Add Question to get request 
    method protected logical AddQuestion(pId as integer, phbuffer as handle, pGetDataRequest as Pmfo.Core.BusinessLogic.IGetDataRequest):
        define variable oGetDataQuestion as IGetDataQuestion no-undo.
        define variable oQuestion        as IQuestion no-undo.
        
        oQuestion = GetQuestion(pId). 
        if oQuestion:Message = "" then     
            undo, throw new IllegalOperationError(subst("Question &1 has no predefined message. Use AddQuestion with parameter",pId)).
        
        return pGetDataRequest:Questions:Add(NewDataQuestion(pid,phBuffer,oQuestion:Message)).
    end method.    
    
    // Ask numbered Question using the specified message parameter
    method protected logical AddQuestion(pId as integer, pMessage as character,phbuffer as handle, pGetDataRequest as Pmfo.Core.BusinessLogic.IGetDataRequest):
        define variable oQuestion as IQuestion no-undo.
       
        oQuestion = GetQuestion(pId). 
        if oQuestion:Message > "" then     
            undo, throw new IllegalOperationError(subst("Use of AddQuestion with message parameter for question &1 with predefined with message",pId)).
        
        return pGetDataRequest:Questions:Add(NewDataQuestion(pid,phBuffer,pMessage)).
        
    end method.
    
     // Ask numbered Question using the character arry as substitute sfor the defined message 
    method protected logical AddQuestion(pId as integer, pcParams as character extent,phbuffer as handle, pGetDataRequest as Pmfo.Core.BusinessLogic.IGetDataRequest):
        define variable oQuestion as IQuestion no-undo.
        define variable cResolvedMessage as character no-undo.
        
        oQuestion = GetQuestion(pId). 
       
        if oQuestion:Message = "" then     
            undo, throw new IllegalOperationError(subst("Use of AddQuestion with extent message parameter for Question &1 with no predefined message",pId)).
        
        cResolvedMessage = Array:SubstituteArray(oQuestion:Message, pcParams).
       
        return pGetDataRequest:Questions:Add(NewDataQuestion(pid,phBuffer,cResolvedMessage)).
        
    end method.
    
    method protected void AddSuccessMessage(phBuffer as handle,pcMessage as character):
        SuccessMessages:Add(new SuccessMessage(Resource,ServiceInfo:GetPublicKey(phBuffer),pcMessage)).
    end method.
        
    // override to use a JsonWriter instead of json-write
    method public IJsonWriter GetJsonWriter(pRowState as RowStateEnum, pcServerMethodName as character, pcCollections as character extent).
        return ?.
    end method.
    
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method override public void Destroy(  ):
        if valid-handle(RequestDatasetHandle) and RequestDatasetHandle:dynamic then
        do: 
            delete object RequestDatasetHandle no-error.
        end.  
        if mFunctionsCreated then
            delete object Functions no-error.
        if mActionsCreated then
            delete object Actions no-error.
        if mLinksCreated then
            delete object Links no-error.        
         if mRedirectCreated then
            delete object RedirectMap no-error.                 
    end method.
    
    method override public character ToString(  ):
        define variable cResource as character no-undo.   
        define variable i as integer no-undo.
        define variable iTop as integer no-undo. 
        define variable j as integer no-undo.
        cResource = Resource.
        do i = 1 to extent(Datasets):
            cResource = subst("&1 Ds[&2]: &3",cResource,i,Datasets[i]:serialize-name ).
            iTop = DatasetUtil:GetNumTopBuffers(Datasets[i]).
            do j = 1 to iTop:
                // may throw error if called when some buffers are being deleted   
                cResource = subst("&1 Top[&2]: &3",cResource,j,DatasetUtil:GetTopBuffer(Datasets[i], j):serialize-name) no-error.
            end.
        end.

        return subst("&1 (&2)",cResource,super:ToString()).  

    end method.
    
    method public void Clear():
        define variable i as integer no-undo.
        do i = 1 to extent(Datasets):
            Datasets[i]:empty-dataset ().
        end.   
        AllDataIsFetched = false.   
    end. 
    
    // overide to call any stuff needed by catalog that is lazy loaed (actions, functions)
    method public void PrepareCatalog().
    end method.
	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	destructor public BusinessEntity ( ):
        Destroy().
	end destructor.
    
end class.