 
 /*------------------------------------------------------------------------
    File        : DataAccess
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 25 10:41:26 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.CommitScopeEnum from propath.
using Ccs.BusinessLogic.IGetDataRequest from propath.
using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using Ccs.BusinessLogic.INamedQuery from propath.
using Ccs.Common.Application from propath.
using OpenEdge.BusinessLogic.UpdateDataRequest from propath.
using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.BusinessLogic.ActionScopeEnum from propath.
using Pmfo.Core.BusinessLogic.GetDataRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataResponse from propath.
using Pmfo.Core.BusinessLogic.GetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.GetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.UpdateDataRequestAdapter from propath.
using Pmfo.Core.Common.DataService from propath.
using Pmfo.Core.Common.IWarning from propath.
using Pmfo.Core.Common.LifecycleScope from propath.
using Pmfo.Core.DataLayer.DataSource from propath.
using Pmfo.Core.DataLayer.IDataAccess from propath.
using Pmfo.Core.DataLayer.IDataSource from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.
using Pmfo.Core.DataLayer.Update.ITransactionManager from propath.
using Pmfo.Core.DataLayer.Update.NestedTransactionManager from propath.
using Pmfo.Core.DataLayer.Update.TransactionManager from propath.
using Pmfo.Core.DataLayer.Util.IHandleDataSourceMap from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Error.ConflictError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.LockTimeOutError from propath.
using Pmfo.Core.Error.NotFoundError from propath.
using Pmfo.Core.Error.QueryTimeOutError from propath.
using Pmfo.Core.Error.RefreshError from propath.
using Pmfo.Core.Error.RequestTimeOutError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Core.Error.ValidationError from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Core.Manager.ISessionManager from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Util.DatasetUtil from propath.
using Pmfo.Util.StringUtil from propath.
using Progress.Lang.Error from propath.
using Progress.Lang.Object from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
using Pmfo.Core.DataLayer.IInfrastructureDataAccess from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.Collections.ICollection from propath.
 

class Pmfo.Core.DataLayer.DataAccess abstract inherits DataService implements IDataAccess: 
    {Pmfo/Core/schema/warning.i}
    /* Event published before update starts. 
       DataSources may subscribe to this for example to change row-state  
    */
    define public event BeforeUpdate signature void (input dataset-handle phDataset). 

     /* Event published after update is done. 
       DataSources may subscribe to this for example check or change errors   
     */
    define public event AfterUpdate signature void (input dataset-handle phDataset). 
    
     
    define public abstract property Resource as character no-undo  
       get.
    
    define protected property DatasetHandle as handle get. 
       set(ph as handle):
           if valid-handle(DatasetHandle) and DatasetHandle <> ph then
               delete object DatasetHandle no-error.
           DatasetHandle = ph.    
       end.
   
    method override public void Destroy(  ):
    end method.
    
    method override public void Initialize(  ):
        
    end method.
    
    method protected IHandleDataSourceMap GetDataSourceMap(phHandle as handle):
        return cast(cast(Application:ServiceManager,IServiceManager):getService(get-class(IHandleDataSourceMap), new LifecycleScope(phHandle)),IHandleDataSourceMap). 
    end.    
    
    method protected void DestroyDataSources(phHandle as handle):
        cast(Application:ServiceManager,IServiceManager):stopServices(new LifecycleScope(phHandle)). 
    end.    
    
    method public IGetDataResponse FetchData( input poRequest as IGetDataRequest, input-output dataset-handle phDataset ):
        define variable oIter              as IIterator   no-undo.
        define variable oDataSourceMap     as IHandleDataSourceMap no-undo.
        define variable oDataSources       as IDataSource extent no-undo.
        
        define variable oTableResponses    as GetDataTableResponse  extent no-undo.
        define variable i                  as integer no-undo. 
        define variable iStopAfter         as integer no-undo.
      
        DatasetHandle = phDataset. // setter and destructor avoid memory leak
        
        oDataSourceMap = StartDataSources(poRequest,phDataset).
        
        if type-of(poRequest,Pmfo.Core.BusinessLogic.IGetDataRequest) then 
            iStopAfter = cast(poRequest,Pmfo.Core.BusinessLogic.IGetDataRequest):timeout no-error.
        
        if iStopAfter = 0 or iStopAfter = ?  then  
            iStopAfter = cast(Application:SessionManager,ISessionManager):QueryTimeout.
            
        do stop-after iStopAfter:
            if valid-object(poRequest:NamedQuery) then 
            do:
                this-object:Fill(phDataset,poRequest:NamedQuery).
            end.
            else do:
                this-object:Fill(phDataset).      
            end.
        end.
        oDataSources = oDataSourceMap:Sources().
        extent(oTableResponses) = extent(oDataSources).
        do i = 1 to extent(oDataSources).
            oTableResponses[i] = new GetDataTableResponse(oDataSources[i]:DataBuffer:table-handle:name).
            if oDataSources[i]:NumRecords > 0 then 
            do:
                oTableResponses[i]:NextPagingContext = oDataSources[i]:NextRowKey.
            end.
            if oDataSources[i]:TotalCount > 0 then 
            do:
                oTableResponses[i]:NumResults = oDataSources[i]:TotalCount.
            end.
        end.   
           
        return new GetDataResponse(oTableResponses).          
        
        catch eStopAfter as Progress.Lang.StopAfter:
            undo, throw new QueryTimeOutError(eStopAfter,iStopAfter).        
        end catch.
        
        catch eConflictStop as Progress.Lang.LockConflict :
            undo, throw new LockTimeOutError("query",eConflictStop).    
        end catch.
        
        catch eStop as Progress.Lang.StopError :
            undo, throw new ApplicationError("Stop condition encountered during fetch of data. Errors may have been written to log. Please Contact IT",eStop).    
        end catch.

        finally:
            DestroyDataSources(phDataset).
        end finally.     
    end method.
    
    method protected void Fill(phDataset as handle):
        define variable StringKey as String no-undo.
        define variable oSource  as DataSource no-undo.
        define variable lOk as logical no-undo.
        lok = phdataset:fill().
        if not lok then do:
           phDataset:empty-dataset.
           ThrowFillErrors(phDataset).
        end. 
    end method.  
    
    method protected void ThrowFillErrors (phDataset as handle):
        define variable oDataSources   as IDataSource extent no-undo.
        define variable oIterator      as IIterator no-undo.
        define variable oErrorIterator as IIterator no-undo.
        define variable oError         as Error no-undo.
        define variable oManyError     as ApplicationError no-undo.
        define variable iCount         as integer no-undo.
        define variable cMsg           as character no-undo.    
        define variable i as integer no-undo.
           
        oDataSources = GetDataSourceMap(phDataset):Sources().
        
        cMsg = subst("Read of &1 failed",if phDataset:serialize-name > "" then quoter(phDataset:serialize-name) else "data").
        
        oManyError = new ApplicationError(cMsg).
        do i = 1 to extent(oDataSources):
            oErrorIterator = oDataSources[i]:FillErrors:Iterator().
            do while oErrorIterator:HasNext():
               oError = cast(oErrorIterator:Next(),Error).
               iCount = iCount + 1.
               oManyError:AddMessage(oError:GetMessage(1),oError:GetMessageNum(1)).
            end.
        end.
        if iCount = 1 then
        do:
            if type-of(oError,ApplicationError) then 
                undo, throw new ApplicationError(subst("&1. &2",cMsg,cast(oError,ApplicationError):GetStrippedMessage()),oError).
            else   
                undo, throw new ApplicationError(subst("&1. &2",cMsg,oError:GetMessage(1)),oError).
        end.    
        else if iCount = 0 then
            undo, throw new ApplicationError(subst("&1. &2",cMsg,"The errors have been written to the log. Please Contact IT.")).
        else  
            undo, throw oManyError.   
    end method.    
    
    // override to handle named query
    method protected void Fill(phDataset as handle,pquery as INamedQuery ):
        this-object:fill(phdataset).
    end method.  
 
    method protected IHandleDataSourceMap StartSaveSources(poRequest as IUpdateDataRequest,phDataset as handle):
        define variable hBuffer         as handle      no-undo.
        define variable i               as integer     no-undo.
        define variable cNoTargetTables as character   no-undo.
        define variable oUpdateTarget   as IUpdateTarget no-undo.
        define variable oDataSource     as IDataSource no-undo.
        define variable oDataSources    as IHandleDataSourceMap no-undo.
        
        oDataSources = GetDataSourceMap(phDataset).
        if type-of(poRequest,Pmfo.Core.BusinessLogic.IUpdateDataRequest) then
            cNoTargetTables = poRequest:NoTargetTables.     
        
        
        do i = 1 to phDataset:num-buffers:
            oUpdateTarget = ?.
            oDataSource   = ?.
            hBuffer = phDataset:get-buffer-handle(i).
             
            // This is too drastic - there are DS objects that retrieves other DS from GetDataSourceMap and they do not check if the class is valid
            // and they should no need to - 
            // The use of the map would need to be replaced with a class that can start sources as is done when using the client data manager 
            // BUT the startup is more complex here since prepare depends on parent          
            /// if (hbuffer:table-handle:has-records or (valid-handle(hbuffer:table-handle:before-table) and hbuffer:table-handle:before-table:has-records)) 
            // and 
            if cNoTargetTables = "" or lookup(hbuffer:table-handle:name,cNoTargetTables) = 0 then 
            do: 
                do on error undo, throw: 
                    oUpdateTarget = CreateSaveSource(hBuffer:table-handle:serialize-name).
                    catch e as Progress.Lang.Error :
                        // Ignore error could be a datasource - try again below. 
                        // As of current IUpdateTarget inherit IDataSources so if it is not
                        // found here it would not be found below either, but keep the independency 
                        
                    end catch.
                end.
                
                // Current DataSource implements both IDataSoyrce and IUpdateTargetsm 
                // but keep this code so that they could be separate interfaces in future  
                // We still want to start IDataSoirce since it would be a parent of \
                // updatetargets if in this dataset              
                if valid-object(oUpdateTarget) then 
                do:
                    if poRequest:IsActionTarget(hBuffer) = false then
                    do: 
                        BeforeUpdate:Subscribe(oUpdateTarget:BeforeUpdate).
                        AfterUpdate:Subscribe(oUpdateTarget:AfterUpdate).
                    end.
                    
                    oUpdateTarget:PrepareSave(hBuffer,poRequest).
                    oDataSource = oUpdateTarget.
                end.    
                
                if not valid-object(oDataSource) then
                    oDataSource = CreateDataSource(hBuffer:table-handle:serialize-name).
                
                //DataSources:Put(new String(hBuffer:table-handle:name), oDataSource).
                oDataSources:Put(hBuffer, oDataSource).
            end. // has data and not in NOTargetTab;es 
        end.   
        return oDataSources. 
    end method.     
    
    method protected IUpdateTarget CreateSaveSource(pcName as character):
        return cast(Application:ServiceManager,IServiceManager):CreateUpdateTarget(pcName).
    end method.
    
    method protected IDataSource CreateDataSource(pcname as character):
        return cast(Application:ServiceManager,IServiceManager):CreateDataSource(pcName).
    end method.
        
    // override to handle named query
    method protected IHandleDataSourceMap StartDataSources(poRequest as IGetDataRequest, phDataset as handle):
        define variable i              as integer   no-undo.
        define variable hBuffer        as handle    no-undo.
       
        define variable iBuffer        as integer   no-undo.
        define variable cErrorTables   as character no-undo.
        define variable lClientOnly    as logical no-undo.
        define variable lOk            as logical no-undo.
        define variable hTopBuffers    as handle extent no-undo.
        define variable oDataSourceMap as IHandleDataSourceMap no-undo.
        define variable oDataSource    as IDataSource no-undo.
        
        oDataSourceMap = GetDataSourceMap(phDataset).
         
        // prepare may deactivate relation and add the buffer as top-buffer, so
        // keep track of the original top buffers to avoid preparing twice          
        extent(hTopBuffers) = phDataset:num-top-buffers.
        do i = 1 to phDataset:num-top-buffers:
            hTopBuffers[i] = phDataset:get-top-buffer(i).
        end.
 
        do i = 1 to extent(hTopBuffers):
            StartDataSource(oDataSourceMap,hTopBuffers[i],poRequest,?,false).
        end.    
       
        // check that at least one top table have a datasource   
        // we don't enforce requests for all top tables.
        // this will catch an unlikely case of no TableRequests sonce the size will be zerp and the extent ?
        if oDataSourceMap:Size <> extent(poRequest:TableRequests) then 
        do:
            do iBuffer = 1 to phDataset:num-top-buffers:
                if oDataSourceMap:ContainsKey(phDataset:get-top-buffer(iBuffer)) then
                do:
                   lok = true.
                   lClientOnly = false.
                   leave. 
                end.
                else do:
                   
                    if type-of(poRequest:TableRequests[iBuffer],Pmfo.Core.BusinessLogic.IGetDataTableRequest) 
                    and cast(poRequest:TableRequests[iBuffer],Pmfo.Core.BusinessLogic.IGetDataTableRequest):ClientOnly then 
                       lClientOnly = true.
                    else 
                        cErrorTables = cErrorTables 
                                     + (if cErrorTables = "" then "" else ",")
                                    + phDataset:get-top-buffer(iBuffer):table-handle:name. 
                end.    
            end. 
            if not lok then
            do: 
                if lClientOnly then 
                    undo, throw new IllegalOperationError("The GetDataRequest top table requests(s) are all Client Only requests").
                else    
                    undo, throw new IllegalOperationError(subst("The GetDataRequest is missing request&1 for &2 top table&1 &3 in the dataset",
                                                            if num-entries(cErrorTables) = 1 then "" else "s",
                                                            if num-entries(cErrorTables) = 1 then "the" else "all",
                                                            StringUtil:GetMessageFromEntries(cErrorTables))).
            end.                                                         
        end. 
        return oDataSourceMap. 
    end method.    
    
    method protected IDataSource StartDataSource(oDataSourceMap as IHandleDataSourceMap, phBuffer as handle,poRequest as IGetDataRequest,poParentTableRequest as IGetDataTableRequest, plInnerJoin as logical):
        define variable oDataSource    as IDataSource no-undo.
        define variable oTableRequest  as IGetDataTableRequest  no-undo.
        define variable iChild         as integer no-undo. 
        define variable iCount         as integer no-undo.
        define variable iBuffer        as integer no-undo.
        define variable cErrorTables   as character no-undo.
        define variable hRelation      as handle no-undo.
        define variable lClientOnly    as logical no-undo.
        define variable lok            as logical no-undo.
        define variable cInnerJoins    as character no-undo. 
        define variable cServerinnerJoins as character no-undo.
        define variable hCountFld      as handle no-undo.
        
        oTableRequest = GetDataTableRequest(poRequest,phBuffer).
        
        if type-of(oTableRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest) then
            lClientOnly = cast(oTableRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest):ClientOnly.  
         
        if valid-object(oTableRequest) and not lClientOnly then 
        do:    
            oDataSource = CreateDataSource(phBuffer:table-handle:serialize-name).
           
            if not valid-object(oDatasource) then 
                undo, throw new NotFoundError(subst("DataSource '&1'",phBuffer:serialize-name)).
            oDataSourceMap:Put(phBuffer, oDataSource).
            
           // DataSources:Put(new String(phBuffer:table-handle:name), oDataSource).
            if valid-object(poParentTableRequest) and cast(oTableRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest):IsMainTable = false then 
            do: 
                if plInnerJoin and cast(oTableRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest):IsPaging then
                    undo, throw new IllegalOperationError(subst("Paging is not allowed on '&1' when inner joined to parent '&2'",oTableRequest:TableName,poParentTableRequest:TableName)).
                lok = oDataSource:Prepare(phBuffer,oTableRequest,poParentTableRequest).
            end.
            else do:   
                
                if cast(oTableRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest):IsMainTable then
                do:
                    cInnerJoins =  cast(oTableRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest):GetInnerJoins(output cServerinnerJoins).
                end.
                if cInnerJoins > "" then
                    lok = oDataSource:PrePrepare(phBuffer, oTableRequest).
                       
                else do:   
                    if valid-object (poParentTableRequest) then 
                        lok = oDataSource:Prepare(phBuffer,oTableRequest,poParentTableRequest).
                    else
                        lok = oDataSource:Prepare(phBuffer,oTableRequest).
                    // if this is a /count and we have TotalCount then there is no need to fill  
                    if lok 
                    and oTableRequest:NumRecords > 0 
                    and cast(oTableRequest,Pmfo.Core.BusinessLogic.IGetDataTableRequest):SelectFields = "count" 
                    and oDataSource:TotalCount > 0 then 
                    do:
                        // if there actually is a count field then don't  do  this
                        hCountFld = phBuffer:buffer-field("count") no-error.
                        if not valid-handle(hCountFld) then 
                        do:
                            phBuffer:fill-mode = "no-fill".
                            
                        end.
                    end.    
                end.    
            end.
            if lok then  
            do iChild = 1 to phBuffer:num-child-relations:
                hRelation = phBuffer:get-child-relation(iChild).
                // reposition is also a top-buffer so skip it here
                // the repos table does not use the parent relation in fill
                if hRelation:reposition = false and hRelation:recursive = false then
                    StartDataSource(oDataSourceMap,hRelation:child-buffer,poRequest,oTableRequest,plInnerJoin or cInnerJoins > "").
                
                if lookup(hRelation:child-buffer:table-handle:name,cServerInnerJoins) > 0 then
                    hRelation:child-buffer:fill-mode = "no-fill".
            end.
            else 
                phBuffer:fill-mode = "no-fill".
                
        end. 
        else  // allow skip of top table - there will be a check to see that we have at least one  
            phBuffer:fill-mode = "no-fill".
            
        if cInnerJoins > "" then
        do: 
             lok = oDataSource:PrepareInnerJoins(oTableRequest, cInnerJoins, poParentTableRequest, phBuffer:dataset).
             if not lok then
                 phBuffer:fill-mode = "no-fill".
        end.
         
        return oDataSource.
    end method.
    
    method protected IGetDataTableRequest GetDataTableRequest(pDataRequest as IGetDataRequest, phHandle as handle):
        define variable i as integer no-undo.
        define variable oTableRequest as IGetDataTableRequest no-undo.
        Assert:NotNull(pdataRequest,"Data Request passed to GetDataTableRequest").
        Assert:NotNull(phHandle,"Buffer or table-handle passed to GetDataTableRequest").
         
        if type-of(pDataRequest,GetDataRequest) then
        do:
            oTableRequest = cast(pDataRequest,GetDataRequest):GetTableRequest(phHandle).
            if not valid-object(oTableRequest) and valid-object(pDataRequest:NamedQuery) then
                oTableRequest = new GetDataTableRequest( if phHandle:type = "buffer" then phHandle:table-handle:name else phHandle:name). 
        end.   
        else do i = 1 to extent(pDataRequest:TableRequests):
            case phHandle:type:
                when "buffer" then
                do:
                    if phHandle:table-handle:name = pDataRequest:TableRequests[i]:TableName then
                        oTableRequest =  pDataRequest:TableRequests[i].
                end.
                when "temp-table" then 
                do:
                    if phHandle:name = pDataRequest:TableRequests[i]:TableName then
                        oTableRequest = pDataRequest:TableRequests[i].
                end.         
            end case.
        end.                 
        return oTableRequest.
    end.  
      
    /**
    method public Object SubmitData( input poRequest as Ccs.BusinessLogic.IUpdateDataRequest, input-output dataset-handle phDataset, output table-handle phWarning ):
        define variable oRequest as IUpdateDataRequest no-undo.
        if type-of(poRequest,Ccs.BusinessLogic.IUpdateDataRequest) then
            oRequest = cast(Adapt(poRequest,phDataset),IUpdateDataRequest).
        else   
            oRequest = cast(poRequest,IUpdateDataRequest).  
        return SubmitData(oRequest,input-output dataset-handle phDataset by-reference, output table-handle phWarning by-reference).    
    end method.
    **/
     
    method public Object SubmitData( input poRequest as IUpdateDataRequest, input-output dataset-handle phDataset, input dataset-handle phInfrastructureDataset, output table-handle phWarning ):
       
        define variable hBuffer             as handle no-undo.
        define variable oRequestKey         as String no-undo.
        define variable oTransactionManager as ITransactionManager no-undo.
        define variable oDataSources        as IHandleDataSourceMap no-undo.
        define variable oAction             as IAction no-undo.
        define variable lOk                 as logical no-undo.
        define variable oUpdateRe           as character no-undo.
      
        define variable lCanExecuteAsTransaction as logical no-undo.
        // error - failed validation that has not beem caught on client  
        //         so make sure they are thrown and shown to user 
        if phDataset:error then 
            undo, throw new ValidationError(). 
            
        // setter and destructor for DatasetHandle to avoid memory leak 
        // it cannot be deleted here since the method is not final and can be (and is) overridden 
        DatasetHandle = phDataset.  
        
        if not valid-object(poRequest:CommitScope) then 
            assign cast(poRequest,UpdateDataRequest):CommitScope = CommitScopeEnum:All.
        
        if valid-object(poRequest:CustomRequest) then
        do: 
            if  type-of(poRequest:CustomRequest,IAction) then
            do:
/*                if CommitScopeEnum:Nested:Equals(poRequest:CommitScope) then                      */
/*                     undo, throw new NotImplementedError(subst("Nested CommitScope for Actiona")).*/
                oAction = cast(poRequest:CustomRequest,IAction).
            end.
/*            else                                                                                             */
/*                undo, throw new NotImplementedError(subst("Execution of CustomRequest that is not IAction")).*/
        end. 
           
        oDataSources = StartSaveSources(poRequest,phDataset). 
        
        BeforeUpdate:publish(input dataset-handle  phdataset by-reference).
        
        if valid-object(oAction) and oAction:ActionScope = ActionScopeEnum:Table then 
        do:
            if valid-handle(phInfrastructureDataset) and DatasetUtil:HasRecords(phDataset) then 
            do:
                if CanExecuteTableActionAsTranaction(oAction, phDataset) then 
                   lCanExecuteAsTransaction = true. 
                else    
                   undo, throw new UnsupportedOperationError(subst("Action '&1' with Action Scope 'table' with client side infrastructure data. The infrastructure data must be udated from the Action inside the transaction. Alternatively setup the Action to be called from ExecuteActionAsTransaction'.",oAction:name)).
            end.   
            if lCanExecuteAsTransaction then 
                lok = ExecuteActionAsTransaction(oAction, phdataset,phInfrastructureDataset).     
            else 
                lok = ExecuteAction(oAction, phdataset).
            if not lok then
                undo, throw new IllegalOperationError(subst("Action '&1' was not found in any Update Target",oAction:name)).
        end.
        else do:
            if CommitScopeEnum:Nested:Equals(poRequest:CommitScope) then
                oTransactionManager = new NestedTransactionManager(oDataSources,poRequest).
            else 
                oTransactionManager = new TransactionManager(oDataSources,poRequest).
        end.
        
        if valid-object(oTransactionManager) then
        do:
            if valid-handle(phInfrastructureDataset) then 
                oTransactionManager:ProcessInfrastructureData:subscribe(ProcessInfrastuctureDataset).
            // validate and save all records 
            oTransactionManager:ProcessDataset(phdataset,phInfrastructureDataset).
        end.
        
        if poRequest:NoRefresh = false then
        do on error undo, throw:
            RefreshBuffers(DatasetHandle).
            catch e as Progress.Lang.Error :
                undo, throw new RefreshError(e).    
            end catch.
        end. 
        
        AfterUpdate:publish(input dataset-handle phdataset by-reference).
        
        CollectWarnings(oDataSources:Sources()).
        GetWarnings(output table-handle phWarning by-reference).
        
         // bug workarond dataet:error is not detected on client when by-reference is used
        if phDataset:error then
            return new String("Error").
         
        if poRequest:NoRefresh then
            phDataset:empty-dataset ().
               
        catch eConflict as ConflictError :
            undo, throw eConflict.
        end catch.
        
        catch e as Progress.Lang.Error:
            if not phDataset:Error then 
                undo, throw e.
            return new String("Error").
        end catch.
        
        // Catch Progress.Lang.Stop errors since we use -catchStop 1
        catch eConflictStop as Progress.Lang.LockConflict :
            undo, throw new LockTimeOutError(eConflictStop).    
        end catch.
        catch eStopAfter as Progress.Lang.StopAfter :
            undo, throw new RequestTimeOutError("update",eStopAfter).    
        end catch.
        
        finally:
            DestroyDataSources(phDataset).
        end finally.
    end method.

    method protected void GetWarnings(output table ttWarning): 
    end method.      

    method public void ProcessInfrastuctureDataset(input phInfrastreuctureDataset as handle, phApplicationDataset as handle):
        define variable i as integer no-undo.
        define variable oDas as IDataAccess extent no-undo.
        if not transaction then 
            undo, throw new IllegalOperationError("ProcessInfrastuctureDataset can only be called from an active transaction").
        extent(oDas)  = phInfrastreuctureDataset:num-top-buffers.
        do i = 1 to phInfrastreuctureDataset:num-top-buffers:
            if phInfrastreuctureDataset:get-top-buffer (i):table-handle:has-records then 
            do on error undo, throw:
                oDas[i] = cast(Application:ServiceManager,IServiceManager):CreateDataAccess(phInfrastreuctureDataset:get-top-buffer (i):table-handle:serialize-name).
                catch e as NotFoundError:
                    undo, throw new IllegalStateError(subst("Data Access for save of '&1' data is not defined. Please Contact IT.",phInfrastreuctureDataset:get-top-buffer (i):table-handle:serialize-name),e).     
                end catch.
            end.    
            
            if valid-object(oDas[i]) then 
            do:
                if type-of(oDas[i],IInfrastructureDataAccess) then 
                do:
                    cast(oDas[i],IInfrastructureDataAccess):SubmitData(input phInfrastreuctureDataset, phApplicationDataset).
                    if valid-object(cast(oDas[i],IInfrastructureDataAccess):Warnings) then 
                        CollectWarnings(cast(oDas[i],IInfrastructureDataAccess):Warnings, phInfrastreuctureDataset:get-top-buffer (i):table-handle:serialize-name).
                end.
                else do:
                     undo, throw new IllegalStateError(subst("Data Access for save of '&1' data is not defined as IInfrastrucutreinDataAccess. Please Contact IT.",phInfrastreuctureDataset:get-top-buffer (i):table-handle:serialize-name)).     
                 end.
            end.
            
        end.
        finally: 
            do i = 1 to extent(oDas):
                if valid-object(oDas[i]) then 
                    delete object oDas[i]. 
            end.
        end.                         
    end method.     
    
    method protected logical ExecuteActionAsTransaction(pAction as IAction, phDataset as handle, phInfrastrcuturedata as handle):
        define variable lok as logical no-undo.
        do transaction: 
            lok = ExecuteAction(pAction,phDataset).
            if lok then
               ProcessInfrastuctureDataset(phInfrastrcuturedata,phDataset). 
        end.  
        return lok.   
    end method.    
    
    method protected logical ExecuteAction(pAction as IAction, phDataset as handle):
        define variable oIterator      as IIterator no-undo.
        define variable oUpdateTargets as IUpdateTarget extent no-undo.
        define variable oParameterList as ParameterList no-undo.
        define variable oMethod        as Method no-undo.       
        define variable i              as integer no-undo.
        
        oUpdateTargets = cast(GetDataSourceMap(phDataset):Sources(),IUpdateTarget).
        do i = 1 to extent(oUpdateTargets):
            oMethod = oUpdateTargets[i]:GetActionMethod(pAction, phDataset, output oParameterList).
            if valid-object(oMethod) then
            do:
                oMethod:Invoke(oUpdateTargets[i],oParameterList).
                return true.
            end.    
        end.    
        return false. 
    end method.    
    
    method protected logical CanExecuteTableActionAsTranaction(pAction as IAction, phDataset as handle):
        define variable oIterator      as IIterator no-undo.
        define variable oUpdateTargets as IUpdateTarget extent no-undo.
        define variable oParameterList as ParameterList no-undo.
        define variable oMethod        as Method no-undo.       
        define variable i              as integer no-undo.
        
        oUpdateTargets = cast(GetDataSourceMap(phDataset):Sources(),IUpdateTarget).
        do i = 1 to extent(oUpdateTargets):
            oMethod = oUpdateTargets[i]:GetActionMethod(pAction, phDataset, output oParameterList).
            if valid-object(oMethod) then
            do:
                if lookup(oMethod:name,oUpdateTargets[i]:GetTransactionTableActions()) > 0 then 
                    return true.
            end.    
        end.    
        return false. 
    end method.    
    
    
    method protected void CollectWarnings (pDataSources as IDataSource  extent ):
        define variable oDataSources as IDataSource no-undo.
        define variable i as integer no-undo.
        do i = 1 to extent(pDataSources):
            if valid-object(pDataSources[i]:Warnings) then
                CollectWarnings(pDataSources[i]:Warnings, pDataSources[i]:EntityName).
        end.
    end method.    
    
    method protected void CollectWarnings (pWarnings as ICollection, pcEntity as character):
        define variable iWarning     as integer no-undo.
        define variable oIterator    as IIterator no-undo.
        define variable oMsgIterator as IIterator no-undo.
        define variable oWarning     as IWarning no-undo.
        
        for each ttWarning by ttWarning.Seq descending.
            iWarning = ttWarning.seq.
            leave.
        end.
       
        oMsgIterator = pWarnings:Iterator().
        do while oMsgIterator:HasNext():
            create ttWArning.
            assign 
                oWarning = cast(oMsgIterator:Next(),IWarning)
                iWarning = iWarning + 1 
                ttWarning.entity    =pcEntity
                ttWarning.seq       = iWarning
                ttWarning.msg       = oWarning:Message
                ttWarning.fieldName = oWarning:FieldName
                .
        end.
    end method.    
            
    method public void RefreshBuffers(phDataset as handle):
        define variable i as integer no-undo.
        do i = 1 to phDataset:num-top-buffers:
            RefreshBuffer(phDataset,phDataset:get-top-buffer(i),?).
        end.
        
    end method.     
    
    method public void RefreshBuffer(phDataset as handle,phBuffer as handle, poParentRequest as IGetDataTableRequest):
        define variable hQuery         as handle no-undo.
        define variable oDataSource    as IDataSource no-undo.
        define variable iChild         as integer no-undo.
        define variable hChildBuffer   as handle no-undo.
        define variable hRelation      as handle no-undo.
        define variable lok            as logical no-undo.
        define variable oRequest       as IGetDataTableRequest no-undo.
        define variable cJoin          as character no-undo.     
        define variable oDataSourceMap as IHandleDataSourceMap no-undo.
        
        oDataSourceMap = GetDataSourceMap(phDataset).
       
        if phBuffer:table-handle:has-records then 
        do:
            oDataSource = oDataSourceMap:Get(phBuffer).
            if valid-object(oDataSource) then
            do:   
            
                create query hQuery.
                hQuery:add-buffer(phBuffer).
                
                if valid-handle(phBuffer:parent-relation) then
                do: 
                   cJoin = BufferUtil:GetJoinExpression(phBuffer:parent-relation).
                   if cJoin > "" then
                      cJoin = "where " + cJoin.     
                end.
                hQuery:query-prepare(trim(subst("preselect each &1 &2",phBuffer:name,cJoin))).
                hQuery:query-open().
                hQuery:get-first.
                 
                do while phBuffer:avail:
                    if phBuffer:error = false and phBuffer:rejected = false then
                    do:
                        // todo - check return and avoid Refill if false (in most cases refillcurrentrow is ) 
                        oRequest = oDataSource:PrepareCurrent(phBuffer, poParentRequest).
                        if oDataSource:RefillCurrentRow() then
                        do:
                            do iChild  = 1 to phBuffer:num-child-relations:
                                hRelation = phBuffer:get-child-relation(iChild).
                                if hRelation:active 
                                and hRelation:reposition = false
                                and hRelation:recursive = false
                                then
                                do:
                                    hChildBuffer = phBuffer:get-child-relation(iChild):child-buffer.
                                    RefreshBuffer(phDataset,hChildBuffer,oRequest).
                                end.
                            end.
                        end.
                       // else delete?? 
                    end.
                    hQuery:get-next.
                end.
            end.
        end.
        finally:
            if valid-handle(hQuery) then
                delete object hQuery.        
        end finally.
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
    
    
/*    method protected void DestroySources(plUpdate as logical):                   */
/*        define variable oSources as ICollection no-undo.                         */
/*        define variable oIter    as IIterator no-undo.                           */
/*        define variable oObj     as Object no-undo.                              */
/*        oSources = DataSources:Values.                                           */
/*        oIter = oSources:Iterator().                                             */
/*        do while oIter:HasNext():                                                */
/*            oObj = oIter:Next().                                                 */
/*            if plUpdate and type-of(oObj,IUpdateTarget) then                     */
/*            do:                                                                  */
/*                 BeforeUpdate:Unsubscribe(cast(oObj,IUpdateTarget):BeforeUpdate).*/
/*                 AfterUpdate:Unsubscribe(cast(oObj,IUpdateTarget):AfterUpdate).  */
/*            end.                                                                 */
/*                                                                                 */
/*            delete object oObj.                                                  */
/*        end.                                                                     */
/*                                                                                 */
/*        delete object DataSources.                                               */
/*                                                                                 */
/*    end method.                                                                  */
    
     // @TODO use this if to support update from non UI without before-image - for example edi?? 
  //  method private logical ProcessBIData(phDataset as handle,input iRowState as integer):
    /**********************************************************************************************
    *    Copyright (C) 2014-2018 by Progress Software Corporation. All rights reserved.           *
    *    Prior versions of this work may contain portions contributed by participants of Possenet.*
    ***********************************************************************************************/
/*                                                                                                                                                                      */
/*        define var iIndex as integer no-undo.                                                                                                                         */
/*        define var hBuffer as handle no-undo.                                                                                                                         */
/*        define var hBeforeBuffer as handle no-undo.                                                                                                                   */
/*        define var lAvailable as logical no-undo.                                                                                                                     */
/*        define var lReturnValue as logical no-undo.                                                                                                                   */
/*        define var hAfterQuery  as handle extent no-undo.                                                                                                             */
/*        define var lHasBIData as logical init ? no-undo.                                                                                                              */
/*                                                                                                                                                                      */
/*        extent(hAfterQuery) = phDataSet:NUM-BUFFERS.                                                                                                                  */
/*                                                                                                                                                                      */
/*        repeat iIndex = 1 to phDataSet:NUM-BUFFERS:                                                                                                                   */
/*            // Only need to worry about tables that have data-source attached..                                                                                       */
/*            if valid-handle(phDataSource[iIndex]) then do:                                                                                                            */
/*                hBuffer = phDataSet:GET-BUFFER-HANDLE(iIndex).                                                                                                        */
/*                hBeforeBuffer = hBuffer:before-buffer.                                                                                                                */
/*                                                                                                                                                                      */
/*                if hBeforeBuffer eq ? then                                                                                                                            */
/*                    undo, throw new                                                                                                                                   */
/*                    IllegalOperationError("To use BusinessEntity CUD functionality, temp-tables must be specified with BEFORE-TABLE").                                */
/*                                                                                                                                                                      */
/*                // Check for before-image rows. Skip over tables that have no rows at all                                                                             */
/*                if hBuffer:table-handle:HAS-RECORDS or hBeforeBuffer:table-handle:HAS-RECORDS then do:                                                                */
/*                                                                                                                                                                      */
/*                    if lHasBIData eq ? then do:                                                                                                                       */
/*                        lHasBIData = hBeforeBuffer:table-handle:HAS-RECORDS.                                                                                          */
/*                    end.                                                                                                                                              */
/*                    else if lHasBIData ne hBeforeBuffer:table-handle:HAS-RECORDS then do:                                                                             */
/*                        undo, throw new IllegalOperationError("To use BusinessEntity CUD functionality, all temp-tables must have same setting of before-image data").*/
/*                    end.                                                                                                                                              */
/*                end.                                                                                                                                                  */
/*              end.                                                                                                                                                    */
/*        end.                                                                                                                                                          */
/*                                                                                                                                                                      */
/*        // If no bi data, then create it..                                                                                                                            */
/*        if lHasBIData eq false and iRowState ne ? then do:                                                                                                            */
/*            repeat iIndex = 1 to ProDataSet:NUM-BUFFERS:                                                                                                              */
/*                  // Only need to worry about tables that have data-source attached..                                                                                 */
/*                if valid-handle(ProDataSource[iIndex]) then do:                                                                                                       */
/*                    hBuffer = ProDataSet:GET-BUFFER-HANDLE(iIndex).                                                                                                   */
/*                                                                                                                                                                      */
/*                    create query hAfterQuery[iIndex].                                                                                                                 */
/*                    hAfterQuery[iIndex]:set-buffers(hBuffer).                                                                                                         */
/*                    lReturnValue = hAfterQuery[iIndex]:query-prepare(substitute('FOR EACH &1', hBuffer:name)).                                                        */
/*                    lReturnValue = hAfterQuery[iIndex]:query-open().                                                                                                  */
/*                    lAvailable = hAfterQuery[iIndex]:get-first().                                                                                                     */
/*                                                                                                                                                                      */
/*                    do while lAvailable:                                                                                                                              */
/*                        /* Note: If a before-table row already exists, MARK-ROW-STATE() is a no-op */                                                                 */
/*                        lReturnValue = hBuffer:mark-row-state(iRowState).                                                                                             */
/*                                                                                                                                                                      */
/*                        if not lReturnValue then                                                                                                                      */
/*                            undo, throw new Progress.Lang.AppError('Unable to ' + GetRowState(iRowState) +                                                            */
/*                                   ' record. Cannot create corresponding row in BEFORE-TABLE.', 0).                                                                   */
/*                          lAvailable = hAfterQuery[iIndex]:get-next().                                                                                                */
/*                      end.                                                                                                                                            */
/*                  end.                                                                                                                                                */
/*              end.  // END REPEAT                                                                                                                                     */
/*          end.                                                                                                                                                        */
/*                                                                                                                                                                      */
/*          finally:                                                                                                                                                    */
/*              repeat iIndex = 1 to ProDataSet:NUM-BUFFERS:                                                                                                            */
/*                  if valid-handle(hAfterQuery[iIndex]) then                                                                                                           */
/*                      delete object hAfterQuery[iIndex].                                                                                                              */
/*              end.                                                                                                                                                    */
/*                                                                                                                                                                      */
/*              return lHasBIData.                                                                                                                                      */
/*          end finally.                                                                                                                                                */
/*                                                                                                                                                                      */
  //  end method.

   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    destructor public DataAccess ( ):
        if valid-handle(DatasetHandle) then
        do: 
            delete object DatasetHandle no-error.
        end.
    end destructor.
    
    
end class.