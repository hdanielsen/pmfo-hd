 
 /*------------------------------------------------------------------------
    File        : ClientBusinessEntity
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Feb 17 12:04:35 EST 2021
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.BusinessEntity from propath.
using Pmfo.Core.BusinessLogic.ClientDataSource from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IClientEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.QueryBuilder.QueryRemovePredicateTableQualifier from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.QueryGroup from propath.
using Ccs.BusinessLogic.IQuerySortEntry from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.IReceiveDataEventArgs from propath.
 
class Pmfo.Core.BusinessLogic.ClientBusinessEntity  abstract inherits BusinessEntity implements IClientEntity: 
    
    define protected property NoParentJoin as logical no-undo get. set. 
    define protected property InnerJoinQueryEntries as IQueryEntry extent no-undo get. set.
    
    
    constructor public ClientBusinessEntity ( input phHandle as handle ):
        super (input phHandle).
        ClientOnly = true.
    end constructor.
     
    // dynamic create on the fly BEs only know resource name
    constructor protected ClientBusinessEntity (pcResource as character):
        super(pcResource).
        ClientOnly = true.
    end constructor.
    
    constructor public ClientBusinessEntity ( input phHandles as handle extent ):
        super (input phHandles).
        ClientOnly = true.
    end constructor.
    
   // IClientJoinEntity
    method public void PrepareInnerJoin(pRequest as IGetDataRequest, phBuffer as handle, output plHasClientPredicate as logical, output poClientSort as IQuerySortEntry extent):
        define variable oParentTableRequest as IGetDataTableRequest no-undo.
        define variable cInnerJoins         as character no-undo.
        define variable oQueryRemovequal    as QueryRemovePredicateTableQualifier no-undo. 
        define variable i                   as integer no-undo.
        
        // paging is denied on server for inner join  
        oParentTableRequest = cast(pRequest:TableRequests[1],IGetDataTableRequest).
         
        InnerJoinQueryEntries = cast(oParentTableRequest:GetQualifiedQueryEntries(phBuffer:table-handle:name),IQueryEntry).
        
        if extent(InnerJoinQueryEntries) > 0 then 
        do:
            oParentTableRequest:RemoveQueryEntries(InnerJoinQueryEntries).
            new QueryRemovePredicateTableQualifier(phBuffer:table-handle:name)           // remove the qualifier for internal use 
                 :Traverse(cast(new QueryGroup(InnerJoinQueryEntries),Ccs.BusinessLogic.IQueryGroup)).
            plHasClientPredicate = true.
        end. 
        if oParentTableRequest:HasSort() then
        do: 
            if lookup(phBuffer:table-handle:name,oParentTableRequest:GetQualifiedSortTableReferences()) > 0 then
                poClientSort = oParentTableRequest:RemoveQuerySort().
        end.
          
    end method. 
    
    // the call back from the ClientDataSource
    method public abstract int64 CreateSourceRecords(output table-handle phTable).
    
    // hook for save to server after all vlaidation has been done - called from SubmitData  
    method public Object SubmitClientData(poUpdateDataRequest as IUpdateDataRequest, input-output dataset-handle phDataset, input dataset-handle phInfrastructureDataset, output table-handle phWarning).
        // update is rarely called in ClientBe , but if it is then it need to have an override with behavior 
        undo, throw new NotImplementedError(subst("SubmitData in ClientBusinessEntity &1",Resource)).
    end method.   
      
    // final - the data is to be created in CreateSourceRecords that the ClientDataSource uses to filter and sort if necessary
    method override final protected void ReceiveBuffer( input pParent as IBusinessEntity, input pRequest as IGetDataTableRequest, input pResponse as IGetDataTableResponse, input phBuffer as handle ):
        define variable oService as ClientDataSource no-undo.
        // hook for child data
        BeforeCreateRecords(phBuffer). 
        // it is important to use the received buffer as this may be called with a dataset from a different BE 
        oService = new ClientDataSource(this-object). // prevents Garbage collection = delete servcice at end
        if NoParentJoin then 
            oService:NoParentJoin = true.
        if extent(InnerJoinQueryEntries) > 0 then
             pRequest:AddQueryEntries(InnerJoinQueryEntries).       
        oService:CreateRecords(pRequest, pResponse, phBuffer).  
        // hook for child data
        AfterCreateRecords(phBuffer). 
        
        extent(InnerJoinQueryEntries) = ?.
        finally:
            // the service leeps track of this-object for callbacks
            // this prevents GCC  so we must delete it when done 
            if valid-object(oService) then 
                delete object oService.
        end.
    end method.    
    
    method protected void BeforeCreateRecords(phBuffer as handle):    
    end method.
      
    // optionaL sub class hook
    method protected void AfterCreateRecords(phBuffer as handle):    
    end method.
end class.   
    