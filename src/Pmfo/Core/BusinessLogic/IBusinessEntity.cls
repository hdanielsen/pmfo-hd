
/*------------------------------------------------------------------------
    File        . IBusinessEntity
    Purpose     . 
    Syntax      . 
    Description .
    Author(s)   . hdaniels
    Created     . Mon Feb 11 00.17.18 EST 2019
    Notes       . 
  ----------------------------------------------------------------------*/
 
 
using Ccs.Common.IService from propath.
using OpenEdge.Core.Collections.IMap from propath.
using OpenEdge.Core.Collections.ISet from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.BusinessLogic.BeforeImageReadEnum from propath.
using Pmfo.Core.BusinessLogic.IAction from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IJsonWriter from propath.
using Pmfo.Core.BusinessLogic.IQueryEntry from propath.
using Pmfo.Core.BusinessLogic.IReceiveDataEventArgs from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataEventArgs from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.BusinessLogic.RowStateEnum from propath.
using Pmfo.Core.Common.IFunction from propath.
using Pmfo.Core.Common.ISuccessMessages from propath.
using Pmfo.Core.Common.IWarningMessages from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.Common.INamedArrayHolder from propath.
 
interface Pmfo.Core.BusinessLogic.IBusinessEntity inherits IEntity, IService.  
    define public event DataReceived signature void (pParent as Pmfo.Core.BusinessLogic.IBusinessEntity, pReceiveDataArgs as IReceiveDataEventArgs, phDataset as handle).
    
    define public property HasLink            as logical          no-undo get.
    define public property Links              as ISet             no-undo get.
    define public property HasFunction        as logical          no-undo get.
    define public property Functions          as IMap             no-undo get.
    define public property HasAction          as logical          no-undo get.
    define public property Actions            as IMap             no-undo get.
    define public property RedirectMap        as IStringStringMap no-undo get.
    define public property HasRedirectMap     as logical          no-undo get.
    define public property NextIds            as IMap             no-undo get.
    
    /* set to true to use jsonReader on POST 
       needed to override the  bug that causes json-read to not update index 
       when protected temp-table is used - fixed in 12.6 
       Can also return true if the BE needs Json load for other reasons 
       
    */ 
    define public property UseJsonReaderOnPost   as logical             no-undo get.
    define public property UseJsonReaderOnPut    as logical             no-undo get.
    define public property BeforeImageReadOption as BeforeImageReadEnum no-undo get. set.
    define public property InheritsResource      as character           no-undo get.
    define public property WarningMessages       as IWarningMessages    no-undo get. set.
    define public property SuccessMessages       as ISuccessMessages    no-undo get.
    define public property HasSuccessMessages    as logical             no-undo get.
    
    define public property Datasets       as handle extent no-undo get. 
    // allow control of which dataset to use for linking - useful if main dataset is recursive, perhoas  also ifdataset has lots tables for update  
    define public property LinkDataset    as handle no-undo get.    
    
     /*------------------------------------------------------------------------------
        Purpose: Returns an empty instance of the primary ProDataset to the caller
        Notes:   Used for catalog generation or initialization of dynamic user
                 interfaces etc.
        @param phDataset OUTPUT Dataset (not intended to be called BY-REFERENCE)
    ------------------------------------------------------------------------------*/
    method public void getDataset (output DATASET-HANDLE phDataset).
    
    method public handle GetDataset(pcCollections as character extent):
      
    method public handle GetRequestDataset(pClientRequest as IClientRequest).
    method public handle GetUpdateRequestDataset(pClientRequest as IClientRequest,output plBeforeImageRequired as logical, output pUpdateRequest as IUpdateDataRequest).
    method public handle GetExpandDataset(pcExpandExpression as character).
          
    // optionally return a separated list of resporces that is to be exposed as a common childlink to a BE 
    // Typically overridden in the application level BusinssEntity subclass 
    // For example a reportBE that may be used be any BE (Request Builder and Operation Handler will likely need 
    // dedicated logic for these)  
    method public character GetFixedChildren(pClientRequest as IClientRequest).
    
    // optionally returns a resource name in the collections of the clientRequest that 
    // is to be used to redirect to   
    method public character GetContainerRedirect(pClientRequest as IClientRequest, output pcContext as character). 
           
     // use to fill the BE when the tt or ds is not needed immediately/directly
    method public void GetData():    
        
    method public Pmfo.Core.BusinessLogic.IGetDataResponse GetData( output dataset-handle phDataset ).
    method public Pmfo.Core.BusinessLogic.IGetDataResponse GetData( input pRequest as IGetDataRequest,output dataset-handle phDataset ).
    
    // Retrieve the unique records specified in the INamedArrayHolder, and ignore other tables in the dataset  
    method public Pmfo.Core.BusinessLogic.IGetDataResponse GetRows(poArrayValues as INamedArrayHolder,  output dataset-handle phDataset ).
    // Retrieve the unique records specified in the INamedArrayHolder, but fill the dataset if it has other tables 
    method public Pmfo.Core.BusinessLogic.IGetDataResponse GetData(poArrayValues as INamedArrayHolder,  output dataset-handle phDataset ):
   
    // for primitive data - expected to return IPrimitiveHolder or IprimitveArrayHolder (may depend on the operation handlert)  
    // dataset is input - to disambiguate other overloads and since the output is not needed -  input becuase it is found/created by GetRequestDataset 
    method public Object GetData( input pRequest as IGetDataRequest,input dataset-handle phDataset ).
    method public logical GetData( input poRequest as IGetDataRequest).
    // for ABL request for just the table
    method public logical GetData( input pTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest,output table-handle phTable).
    // for ABL request for just the table
    method public logical GetData( input pQueryEntry as IQueryEntry,output table-handle phTable).
    
    method public int64 Count().
    method public int64 Count(input pTableRequest as Pmfo.Core.BusinessLogic.IGetDataTableRequest).
    
    method public IRelation GetLink(pcname as character).
    method public IRelation GetLink(pcParent as character, pcname as character):
    
    method public void BeforeUpdateData(pUpdateDataArgs as IUpdateDataEventArgs, phDataset as handle).
    method public void PrepareGetTableRequest (poRequest as IGetDataTableRequest, phBuffer as handle):
    
    method public Object updateData (input-output dataset-handle phDataset,poUpdateDataRequest as IUpdateDataRequest).     
    method public Object UpdateData(input-output dataset-handle phDataset).
     
    method public IAction extent GetServerTypeActions(pServerType as ServerTypeEnum).
    method public IFunction extent GetServerTypeFunctions(pServerType as ServerTypeEnum): 
   
    method public void ThrowDatasetError( phDataset as handle).
    // create a request for the main dataset 
    method public IGetDataRequest CreateGetRequest().
    // optional method to use native JsonWriter
    method public IJsonWriter GetJsonWriter(pRowState as RowStateEnum, pcServerMethodName as character, pcCollections as character extent).
    
    method public character GetKeys().
    method public character GetPublicKeys().
    method public character GetPublicTextField().
    
    method public character ConvertSingleKeyToExpression (phbuffer as handle, pcExpression as character) .
     
    method public void Clear().
    method public void SubscribeDataReceived(pEntity as Pmfo.Core.BusinessLogic.IBusinessEntity, pcSerializeName as character).
    method public void UnsubscribeDataReceived().
    
    // for catalog - call before building catalog for example if Actions and Functions are optimized to lazy load on request
    method public void PrepareCatalog().
    
end interface.