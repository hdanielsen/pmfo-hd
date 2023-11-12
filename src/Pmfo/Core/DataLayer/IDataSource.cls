
/*------------------------------------------------------------------------
    File        : IDataSource
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Feb 11 15:30:14 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/ 

using Ccs.BusinessLogic.IGetDataTableRequest from propath.
using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IList from propath.
using OpenEdge.Core.ISupportInitialize from propath.
using Pmfo.Core.Common.IDataService from propath.
using Pmfo.Core.DataLayer.IDataSourceMap from propath.
using Pmfo.Core.DataLayer.IDbQuery from propath.
using Pmfo.Core.DataLayer.IUpdateTarget from propath.

interface Pmfo.Core.DataLayer.IDataSource inherits IDataService, IDbQuery, IDataSourceMap, ISupportInitialize:  
    /* Public name used to start the Source in the ServiceNanager
       (Stored in resources.json and defined as primary key in the temp-table
        that reads this) 
    */ 
    define public property EntityName           as character no-undo get.
    
    /* stores the buffer-handle of the dataset buffer that is being saved 
       Assigned in prepareSave
       Also defined in IDataSource and used for read
    */ 
    define public property DataBuffer           as handle    no-undo get. 
    /* requested number */
    define public property NumRecords  as int64        no-undo get.  
    define public property Count       as logical      no-undo get. 
    define public property TotalCount  as int64        no-undo get. 
    define public property NextRowKey  as character    no-undo get.  
    define public property Warnings    as IList        no-undo get.  
    define public property FillErrors  as ICollection  no-undo get.  
    define public property QueryStringWithoutParent as character  no-undo get.  
           
    method public logical Prepare(phBuffer as handle, pRequest as IGetDataTableRequest, pParentRequest as IGetDataTableRequest).
    method public logical PrepareCurrent(phBuffer as handle, pParentRequest as IGetDataTableRequest).
    method public logical Prepare(phBuffer as handle, pRequest as IGetDataTableRequest).
    // everything expect final query prepare - waiting for all data sources to be started  
    // to be able to add innerjoins 
    method public logical PrePrepare(phBuffer as handle, pRequest as IGetDataTableRequest).
    // call after all sources are started in datasouece that is prepared. 
    method public logical PrepareInnerJoins(pRequest as IGetDataTableRequest, pcInnerJoinTables as character, pParentRequest as IGetDataTableRequest,  phdataset as handle).
    // hook called from PrepareInnerJoins in each involved data source 
    method public void PrepareInnerJoinRequest(pRequest as IGetDataTableRequest, pcInnerJoinTables as character, phdataset as handle).
    // public to be called from DataAccess    
    method public void AfterRow(input dataset-handle phDataset).  
    method public logical Fill().
    method public logical RefillCurrentRow().
    
    method public void FindSource(plock as integer).
    
end interface.