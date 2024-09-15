
/*------------------------------------------------------------------------
    File        : IGetDataResponse
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu May 30 20:26:04 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Ccs.BusinessLogic.IGetDataResponse from propath.
using Ccs.BusinessLogic.IGetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.ResponseMetaDataEnum from propath.
using Pmfo.Core.Common.IGetDataQuestions from propath.
using Pmfo.Core.BusinessLogic.IDataResponse from propath.

interface Pmfo.Core.BusinessLogic.IGetDataResponse inherits IGetDataResponse, IDataResponse:  
    
    define public property HasQuestions as logical no-undo get.  
    define public property Questions as IGetDataQuestions  no-undo get. 
    
    define public property MainRequestedBuffer   as character no-undo get.  
    define public property RequestType           as RequestTypeEnum no-undo get. set. 
     // set to true for singlerow method call to return empty
    define public property NoError               as logical no-undo get.  
    define public property MetaDataEnum          as ResponseMetaDataEnum no-undo get. set.
    // set to true in SetRequestProperties to signal already done  to avoid calling receive and fetch twice
    // when subclasses call FethcAndRecive in a function override 
    define public property RequestComplete       as logical no-undo get.  
    // Optionally specify CSV of serializeClientGetRequest and BusinessEntityOperationHandler
    // Fields in children of the requested table must be qualified
    define public property Select                as character no-undo get. set.
    // what?? this is real... mass update replace reurns data wirh changes 
    // so that the user can look at it and confirm and PUT if ok 
    define public property WriteBeforeImage      as logical   no-undo get. set.
     
    method public void SetRequestProperties(pRequest as IGetDataRequest).
   // method public JsonArray GetQuestions(): 
    
    method public IGetDataTableResponse GetTableResponse(pName as character).
end interface.