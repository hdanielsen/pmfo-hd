 
 /*------------------------------------------------------------------------
    File        : GetDataResponse
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu May 30 20:28:08 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.BusinessLogic.IGetDataTableResponse from propath.
using OpenEdge.BusinessLogic.GetDataResponse from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataResponse from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.ResponseMetaDataEnum from propath.
using Pmfo.Core.Common.GetDataQuestions from propath.
using Pmfo.Core.Common.IGetDataQuestions from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Core.Common.IGetDataMessages from propath.
using Pmfo.Core.Common.GetDataMessages from propath.

class Pmfo.Core.BusinessLogic.GetDataResponse inherits GetDataResponse implements IGetDataResponse: 
    define private variable mTables as character no-undo.
    define public property MainRequestedBuffer as character no-undo get. protected set. 
    define public property RequestType as RequestTypeEnum no-undo get. set. 
     // set to true for singlerow method call to return empty
    define public property NoError             as logical no-undo get. protected set. 
    define public property RequestComplete as logical no-undo get.  protected set.
    
    define public property MetaDataEnum as ResponseMetaDataEnum no-undo 
        get():
            if not valid-object(MetaDataEnum) then
                MetaDataEnum = ResponseMetaDataEnum:None.
            return MetaDataEnum.    
        end. 
        set. 
    
     define public property Messages as IGetDataMessages  no-undo 
        get():
            if not valid-object(Messages) then 
                Messages = new GetDataMessages().
            return Messages.    
        end.    
        protected set.
            
    // Optionally specify CSV of serializeClientGetRequest and BusinessEntityOperationHandler
    // Fields in children of the requested table must be qualified
    define public property Select              as character no-undo get. set.
    // mass update replace reurns data wirh changes 
    // so that the user can look at it and confirm and PUT if ok 
    define public property WriteBeforeImage      as logical   no-undo get. set.
    
    // for performance use for optional requests to avoid lazy load if not necessary     
    define public property HasQuestions as logical no-undo get. protected set.  
    
    define private property mQuestions as IGetDataQuestions  no-undo get. set.
    
    define public property Questions as IGetDataQuestions  no-undo  
        get():
            if not valid-object(mQuestions) then 
                 mQuestions = new GetDataQuestions().
            return mQuestions.    
        end.     
        protected set(pquestions as IGetDataQuestions):
            if valid-object(mQuestions) and mQuestions:Size > 0 then 
                undo, throw new IllegalOperationError("Questions is already set and has data"). 
            mQuestions = pquestions.
            HasQuestions = valid-object(pquestions).
        end. 
        
    constructor public GetDataResponse ():
        super ().
    end constructor. 
     
    constructor public GetDataResponse ( input pResponse as  IGetDataTableResponse ):
        this-object(cast(Array:ToArray(pResponse),IGetDataTableResponse)).
    end constructor. 
     
    constructor public GetDataResponse ( input pResponses as  IGetDataTableResponse extent ):
        define variable i as integer no-undo.
        super (input pResponses).
        do i = 1 to extent(pResponses):
            mTables = mTables 
                    + (if i = 1 then "" else ",")
                    + pResponses[i]:TableName.
        end.    
    end constructor.
    
    method public IGetDataTableResponse GetTableResponse(pName as character):
        define variable iExtent as integer no-undo.
        iExtent = lookup(pName,mTables).
        if iExtent > 0 then
            return  TableResponses[iExtent] .
        return ?.        
    end.
    
    method public final void SetRequestProperties(poRequest as IGetDataRequest):
        assign
            MainRequestedBuffer = poRequest:MainRequestedBuffer
            RequestType = poRequest:RequestType 
            NoError = poRequest:NoError
            RequestComplete = true // see BusinessEntity  GetData
            .
        if poRequest:HasQuestions then  
        do:   
            if HasQuestions = false then
                Questions = poRequest:Questions.
            else
                Questions:AddAll(poRequest:Questions).
         end.           
    end method.
    
end class.