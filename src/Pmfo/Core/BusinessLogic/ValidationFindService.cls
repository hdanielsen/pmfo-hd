 
 /*------------------------------------------------------------------------
    File        : ValidationFindService
    Purpose     : Extends FindService in order to be used for validation 
                  from a BE - Allowing errors to be accumulated   
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Nov 23 09:50:30 EST 2021
    Notes       : Intended to be newed from ServiceManager GetService( poService as Class, poScope as ILifecycleScope )  
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.BusinessLogic.FindService from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
//using Pmfo.Core.BusinessLogic.IValidationFindService from propath.
using Pmfo.Core.Error.TableNotFoundError from propath.
using Pmfo.Util.ErrorStringUtil from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IRelation from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using Pmfo.Util.DatasetUtil from propath.
using Ccs.BusinessLogic.IQueryPredicate from propath.
using Ccs.BusinessLogic.QueryOperatorEnum from propath.
using Pmfo.Util.BufferUtil from propath.
using Pmfo.Core.Error.ValidationError from propath.

class Pmfo.Core.BusinessLogic.ValidationFindService inherits FindService: // implements IValidationFindService: 
    define protected property BufferHandle as handle get. set.
       // set to use to set fieldname in Buffer errors 
    define public property Links as ICollection no-undo get. set.
    // NOTE: this is the defined data set of the entity whose buffer is passed to the constructor
    //       Itmay NOT the dataset in use for this reuqest 
     //      It is supposed to only be used to find relations    
    //      (The reaosn is that this is invoked from ValidateTable and the buffer do not have the data set 
    //       if this becomes a problem add logfic to pass IUpdateDataEventArgs:dataset)
    // )
    define property DefinedDataset as handle no-undo get. set.
     
    constructor public ValidationFindService (phBuffer as handle):
        super ().
     // ??? 
    end constructor.
    
    method protected void BufferError(pcMsg as character):
        ErrorStringUtil:AddError(BufferHandle, pcMsg).
    end method.    
    
    method protected void BufferError(pcMsg as character, pcField as character):
         ErrorStringUtil:AddError(BufferHandle, pcMsg, pcField).
    end method.   
    
    method protected override logical Execute(pEntity as IBusinessEntity,pRequest as IGetDataRequest, phBuffer as handle, plKeepdata as logical):
        return super:Execute(pEntity, pRequest, phBuffer, plKeepdata).
        catch e as TableNotFoundError :
            HandleTableNotFound(pRequest,phbuffer,e).
        end catch.  
    end method.  
    
    method protected void HandleTableNotFound(pRequest as IGetDataRequest, phBuffer as handle,  pError as TableNotFoundError):
        define variable oTableRequest as IGetDataTableRequest no-undo.
        define variable cParentField  as character no-undo.
        define variable cEqualFields  as character no-undo.
        define variable oRelation     as IRelation no-undo.
        define variable cFieldPairs   as character no-undo.
        define variable hRelation     as handle no-undo.
        define variable oPredicate    as IQueryPredicate no-undo.
        define variable cMsg          as character no-undo.
        define variable hFld          as handle no-undo.
        
        oTableRequest = cast(pRequest:TableRequests[1],IGetDataTableRequest).
        cEqualFields = oTableRequest:GetPredicateFieldEqualMatches().
        if cEqualFields > "" and num-entries(cEqualFields) = 1 then 
        do: 
            oRelation = GetLink(BufferHandle:table-handle:serialize-name,phBuffer:table-handle:serialize-name).
            if valid-object(oRelation) then 
                cFieldPairs = oRelation:PairsList().
                
            if cFieldPairs = "" and valid-handle(DefinedDataset) then 
            do:
                hRelation = DatasetUtil:GetRelation(DefinedDataset,BufferHandle:table-handle:serialize-name, phBuffer:table-handle:serialize-name).
                if valid-handle(hRelation) then
                    cFieldPairs = hRelation:relation-fields.
            end.
        end.
            
        if num-entries(cFieldPairs) = 2 then 
        do: 
            if entry(2,cFieldPairs) = cEqualFields then
                cParentField = entry(1,cFieldPairs).
        end. 
       
        if cParentField > "" then   
        do: 
            oPredicate = oTableRequest:GetQueryPredicate(cEqualFields, QueryOperatorEnum:EQ).
            if valid-object(oPredicate) 
            and type-of(oPredicate,Pmfo.Core.BusinessLogic.IQueryPredicate)  
            and cast(oPredicate,Pmfo.Core.BusinessLogic.IQueryPredicate):StringValue = ? then
            do: 
                hFld = BufferHandle:buffer-field(cEqualFields).
                cMsg = subst("'&1' is required",BufferUtil:GetFieldLabel(hFld)).
            end.
            else 
                cMsg = pError:GetMessage(1).
                
            BufferError(cMsg,cParentField).
        end.
        else    
            BufferError(pError:GetMessage(1)).        
    end.    
    
    method protected IRelation GetLink(pcParent as character, pcName as character):
        define variable oIterator as IIterator no-undo.  
        define variable oRelation       as IRelation no-undo.
        if valid-object(Links) then
        do:
            oIterator = Links:Iterator().
            do while oIterator:HasNext():
                oRelation = cast(oIterator:Next(),IRelation).
                if oRelation:ParentName  = pcParent
                and oRelation:PublicName = pcName then
                   return oRelation.
            end.       
        end.
        return ?.
    end method. 
    
    method public void AssertDataset(phDataset as handle). 
        if phDataset:error then 
            undo, throw  new ValidationError().
    end method. 
    
    
end class.