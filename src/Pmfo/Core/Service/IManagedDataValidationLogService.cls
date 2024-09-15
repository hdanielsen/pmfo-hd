/*------------------------------------------------------------------------
  File        : IManagedDataValidationLogService
  Puurpose    : Asserts and/or Completes Managed Data Validation Messages 
                    
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Sep 06 11:29:20 EDT 2021
    Notes       : This is managed as a service    
                  The BE should have a Property that starts this.
                  The BE should call dispose as soon as data is submitted
                - DstaAccess requirements   
                    The DataAccess must commit all alerts in one transaction    
                    The server must throw an error if the UpdateRequest CommitScope  
                    requires more than one transaction. 
                    - All - always ok 
                    - Nested - Ok if one single record in top dataset
                    - all other scopes are disallowed 
----------------------------------------------------------------------------------*/
                  
 
using Pmfo.Core.Service.IInfrastructureService from propath.

interface Pmfo.Core.Service.IManagedDataValidationLogService : 
    // the infrastructure service that manages all infrastructure data for an update request 
    // that is to be checked or persisted together with associated data in submit
    
    define public property ParentInfrastructureService as IInfrastructureService no-undo get. set.
       
    // complete the validation message for create and/or update of single row
    // intended to be called from ValidaTable for each row (as cf current the cause for these is per record)
    method public void AssertAndCompleteRow(
        input phbuffer           as handle,  
        input pcKey              as character, 
        input piDataValidationId as integer,
        input pcCause            as character
    ). 
    
    // Assert that the validation message has been added to the log
    // for create and/or update of single row
    // intended to be called from validate actions that are not submitting to data access)
    method public void AssertRow(
        input phbuffer           as handle,  
        input pcKey              as character, 
        input piDataValidationId as integer,
        input pcCause            as character
    ). 
    
    // complete the validation message for action - with possibly many records and keys to only process a subset 
    // the cause for actions is supposed to be collected from only the involved records, 
    // The implemenation may not check the cause, only keys, mainly because it is more difficult.. 
    // but we may want to allow save of fewer records than initially approved (?) 
    method public void AssertAndComplete(
        input table-handle phTable,
        input pcKey              as character,   
        input iKeyValues         as integer extent,
        input piDataValidationId as integer,
        input pcAction           as character,
        input pcCause            as character 
     ).
    
    // Assert that the validation message has been added to the log 
    // the cause for actions is supposed to be collected from only the involved records, 
    // The implemenation may not check the cause, only keys, mainly because it is more difficult.. 
    // but we may want to allow save of fewer records than initially approved (?) 
    method public void Assert(
        input table-handle phTable,
        input pcKey              as character,   
        input iKeyValues         as integer extent,
        input piDataValidationId as integer,
        input pcAction           as character,
        input pcCause            as character 
     ).
    
    // complete the validation message for action - with possibly many records 
    // As of current the cause for actions is cpllected from all involved records, 
    method public void AssertAndComplete(
        input table-handle phTable,   
        input pcKey       as character, 
        input piDataValidationId as integer,
        input pcAction          as character,
        input pcCause           as character 
    ).
    
   
   
    // Assert that the validation message has been added to the log 
    // The cause for actions is cpllected from all involved records, 
    method public void Assert(
        input table-handle phTable,   
        input pcKey       as character, 
        input piDataValidationId as integer,
        input pcAction          as character,
        input pcCause           as character 
    ).
     
                                                                                                                                             
end interface.