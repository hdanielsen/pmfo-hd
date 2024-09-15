
/*------------------------------------------------------------------------
    File        : ManagedDataValidationRequestTypeEnum
    Purpose     : Decides how to process Managed Data Validations
                  Managed means persisted in db in a log thaty can be 
                  stored priort ot transactiuon and then validared and 
                  or completed in the actual transaction  
                     
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Apr 30 08:21:21 EDT 2024
    Notes       : 
  ----------------------------------------------------------------------*/

enum Pmfo.Core.BusinessLogic.ManagedDataValidationRequestTypeEnum :  

    define enum None     // collect messages so the  caller can grab them and return them     
                Collect  // collect the messages and add to the Request to be returned as Messages  
                Commit   // assert approval and commit to the log 
                Assert   // assert approval but don't save  
                .
end enum.