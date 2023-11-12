
/*------------------------------------------------------------------------
    File        : BeforeImageReadEnum
    Purpose     : Implemeted to allow after-image to be submitted with oinly changes by using AssignAfterFromBeforeImage    
    Syntax      :  
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Jun 25 08:50:31 EDT 2021
    Notes       : This should likely be deprecated 
                  The behavior for AssignAfterFromBeforeImage only kicks in if fields are missing from the after image and exists in the before image
                  so there seems to be no need to never do this. 
                - We already pass this option to JsonReader on POST for some cases in BusinessEntityOperationHandler
                - JsonReader:LoadBeforeImageFromJson now always assigns from after image for child data.                  
                  Before we remove this we may need some logic to enforce that all fields are present in a parent when it is in a PUT without modification to provide 
                  parent keys for the before image (parent-child strict contains-relationship and foreign-key hidden should not require client to add keys to children for update.
                  Note: There is (likely) no validation for this now, but the fact that we may only require changes in after image if the parent has an update
                        and require all fields if there is no updates is a bit odd. We should probably allow it, but add some more control
                - We may also consider to make UseJsonReaderOnPut default - maybe have an opposite option to use dataset read for simple cases?      
  ----------------------------------------------------------------------*/

enum Pmfo.Core.BusinessLogic.BeforeImageReadEnum :  

    define enum None   
              //  AllFieldsInBoth - difficult to enforce with calculated fields 
                AssignAfterFromBeforeImage 
              //  This is a theoretically valid option, but it seems questionable and confusing to support both this and AssignAfterFromBeforeImage in a framework
              //  AssignBeforeFromAfterImage   
                .

end enum.