
/*------------------------------------------------------------------------
    File        : ActionScopeEnum
    Purpose     : Define the scope of the action in regard to posted data 
                  
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sat Aug 17 08:50:37 EDT 2019
    Notes       : Row - calls the action for each posted row/record
                  Table - calls the action once assuming the action 
                  will do a for each     
                - Actions are  (when this is written) supported in 
                  the data access layer. But the scope is assumed to be needed
                  also in the application layer when/if supported there 
  ----------------------------------------------------------------------*/

enum Pmfo.Core.BusinessLogic.ActionScopeEnum :  

    define enum Row 
                Table .

end enum.