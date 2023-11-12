
/*------------------------------------------------------------------------
    File        : MethodBindingEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 13 11:55:46 EDT 2022
    Notes       : 
  ----------------------------------------------------------------------*/

enum Pmfo.Core.BusinessLogic.MethodBindingEnum :  

    define enum Any        // Any context - single entity or collection  
                Entity     // the methiod is bound to a single entity (url must have key for the resource segment)
                Collection // the methiod is bound to a collection (url must not have key for the resourece segment)
                .

end enum.