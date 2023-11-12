
/*------------------------------------------------------------------------
    File        : RowStateEnum
    Purpose     : Define state for row update
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Thu Apr 23 20:37:08 EDT 2020
    Notes       : The numbers are mapped to row-state since the known 
                  use case is an extension of row-state.  
  ----------------------------------------------------------------------*/

enum Pmfo.Core.BusinessLogic.RowStateEnum :  

    define enum None              = 0
                Deleted           = 1
                Modified          = 2
                Created           = 3
                CreatedOrModified = 4
                Action            = 5. // post, but may need different handling

end enum.