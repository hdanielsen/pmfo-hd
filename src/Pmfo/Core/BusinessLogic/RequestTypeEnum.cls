
/*------------------------------------------------------------------------
    File        : RequestTypeEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Apr 17 11:26:51 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

enum Pmfo.Core.BusinessLogic.RequestTypeEnum flags:  

    define enum SingleRow 
                SingleTopTable 
                MultiTable 
                MultiTableHideEmpty = MultiTable.

end enum.