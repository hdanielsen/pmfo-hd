
/*------------------------------------------------------------------------
    File        : NamedArrayNameEnum
    Purpose     : Pass to NamedArrayHolder constructor to specify name qualifier
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Oct 01 06:37:09 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

enum Pmfo.Core.Common.NamedArrayNameEnum :  

    define enum None  = 0 // free form 
                Public = 1 // entity/resource - table serializename
                Table = 2
                Buffer = 3
                BufferSerializeName = 4
                .

end enum.