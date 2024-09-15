
/*------------------------------------------------------------------------
    File        : RequestUtilOptionEnum
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Feb 21 07:32:10 EST 2024
    Notes       : 
  ----------------------------------------------------------------------*/

enum Pmfo.Util.RequestUtilOptionEnum flags:  

    define enum None 
                SkipEmpty 
                IncludeObsolete 
                SkipEmptyIncludeObsolete = SkipEmpty,IncludeObsolete
                .
end enum.