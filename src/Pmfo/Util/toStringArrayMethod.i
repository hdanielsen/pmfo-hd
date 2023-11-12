
/*------------------------------------------------------------------------
    File        : toStringArrayMethod.i
    Purpose     : 

    Syntax      : 

    Description : 

    Author(s)   : hdaniels
    Created     :  
    Notes       :
  ----------------------------------------------------------------------*/
   
    method public static character extent ToStringArray(pArray as {1} extent):
        define variable cValues as character extent no-undo.
        define variable i       as integer no-undo.
        extent(cValues) = extent(pArray).
        do i = 1 to extent(cValues):
            cValues[i] = string(pArray[i]).
        end.     
        return cValues.
    end.    
    
    