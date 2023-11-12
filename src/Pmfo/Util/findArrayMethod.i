
/*------------------------------------------------------------------------
    File        : findArrayMethod.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : 11/11/2022 
    Notes       :
  ----------------------------------------------------------------------*/
    method public static integer Find (pValue as {1}, pArray as {1} extent):
        define variable i       as integer no-undo.
        do i = 1 to extent(pArray):
            if pArray[i] = pValue then 
                return i. 
        end.   
        return 0.        
    end method.
    
     