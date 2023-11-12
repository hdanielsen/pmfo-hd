
/*------------------------------------------------------------------------
    File        : mergeArrayMethod.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Wed Feb 13 10:44:25 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
    method public static {1} extent mergeArrays(p1 as {1} extent, p2 as {1} extent):
        define variable vArray as {1} extent no-undo.
        define variable i as integer no-undo.
        define variable i1length as integer no-undo.
        define variable i2length as integer no-undo.
        assign
            i1length = extent(p1) 
            i2length = extent(p2) 
            extent(vArray) = i1Length + i2length
        .
        
        do i = 1 to i1length + i2length:
            if i <= i1length then 
                vArray[i] = p1[i].
            else do:
                varray[i] = p2[i - i1length]. 
            end.             
        end.     
         
        return vArray.
    end method.
   
    