
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
        if extent(p1) <> ? then
        do: 
            i1length = extent(p1).
            extent(vArray) = i1Length.
        end.
        if extent(p2) <> ? then
            i1length = extent(p2).
            
        do i = 1 to i1length + i2length:
            if i <= i1length then 
                vArray[i] = p1[i].
            else do:
                if Pmfo.Util.Array:Find(p2[1],varray) = 0 then.
                do:  
                    extent(varray) = (if extent(varray) = ? then 0 else extent(varray)) + 1.              
                    varray[extent(varray)] = p2[i - i1length].
                end.    
            end.             
        end.     
         
        return vArray.
    end method.
   
    