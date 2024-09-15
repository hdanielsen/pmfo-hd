
/*------------------------------------------------------------------------
    File        : compareArrayMethod.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Wed Feb 13 10:44:25 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
    method public static logical CompareArrays(p1 as {1} extent, p2 as {1} extent, output pNotInP1 as {1} extent, output pNotInP2 as {1} extent):
        define variable vArray as {1} extent no-undo.
        define variable i as integer no-undo.
        define variable i1length as integer no-undo.
        define variable i2length as integer no-undo.
        
        if extent(p1) = ? then
        do: 
           if extent(p2) = ? then 
              return true. 
           else do:  
              pNotInP2 = p2.
              return false.
           end. 
              
        end.
        else if extent(p2) = ? then
        do: 
           pNotInP1 = p1.
           return false.
        end.
            
        assign
            i1length = extent(p1) 
            i2length = extent(p2) 
        .
        
        do i = 1 to i1length:
            
            if Pmfo.Util.Array:Find(p1[i],p2) = 0  then
            do: 
                extent(pNotInP1) = (if extent(pNotInP1) > 0 then extent(pNotInP1) else 0) + 1.
                pNotInP1[extent(pNotInP1)] = p1[i].
            end.
        end.     
        
        do i = 1 to i2length:
            if Pmfo.Util.Array:Find(p2[i],p1) = 0 then
            do: 
                extent(pNotInP2) = (if extent(pNotInP2 ) > 0 then extent(pNotInP2 ) else 0) + 1.
                pNotInP2[extent(pNotInP2)] = p1[i].
            end.
        end.     
         
        return extent(pNotInP1) = ? and extent(pNotInP2) = ?.
    end method.
   
    