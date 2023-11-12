
/*------------------------------------------------------------------------
    File        : compareArrayMethod.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Wed Feb 13 10:44:25 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
    method public static logical compareArrays(p1 as {1} extent, p2 as {1} extent, output pAdded as {1} extent, output pRemoved as {1} extent):
        define variable vArray as {1} extent no-undo.
        define variable i as integer no-undo.
        define variable i1length as integer no-undo.
        define variable i2length as integer no-undo.
        
        if extent(p1) = ? then
        do: 
           if extent(p2) = ? then 
              return true. 
           else do:  
              pRemoved = p2.
              return false.
           end. 
              
        end.
        else if extent(p2) = ? then
        do: 
           pAdded = p1.
           return false.
        end.
            
        assign
            i1length = extent(p1) 
            i2length = extent(p2) 
        .
        
        do i = 1 to i1length:
            
            if Pmfo.Util.Array:Find(p1[i],p2) = 0  then
            do: 
                extent(pAdded) = (if extent(pAdded) > 0 then extent(pAdded) else 0) + 1.
                pAdded[extent(pAdded)] = p1[i].
            end.
        end.     
        
        do i = 1 to i2length:
            if Pmfo.Util.Array:Find(p2[i],p1) = 0 then
            do: 
                extent(pRemoved) = (if extent(pRemoved ) > 0 then extent(pRemoved ) else 0) + 1.
                pRemoved[extent(pRemoved)] = p1[i].
            end.
        end.     
         
        return extent(pAdded) = ? and extent(pRemoved) = ?.
    end method.
   
    