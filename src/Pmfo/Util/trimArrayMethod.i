
/*------------------------------------------------------------------------
    File        : trimArrayMethod.i
    Purpose     : remove duplicates

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Wed Feb 13 10:44:25 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
    method public static {1} extent TrimArray(pVar as {1} extent):
        define variable vArray as {1} extent no-undo.
        define variable i as integer no-undo.
        define variable iLength as integer no-undo.
        
        if extent(pVar) > 0 then
        do: 
            ilength = extent(pVar).
            do i = 1 to ilength:
            
                if Pmfo.Util.Array:Find(pVar[i],vArray) = 0  then
                do: 
                    extent(vArray) = (if extent(vArray) > 0 then extent(vArray) else 0) + 1.
                    vArray[extent(vArray)] = pVar[i].
                end.
            end.     
            return vArray.
        end.
        return pVar.
    end method.
   
    