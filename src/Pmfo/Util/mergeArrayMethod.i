method public static {1} extent mergeArrays(array1 as {1} extent, array2 as {1} extent):
    define variable i               as integer no-undo.
    define variable array1Length    as integer no-undo.
    define variable array2Length    as integer no-undo.
    define variable mergedArray     as {1} extent no-undo.
    define variable parsedDupsArray as {1} extent no-undo.
    define variable countRight      as integer no-undo.
    define variable countLeft       as integer no-undo.
    
    if extent(array1) = ? then
        return array2.  
    else 
        array1Length = extent(array1).
        
    if extent(array2) = ? then
        return array1.
    else 
        array2Length = extent(array2).
        
    // copy the first array as-is
    extent(mergedArray) = array1Length.
    do i = 1 to array1Length:
        mergedArray[i] = array1[i].
    end.
           
    do i = 1 to array2Length:
        if Pmfo.Util.Array:Find(array2[i],mergedArray) = 0 then 
        do:
            extent(mergedArray) = (if extent(mergedArray) = ? then 0 else extent(mergedArray)) + 1.              
            mergedArray[extent(mergedArray)] = array2[i].
        end.
        else do:
            // also merge if there are more occurrences on the right than the left and currently merged  
            countRight = Pmfo.Util.Array:Count(array2[i],array2).
            if countRight > 1 then
            do:
                if countRight gt Pmfo.Util.Array:Count(array2[i],mergedArray) then
                do:
                    countLeft = Pmfo.Util.Array:Count(array2[i],array1).
                    if countRight gt countLeft then 
                    do:
                        // track the dups 
                        extent(parsedDupsArray) = (if extent(parsedDupsArray) = ? then 0 else extent(parsedDupsArray)) + 1.              
                        parsedDupsArray[extent(parsedDupsArray)] = array2[i].
                        // Wait unil we reach the one that truly is the extra one(s) oin the right 
                        // This is a bit picky, but ensures that all the ones that are only or extra in the righ array keeps the original order in the merged array  
                        if Pmfo.Util.Array:Count(array2[i],parsedDupsArray) > countLeft then 
                        do:   
                            extent(mergedArray) = (if extent(mergedArray) = ? then 0 else extent(mergedArray)) + 1.              
                            mergedArray[extent(mergedArray)] = array2[i].
                        end.
                    end.
                end.
            end.     
        end.
    end.
     
    return mergedArray.
end method.
    