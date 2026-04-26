method public static {1} extent concatenateArrays(array1 as {1} extent, array2 as {1} extent):
    define variable array1Length as integer no-undo.
    define variable array2Length as integer no-undo.
    define variable mergedArray  as {1}     extent no-undo.
    define variable i            as integer no-undo.
    
    assign
        array1Length        = if extent(array1) <> ? then extent(array1) else 0
        array2Length        = if extent(array2) <> ? then extent(array2) else 0.
    
    if array1Length > 0 or array2length > 0 then 
    do:    
        extent(mergedArray) = array1Length + array2Length.
    
        do i = 1 to array1Length:
            mergedArray[i] = array1[i].
        end.
        
        do i = 1 to array2Length:
            mergedArray[array1Length + i] = array2[i].
        end. 
    
    end.
    return mergedArray.
end method.
   
    