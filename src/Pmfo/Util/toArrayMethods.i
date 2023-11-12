
/*------------------------------------------------------------------------
    File        : toArrayMethods.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : hdaniels
    Created     : Wed Feb 13 10:44:25 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/
    method public static {1} extent ToArray(p1 as {1}):
        define variable vArray as {1} extent 1 no-undo.
        assign
            vArray[1] = p1.
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1}, p2 as {1}):
        define variable vArray as {1} extent 2 no-undo.
        assign
            vArray[1] = p1
            vArray[2] = p2
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}):
        define variable vArray as {1} extent 3 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}):
        define variable vArray as {1} extent 4 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}, p5 as {1}):
        define variable vArray as {1} extent 5 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            vArray[5]  = p5
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}, p5 as {1},
                                               p6 as {1}):
        define variable vArray as {1} extent 6 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            vArray[5]  = p5
            vArray[6]  = p6
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}, p5 as {1},
                                               p6 as {1},p7 as {1}):
        define variable vArray as {1} extent 7 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            vArray[5]  = p5
            vArray[6]  = p6
            vArray[7]  = p7
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}, p5 as {1},
                                               p6 as {1},p7 as {1}, p8 as {1}):
        define variable vArray as {1} extent 8 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            vArray[5]  = p5
            vArray[6]  = p6
            vArray[7]  = p7
            vArray[8]  = p8
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}, p5 as {1},
                                            p6 as {1},p7 as {1}, p8 as {1}, p9 as {1}):
        define variable vArray as {1} extent 9 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            vArray[5]  = p5
            vArray[6]  = p6
            vArray[7]  = p7
            vArray[8]  = p8
            vArray[9]  = p9
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}, p5 as {1},
                                            p6 as {1},p7 as {1}, p8 as {1}, p9 as {1}, p10 as {1}):
        define variable vArray as {1} extent 10 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            vArray[5]  = p5
            vArray[6]  = p6
            vArray[7]  = p7
            vArray[8]  = p8
            vArray[9]  = p9
            vArray[10] = p10
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}, p5 as {1},
                                             p6 as {1},p7 as {1}, p8 as {1}, p9 as {1}, p10 as {1},
                                             p11 as {1}):
        define variable vArray as {1} extent 11 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            vArray[5]  = p5
            vArray[6]  = p6
            vArray[7]  = p7
            vArray[8]  = p8
            vArray[9]  = p9
            vArray[10] = p10
            vArray[11] = p11
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}, p5 as {1},
                                            p6 as {1},p7 as {1}, p8 as {1}, p9 as {1}, p10 as {1},
                                            p11 as {1}, p12 as {1}):
        define variable vArray as {1} extent 12 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            vArray[5]  = p5
            vArray[6]  = p6
            vArray[7]  = p7
            vArray[8]  = p8
            vArray[9]  = p9
            vArray[10] = p10
            vArray[11] = p11
            vArray[12] = p12
            .
        return vArray.
    end method.
    
    method public static {1} extent ToArray(p1 as {1},p2 as {1}, p3 as {1}, p4 as {1}, p5 as {1},
                                            p6 as {1},p7 as {1}, p8 as {1}, p9 as {1}, p10 as {1},
                                            p11 as {1}, p12 as {1}, p13 as {1}):
        define variable vArray as {1} extent 13 no-undo.
        assign 
            vArray[1]  = p1
            vArray[2]  = p2
            vArray[3]  = p3
            vArray[4]  = p4
            vArray[5]  = p5
            vArray[6]  = p6
            vArray[7]  = p7
            vArray[8]  = p8
            vArray[9]  = p9
            vArray[10] = p10
            vArray[11] = p11
            vArray[12] = p12
            vArray[13] = p13
            
            .
        return vArray.
    end method.
    
    
    