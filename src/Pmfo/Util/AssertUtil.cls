 
 /*------------------------------------------------------------------------
    File        : AssertUtil
    Purpose     : 
    Syntax      : 
    Description : Methods that are language related and dnoes not fit  in the other utilities for Buffer, Dataset, Json, Date, Relation, Query, Number, String and others.     
    Author(s)   : hdaniels
    Created     : Thu Apr 15 12:31:11 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/


using Pmfo.Core.Error.AssertReadOnlyError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.

block-level on error undo, throw.

class Pmfo.Util.AssertUtil: 
    
    // throw error if property is not called from constructor or its own getter
    method public static void AssertConstructorProperty ():
        define variable i as integer no-undo.
        define variable cCallerMethod as character no-undo.
        define variable cCaller as character no-undo.
        define variable cParentCallerMethod as character no-undo.
        define variable cParentCaller as character no-undo.
        define variable lError as logical no-undo.
        i = 2.
        do while true: 
            if program-name(i) <> ? then
            do: 
                cCallerMethod = entry(1,program-name(i)," ").
                cCaller       = entry(2,program-name(i)," ").
/*                 message*/
/*                      subst("programname(&1)",i + 1) program-name(i + 1) skip*/
/*                      subst("programname(&1)",i) program-name(i) skip        */
/*                      "caller" cCaller skip                                  */
/*                      "caller meth" cCallerMethod skip                       */
/*                      "found it"  cCallerMethod begins "propSet_"            */
/*                 view-as alert-box.                                          */
                if cCallerMethod begins "propSet_" then 
                do:
                    cParentCallerMethod = entry(1,program-name(i + 1)," ").
                    cParentCaller       = entry(2,program-name(i + 1)," "). 
                    leave.
                end.
                i = i + 1. 
               
            end.
            else 
                undo, throw new IllegalOperationError("AssertConstructorProperty is called without a property setter in the stack"). 
        end.         
        if cParentCallerMethod begins "propGet_" then
        do: 
            // not the right getter 
            if entry(2,cCallerMethod,"_") <> entry(2,cParentCallerMethod,"_") then
                lError = true.
        end.
        else if cParentCallerMethod <> entry(num-entries(cParentCaller,"."),cParentCaller,".") then
            lError = true.
          
        if lError then    
            undo, throw new AssertReadOnlyError("is read-only. It can only be set from the constructor or its own getter.",i). 
    end method.
    
    
end class.