 
 /*------------------------------------------------------------------------
    File        : Error.StackError
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sept 2019 - abstracted from NotImplementedError
    Notes       : Abstract class for errors thst refers to call stack 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Error.ApplicationError.
using OpenEdge.Net.HTTP.StatusCodeEnum from propath.
using OpenEdge.Core.StringConstant from propath.

block-level on error undo, throw.

class Pmfo.Core.Error.StackError abstract inherits ApplicationError: 
    constructor public StackError ( input pmsg as character ):
        super (input pmsg).
    end constructor.
    
    method protected character GetMethodName():
        return GetMethodName(3).
    end method.
    
    method protected character GetMethodName(piStack as integer):
        define variable cFullMethod as character no-undo.
        define variable cClass      as character no-undo.
        define variable cMethod     as character no-undo.
        cFullMethod = program-name(piStack).
        
        if cFullMethod > "" and num-entries(cFullMethod,StringConstant:SPACE) > 1 then 
        do:
            cFullMethod = subst("&1:&2",entry(2,cFullMethod,StringConstant:SPACE),entry(1,cFullMethod,StringConstant:SPACE)).
            if num-entries(cFullMethod,":") > 1 then 
            do: 
                cClass  = entry(1,cFullMethod,":").
                cMethod = entry(2,cFullMethod,":").
                if cMethod begins "propGet_" then
                    cFullMethod = subst("&1 &2:&3","Get",cClass,substr(cMethod,9)).  
                else if cMethod begins "propSet_" then
                    cFullMethod = subst("&1 &2:&3","Set",cClass,substr(cMethod,9)).  
            end.     
        end.
        else 
            cFullMethod = "method".
        return cFullMethod.     
    end.      
    
end class.