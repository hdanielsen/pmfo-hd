 
 /*------------------------------------------------------------------------
    File        : Signature
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Tue Oct 15 14:03:23 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/
 
using Pmfo.Core.Common.ISignature.
using Pmfo.Core.Common.IParameter from propath.
using Progress.Lang.Object from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.StringUtil from propath.

block-level on error undo, throw.

class Pmfo.Core.Common.Signature implements ISignature: 

    define public property Parameters as IParameter extent no-undo 
    get.
    set. 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    // needed to support no paramter methods
    constructor public Signature ( ):
        super ().
    end constructor.
        
    constructor public Signature (pParameter as IParameter ):
        super ().
        extent(Parameters) = 1.
        Parameters[1] = pParameter. 
    end constructor.
   
    constructor public Signature (pParameters as IParameter extent ):
        super ().
        Parameters = pParameters. 
    end constructor.
    
    method public integer GetParameterPosition(pcname as character):
        define variable i as integer no-undo.
        do i = 1 to extent(Parameters):
            if Parameters[i]:name = pcname then 
               return i.
        end.     
        undo, throw new IllegalArgumentError(subst("Parameter &1 is not defined in Signature",pcName)).
    end method.
    
    method public override logical Equals  (pObject as Object):
        define variable oSignature as ISignature no-undo.
        define variable i as integer no-undo.
        define variable j as integer no-undo.
        define variable cMatches as character no-undo.
        if super:Equals(pObject) then 
            return true.
        
        if type-of(pObject,ISignature) then
        do:
            oSignature = cast(pObject,ISignature).
            if extent(oSignature:Parameters) = extent(Parameters) then
            do:
                // if no params and similar then they are equal
                if extent(Parameters) = ? then 
                    return true.
                do i = 1 to extent(Parameters):
                    do j = 1 to extent(oSignature:Parameters):
                        if Parameters[i]:name = oSignature:Parameters[j]:name 
                        and Parameters[i]:DataType = oSignature:Parameters[j]:DataType then
                        do:
                            if lookup(string(i),cMatches) = 0 then
                            do:
                                cMatches = cMatches 
                                         + (if cmatches = "" then "" else ",")
                                         + string(i).
                                leave.
                            end.             
                        end.             
                    end.    
                end.  
                  
                return num-entries(cMatches) = extent(Parameters).
            end.                 
        end.        
        
        return false.
    end method.
    
    method public override  character ToString():
        define variable cString as character no-undo.
        define variable i as integer no-undo.
        do i = 1 to extent(Parameters):
            cString = StringUtil:AppendEntry(cString, Parameters[i]:ToString()).
        end.
        return subst("(&1)",cString).    
    end method.    
end class.