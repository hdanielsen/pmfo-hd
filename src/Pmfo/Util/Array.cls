 
 /*------------------------------------------------------------------------
    File        : Array
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Feb 13 10:27:54 EST 2019
    Notes       : Array utilities
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Ccs.Common.Support.ICharacterArrayHolder from propath.
using Ccs.Common.Support.IDateArrayHolder from propath.
using Ccs.Common.Support.IDateTimeArrayHolder from propath.
using Ccs.Common.Support.IDateTimeTzArrayHolder from propath.
using Ccs.Common.Support.IDecimalArrayHolder from propath.
using Ccs.Common.Support.IHandleArrayHolder from propath.
using Ccs.Common.Support.IInt64ArrayHolder from propath.
using Ccs.Common.Support.IIntegerArrayHolder from propath.
using Ccs.Common.Support.ILogicalArrayHolder from propath.
using Ccs.Common.Support.ILongcharArrayHolder from propath.
using Ccs.Common.Support.IPrimitiveArrayHolder from propath.
using OpenEdge.Core.String from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Util.StringUtil from propath.

class Pmfo.Util.Array: 

    {Pmfo/Util/toArrayMethods.i character}
    {Pmfo/Util/toArrayMethods.i date}
    {Pmfo/Util/toArrayMethods.i decimal}
    {Pmfo/Util/toArrayMethods.i handle}
    {Pmfo/Util/toArrayMethods.i integer}
    {Pmfo/Util/toArrayMethods.i int64}
    {Pmfo/Util/toArrayMethods.i logical}
    {Pmfo/Util/toArrayMethods.i Progress.Lang.Object} // organize using diesn ot see this
    
    {Pmfo/Util/mergeArrayMethod.i character}
    {Pmfo/Util/mergeArrayMethod.i date}
    {Pmfo/Util/mergeArrayMethod.i decimal}
    {Pmfo/Util/mergeArrayMethod.i handle}
    {Pmfo/Util/mergeArrayMethod.i integer}
    {Pmfo/Util/mergeArrayMethod.i int64}
    {Pmfo/Util/mergeArrayMethod.i logical}
    {Pmfo/Util/mergeArrayMethod.i Progress.Lang.Object} // organize using diesn ot see this
    
    {Pmfo/Util/findArrayMethod.i character}
    {Pmfo/Util/findArrayMethod.i date}
    {Pmfo/Util/findArrayMethod.i decimal}
    {Pmfo/Util/findArrayMethod.i handle}
    {Pmfo/Util/findArrayMethod.i integer}
    {Pmfo/Util/findArrayMethod.i int64}
    {Pmfo/Util/findArrayMethod.i logical}
    
    // just implement this without the findArrayMethod include to use Equals - it is obviously possible to do this based on
    // preprocessor parameters but this seems clearer 
    method public static integer Find (pValue as Progress.Lang.Object, pArray as Progress.Lang.Object extent):
        define variable i       as integer no-undo.
        do i = 1 to extent(pArray):
            if pArray[i]:Equals(pValue) then 
                return i. 
        end.   
        return 0.        
    end method.
    
    {Pmfo/Util/compareArraysMethod.i character}
    {Pmfo/Util/compareArraysMethod.i date}
    {Pmfo/Util/compareArraysMethod.i decimal}
    {Pmfo/Util/compareArraysMethod.i handle}
    {Pmfo/Util/compareArraysMethod.i integer}
    {Pmfo/Util/compareArraysMethod.i int64}
    {Pmfo/Util/compareArraysMethod.i logical}
    {Pmfo/Util/compareArraysMethod.i Progress.Lang.Object} // use full name as organize using does not see this
    
    {Pmfo/Util/toStringArrayMethod.i date}
    {Pmfo/Util/toStringArrayMethod.i datetime}
    {Pmfo/Util/toStringArrayMethod.i datetime-tz}
    {Pmfo/Util/toStringArrayMethod.i decimal}
    {Pmfo/Util/toStringArrayMethod.i handle}
    {Pmfo/Util/toStringArrayMethod.i integer}
    {Pmfo/Util/toStringArrayMethod.i int64}
    {Pmfo/Util/toStringArrayMethod.i logical}
    {Pmfo/Util/toStringArrayMethod.i longchar}
    {Pmfo/Util/toStringArrayMethod.i Progress.Lang.Object}
     
    {Pmfo/Util/substituteArrayMethod.i date}
    {Pmfo/Util/substituteArrayMethod.i datetime}
    {Pmfo/Util/substituteArrayMethod.i datetime-tz}
    {Pmfo/Util/substituteArrayMethod.i decimal}
    {Pmfo/Util/substituteArrayMethod.i handle}
    {Pmfo/Util/substituteArrayMethod.i integer}
    {Pmfo/Util/substituteArrayMethod.i int64}
    {Pmfo/Util/substituteArrayMethod.i logical}
    {Pmfo/Util/substituteArrayMethod.i longchar}
    
    method public static character extent Split (pcString as character):
        return Split(pcString,",").
    end method.
    
    method public static character extent Split (pcString as character, pcDelimiter as char):
        return Split(pcString,pcDelimiter,false).
    end method.    
    
    method public static character extent Split (pcString as character, pcDelimiter as char, plIgnoreDelimInQuotes as logical):
        define variable cValues       as character extent no-undo.
        define variable i             as integer no-undo.
        define variable cMask         as character no-undo.
        if pcString > "" then
        do:
            if plIgnoreDelimInQuotes and (index(pcString,"'") > 0 or index(pcString,'"') > 0) then
            do:
                // find a mask char not present in string
                do while true:
                    i = i + 1.
                    cMask = chr(i).
                    if index(pcString,cMask) = 0 then 
                       leave.
                end.     
                pcString = StringUtil:MaskCharInQuotes(pcString, pcDelimiter, cMask). 
            end. 
            extent(cValues) = num-entries(pcString,pcDelimiter).
            do i = 1 to extent(cValues):
                cValues[i] = entry(i,pcString,pcDelimiter).
                if cMask > "" then 
                   cValues[i] = replace(cValues[i],cMask,pcDelimiter).
            end.
        end.
        else if pcString = "" then
        do:
            extent(cValues) = 1.
        end.        
        return cValues.        
    end method.
    
    method public static integer extent SplitToInteger (pcString as character):
        return SplitToInteger(pcString,",").
    end method.
    
    method public static integer extent SplitToInteger (pcString as character, pcDelimiter as char):
        define variable iValues as integer extent no-undo.
        define variable i       as integer no-undo.
        define variable cValue  as character no-undo. 
        if pcString > "" then
        do:
            extent(iValues) = num-entries(pcString,pcDelimiter).
            do i = 1 to extent(iValues):
                cValue = entry(i,pcString,pcDelimiter). 
                if cValue = " " then 
                    undo, throw new IllegalArgumentError(subst("Entry &1 in '&2' is blank",i,pcString)). 
                iValues[i] = integer(entry(i,pcString,pcDelimiter)).
            end.
        end.
             
        return iValues.        
    end method.
    
    method public static handle extent SplitToHandle (pcString as character):
        return SplitToHandle(pcString,",").
    end method.
    
    method public static handle extent SplitToHandle (pcString as character, pcDelimiter as char):
        define variable hValues as handle extent no-undo.
        define variable i       as integer no-undo.
        if pcString > "" then
        do:
            extent(hValues) = num-entries(pcString,pcDelimiter).
            do i = 1 to extent(hValues):
               hValues[i] = widget-handle(entry(i,pcString,pcDelimiter)).
            end.
        end.
              
        return hValues.        
    end method.
    
    method public static character extent ToStringArray(pholder as IPrimitiveArrayHolder):
        case pHolder:GetClass():
            when get-class(ICharacterArrayHolder) then
                return cast(pholder,ICharacterArrayHolder):Value.
            when get-class(IDateArrayHolder) then
            do:
                return ToStringArray(cast(pholder,IDateArrayHolder):Value).
            end.
            when get-class(IDateTimeArrayHolder) then
            do:
                return ToStringArray(cast(pholder,IDateTimeArrayHolder):Value).
            end.
            when get-class(IDateTimeTzArrayHolder) then
            do:
                return ToStringArray(cast(pholder,IDateTimeTzArrayHolder):Value).
            end.
            when get-class(IDecimalArrayHolder) then
            do:
                return ToStringArray(cast(pholder,IDecimalArrayHolder):Value).
            end.
            when get-class(IHandleArrayHolder) then
            do:
                return ToStringArray(cast(pholder,IHandleArrayHolder):Value).
            end.
            when get-class(IIntegerArrayHolder) then
            do:
                return ToStringArray(cast(pholder,IIntegerArrayHolder):Value).
            end.
            when get-class(IInt64ArrayHolder) then
            do:
                return ToStringArray(cast(pholder,IInt64ArrayHolder):Value).
            end.
            when get-class(ILogicalArrayHolder) then
            do:
                return ToStringArray(cast(pholder,ILogicalArrayHolder):Value).
            end.
            when get-class(ILongcharArrayHolder) then
            do:
                return ToStringArray(cast(pholder,ILongcharArrayHolder):Value).
            end.
            otherwise 
                undo, throw new NotImplementedError(subst("ToStringArray for type '&1'",pholder:GetClass():TypeName)). 
                    
       end case.                          
    
    end method.    
    
    method public static character ToString (pcStrings as character extent):
        return ToString(pcStrings,",").
    end method.         
   
    method public static character ToString (piIntegers as integer extent):
        return ToString(piIntegers,",").
    end method.   
    
    method public static character ToString (pdDecimals as decimal extent):
        return ToString(pdDecimals,",").
    end method.   
    
    method public static character ToString (poStrings as String extent):
        return ToString(poStrings,",").
    end method.   
         
    method public static character ToString (pcStrings as character extent, pcDelimiter as char):
        define variable cList as character  no-undo.
        define variable i       as integer no-undo.
        define variable cDlm  as character no-undo.
        do i = 1 to extent(pcStrings):
            assign
                cList = cList + cDLm + pcStrings[i]
                cDlm  = pcDelimiter.
        end.   
        return cList.
    end method.
    
    method public static character ToString (piIntegers as integer extent, pcDelimiter as char):
        define variable cList as character  no-undo.
        define variable i       as integer no-undo.
        define variable cDlm  as character no-undo.
        do i = 1 to extent(piIntegers):
            assign
                cList = cList + cDLm + string(piIntegers[i])
                cDlm  = pcDelimiter.
        end.   
        return cList.
    end method.
    
    method public static character ToString (piDecimals as decimal extent, pcDelimiter as char):
        define variable cList as character  no-undo.
        define variable i       as integer no-undo.
        define variable cDlm  as character no-undo.
        do i = 1 to extent(piDecimals):
            assign
                cList = cList + cDLm + string(piDecimals[i])
                cDlm  = pcDelimiter.
        end.   
        return cList.
    end method.
    
    
    method public static character ToString (pStrings as String extent, pcDelimiter as char):
        define variable cList as character  no-undo.
        define variable i       as integer no-undo.
        define variable cDlm  as character no-undo.
        do i = 1 to extent(pStrings):
            assign
                cList = cList + cDLm + pStrings[i]:Value
                cDlm  = pcDelimiter.
        end.   
        return cList.
    end method.
         
end class.