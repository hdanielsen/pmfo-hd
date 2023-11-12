 
 /*------------------------------------------------------------------------
    File        : NumberUtil
    Purpose     : Numeric utilities
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Dec 06 18:34:42 EST 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.

block-level on error undo, throw.

class Pmfo.Util.NumberUtil: 
    method public static integer NumDecimals(pDe as decimal):
        if pDe = 0 then 
            return 0.
        if pDe <> ? then
            return NumDecimals(string(pDe)).
        return ?.
    end method.
    
    method public static integer NumDecimals(pFormat as character):
        if num-entries(pFormat,session:numeric-decimal-point) > 1 then 
            return length(entry(2,pFormat,session:numeric-decimal-point)).
        else
            return 0.    
    end method.
    
    // Any decimal also 0 will return false - in order to check before using a string as int key
    method public static logical IsInteger(pValue as character):
        return IsInteger(pValue, false).
    end.     
    
    // Any decimal also 0 will return false - in order to check before using a string as int key
    method public static logical IsInteger(pValue as character, plThrowifTooLarge as logical):
        define variable iVal as integer no-undo.
        do on error undo, throw:
            iVal = integer(pValue).
            return NumDecimals(pValue) = 0.
            catch e as Progress.Lang.Error :
                if plThrowifTooLarge and e:GetMessageNum(1) = 15747 then 
                    undo, throw  new AppError(subst("Value &1 is too large to fit in integer",pValue),?). 
                return false.             
            end catch.
        end.    
    end.     
    
    // Any decimal also 0 will return false - in order to check before using a string as int key
    method public static logical IsInt64(pValue as character):
        return IsInt64(pValue, false).
    end.     
    
    // Any decimal also 0 will return false - in order to check before using a string as int key
    method public static logical IsInt64(pValue as character, plThrowifTooLarge as logical):
        define variable iVal64 as int64 no-undo.
        do on error undo, throw:
            iVal64 = int64(pValue).
            return NumDecimals(pValue) = 0.
            catch e as Progress.Lang.Error :
                if plThrowifTooLarge and e:GetMessageNum(1) = 78 then 
                    undo, throw  new AppError(subst("Value &1 is too large to fit in int64",pValue),?). 
                  //  undo, throw  new AppError(subst("Value &1 is too large to fit in integer",pValue),?). 
                return false.             
            end catch.
        end.    
    end.     
    
    method public static logical IsNumber(pValue as character):
        return IsNumber(pValue, false).
    end method.
   
    method public static logical IsNumber(pValue as character, plThrowifTooLarge as logical):
        define variable dVal as decimal no-undo.  
        do on error undo, throw:
           dVal = decimal(pValue).
           return true.
           catch e as Progress.Lang.Error :
               if plThrowifTooLarge and e:GetMessageNum(1) = 536 then 
                   undo, throw  new AppError(subst("Value &1 is too large for number (decimal)",pValue),?). 
               return false.    
           end catch.
        end.
    end.     
       
    /* replace blank format with "9". Blank is used for unknown/null values on web  
       so we do not support formats that means blank for 0 
    **/
    method public static character FixBlankNumberFormat(pcFormat as character):
        define variable iDec as integer no-undo.    
        define variable cLastNumber as character no-undo.
        define variable iPos as integer no-undo.
        iDec = index(pcformat,".").
        // if decimal > 1 then check the char before the decimal delimiter
        if iDec > 1 then
        do:
            iPos = iDec - 1.
            cLastNumber = substr(pcformat,iPos,1).
            
        end.    
        // if no decimal then check the last digit
        else if iDec = 0 then
        do:
            iPos = length(pcformat).
            cLastNumber = substr(pcformat,iPos,1).
        end.    
        if cLastNumber = ">" or cLastNumber = "Z" then
           substr(pcformat,iPos) = "9".  
    
        return pcFormat.
    end.     
    
    
    
end class.