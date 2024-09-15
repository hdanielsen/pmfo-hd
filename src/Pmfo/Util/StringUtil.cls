 
 /*------------------------------------------------------------------------
    File        : StringUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 10 16:13:25 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

block-level on error undo, throw.

using OpenEdge.Core.Session from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.NotImplementedError from propath.
using Pmfo.Core.Error.OutOfBoundsError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.DateUtil from propath.
using Pmfo.Util.StringUtil from propath.
 

class Pmfo.Util.StringUtil:
    method public static integer FindAnyWordEnd (pcString as character,pcWords as character):
        define variable cDummy as character no-undo.
        return FindAnyWord(pcString,pcWords,true, output cDummy).   
    end method.
    
    method public static integer FindAnyWord (pcString as character,pcWords as character):
        define variable cDummy as character no-undo.
        return FindAnyWord(pcString,pcWords,false,output cDummy).   
    end method.
    
    method public static integer FindAnyWord (pcString as character,pcWords as character, output pcFoundWord as character):
        return FindAnyWord(pcString,pcWords,false, output pcFoundWord).   
    end method.
    
    method private static integer FindAnyWord (pcString as character,pcWords as character, plEnd as logical,  output pcFoundWord as character):
        define variable i as integer no-undo.
        define variable iPos as integer no-undo.
        define variable cWord as character no-undo.
        // add blank at end so we can index " " + word + " " 
        pcString = pcString + " ".
        do i = 1 to num-entries(pcWords):
            cWord = subst(" &1 ",entry(i,pcWords)).
            iPos = index(pcString,cWord).
            if iPos > 0 then 
            do:
               pcFoundWord = entry(i,pcWords).     
               if plEnd then 
                   return  iPos + (length(cWord) - 1).
               else      
                   return iPos + 1.
              
            end.   
        end.
        return 0.        
    end method.
    
    /* find the word that is first in a string while avoiding index 0 */
    method public static integer MinNotZeroPosition (pcString as character,pcWords as character):
        return MinNotZeroPosition(pcString, pcWords, 0).        
    end method.
    
    /* find the word that is first in a string while avoiding index 0 from a specific positioon */
    method public static integer MinNotZeroPosition (pcString as character,pcWords as character, piStart as integer):
        define variable i as integer no-undo.
        define variable iMin as integer no-undo.
        define variable iPos as integer no-undo.
        define variable iLength as integer no-undo.
        pcString = pcString + " ".
        iLength = length(pcString).
        iMin = iLength.
        do i = 1 to num-entries(pcWords):
            iPos = index(pcString,subst(" &1 ",entry(i,pcWords))).
            if iPos > 0 and iPos >= piStart then
               iMin = min(iMin,iPos). 
        end.
        if iMin < iLength then 
            return iMin + 1. // ignore the space
        else 
            return 0.        
    end method.  
    
    method public static character GetIndefiniteArticle(pcword as character):
        if length(pcWord) > 0 then  // u and y are treared as constonants in beginning of word 
            if lookup(substr(pcword,1,1),"a,e,i,o") > 0 then 
                return "an".
            else 
               return "a".     
    end method.    
    
    method public static logical HasNumber(pcword as character):
        define variable i as integer no-undo.
        if IsInteger(pcWord) then
            return true.
        do i = 1 to 9:
            if index(pcword,string(i)) > 0 then
                return true.
        end.
        return false.       
    end method.    
    
    /* return the second entry of a paired list if the name is first of the pair
       return blank if not found 
       The main purpose of this is to not get tripped by same name entries in the 
       first of the pairs, which is a problem when using ABL lookup  
       Use GetUseName if you want to use the searched value of not found 
       */
    
    method public static character GetMappedName(pcName as char,pcNameMap as longchar):
        define variable lDummy as logical no-undo.
        return GetMappedName(pcname,pcNameMap, output lDummy).
    end method.
       
    method public static character GetMappedName(pcName as char,pcNameMap as longchar, output plWrongEntry as logical):
        define variable iLookup as integer no-undo.
        define variable cSearch as character no-undo.
        define variable i       as integer no-undo.
        
        iLookup = lookup(pcName,pcNameMap). 
        if iLookup > 0 then 
        do:
            // if we find a name in the wrong place then loop through the rest of entries to see if there is a correct match
            // (it would be more efficient to cut the list and do antoher lookup..) 
            if iLookup modulo 2 = 0 then do: 
                do i = iLookup + 1 to num-entries(pcNameMap) by 2:
                    if entry(i,pcNameMap) = pcName then 
                    do:
                        return string(entry(i + 1,pcNameMap)).
                    end.    
                end.
                plWrongEntry = true. // TODO  seems unlikely that we really want to ignore a bad reference? default or parameter plthrow?        
            end.
            else do:
                return string(entry(iLookup + 1,pcNameMap)).  
            end.        
        end.
        return "".
    end method.    
    
    /* return the second entry of a paired list if the name is the first of the pair
       return the original name if not found */
    method public static character GetUseName(pcName as char,pcNameMap as char):
        define variable cMappedName as character no-undo. 
        cMappedName = GetMappedName(pcName,pcnameMap).
        if cMappedName = "" then
            return pcName.
        else 
            return cMappedName.    
    end method.    
    
    method public static integer NumDecimalsInFormat ( pcFormat as char):
       define variable cdummy  as character no-undo.
       define variable ilength as integer no-undo.
       define variable icount  as integer no-undo.
       define variable i as integer no-undo.
       define variable cChar as character no-undo.
       define variable cDecformat as character no-undo.
       do on error undo, throw:
          cdummy = string(0,pcformat). // force invalid format error
          catch e as Progress.Lang.Error :
              undo, throw new IllegalArgumentError(e:GetMessage(1)).
          end catch.
       end. 
       if num-entries(pcFormat,session:numeric-decimal-point) > 1 then 
       do:
           cDecFormat = entry(2,pcFormat,session:numeric-decimal-point).
           ilength = length(cDecFormat). 
           do i = 1 to ilength on error undo, throw:
              cChar = substring(cDecFormat,i,1).
              if cchar <> "-" and cChar <> "+" then
              do:   
                 if cChar <> "<" then 
                    int(cChar). // this will throw error if valid mask that we don't want to count 
                 icount = icount + 1.
              end.
              catch e as Progress.Lang.Error:
                      
              end catch.
               
           end.
           return icount.
       end.
       else
           return 0.    
    end method.     
  
    method public static longchar MaskQuotes ( pcString as longchar,pcReplaceChar as char ) :
        return MaskCharInQuotes(pcString,?,pcReplaceChar).
    end.     
    
    method public static longchar MaskQuotes ( pcString as longchar,pcReplaceChar as char,output pcreplaces as character extent ) :
        return MaskCharInQuotes(pcString,?,pcReplaceChar,output pcreplaces ).
    end.     
  
    /*------------------------------------------------------------------------------
      Purpose:    Utility function that masks all quoted strings in the passed string
                  Used to allow counting entries or other string operation where blank is 
                  a word delimiter                 
      Parameters: pcString = string that might have embedded quoted strings. 
                             must be syntactically correct 
                             - paired single or double.  
           pcReplaceChar = single char to insert in quoted positions. 
           
      Notes: Used in various query manipulation before looking for keywords 
            (a function that could find a particular token outside of the 
             quotes would possibly be "better", but (too?) expensive... )   
        ------------------------------------------------------------------------------*/  
    // output the chr(number) to allow callers to use replace to put it back
    method public static longchar MaskCharInQuotes( pcString as longchar, pcHideChar as character,output pichar as integer)  :
        define variable cReplaces as character extent no-undo.
        define variable cMask     as character no-undo.
        // find a mask char not present in string
        do while true:  
            if pichar > 4 then
                undo, throw new NotImplementedError("Mask char in quotes of string with chr(1), chr(2) , chr(3) and chr(4) present").
            pichar = pichar + 1.
            cMask = chr(pichar).
            if index(pcString,cMask) = 0 then 
               leave.
        end.    
        return MaskCharInQuotes(pcString, pcHideChar, cMask, false, output cReplaces).
    end method.
    
    method public static longchar MaskCharInQuotes( pcString as longchar, pcHideChar as character,pcReplaceChar as character)  :
        define variable cReplaces as character extent no-undo.
        return MaskCharInQuotes(pcString, pcHideChar, pcReplaceChar, false, output cReplaces).
    end method.
    
    method private static longchar MaskCharInQuotes( pcString as longchar, pcHideChar as character,pcReplaceChar as character, output pcreplaces as character extent)  :
        define variable cReplaces as character extent no-undo.
        return MaskCharInQuotes(pcString, pcHideChar, pcReplaceChar, true, output pcreplaces).
    end method. 
    
    method private static longchar MaskCharInQuotes( pcString as longchar, pcHideChar as character,pcReplaceChar as char,plTrack as logical, output pcreplaces as character extent) :
        define variable iChr          as integer   no-undo.
        define variable iState        as integer   no-undo.
        define variable cQuote        as character no-undo.         
        define variable cChr          as character no-undo.   
        
        define variable iSingleQuote  as integer no-undo.  
        define variable iDoubleQuote  as integer no-undo.
        define variable iStart        as integer no-undo.
        define variable iQuoteCount   as integer no-undo.
        define variable iPos          as integer no-undo.
        define variable i2Pos         as integer no-undo.
        define variable cString       as character no-undo.
        define variable iHide         as integer no-undo.
        
        if length(pcHideChar) > 1 or length(pcReplacechar) > 1 then
        do:
            
            if pcHideChar = ? then 
                 undo, throw new IllegalArgumentError(
                                                     subst("Replace character '&1' is invalid for mask  of all quoted data. It can only be a songle character.",
                                                           pcReplaceChar)
                                                      ).   
            if length(pcReplaceChar) <> length(pcHideChar) then 
                undo, throw new IllegalArgumentError(
                                                     subst("Parameters for Hide Character '&1' and Replace character '&2' does not have the same length",
                                                           pcHideChar,
                                                           pcReplaceChar)
                                                      ).
            if plTrack = true then 
                undo, throw new UnsupportedOperationError("Cannot track mask of multiple characters"). 
                                                         
        end.
        
        cString = pcString.
        if pcReplaceChar = ? or pcReplaceChar = "":U then
            pcReplaceChar = '@':U.
        
        iSingleQuote = index(cString,"'":U).
        iDoubleQuote = index(cString,'"':U).
        
        if iSingleQuote > 0 and iDoubleQuote > 0 then
           iStart = min(iSingleQuote,iDoubleQuote).
        else if iSingleQuote > 0 then
           iStart = iSingleQuote.
        else 
           istart = iDoubleQuote.
       
        if iStart > 0 then 
        do:
            do iChr = iStart to length(cString):
                cChr = substring(cString,iChr,1).
               
                case iState:
                    when 0 then 
                    do:
                        if cChr = '"':U or cChr = "'":U then  
                        do:
                            assign      
                                iState = 1
                                cQuote = cChr
                                iQuoteCount = iQuoteCount + 1.
                            if plTrack then 
                               extent(pcreplaces) = (if extent(pcreplaces) = ? then 0 else extent(pcreplaces)) + 1.      
                        end.        
                    end.  
                    when 1 then /* start quote is found */
                    do:
                        if cChr = '~~':U then iState = 2. /* ignore next */
                        if cChr = cQuote then
                        do: 
                            iState = 3. /* possible end */
                            iQuoteCount = iQuoteCount + 1.
                        end.     
                    end.  
                    when 2 then /* prev char was tilde */ 
                        iState = 1.
                    when 3 then /* possible end quote was found */ 
                    do:
                        /* if another quote then we're still in quoted string */
                        if cChr = cQuote then 
                        do:
                            iState = 1.
                            iQuoteCount = iQuoteCount + 1.
                        end.    
                        else do:
                           iState = 0.
                        end.        
                    end.             
                end case.        
                if iState > 0 then 
                do:
                    if length(pcHideChar) > 1 then 
                    do:
                        iHide = index(pcHideChar,cChr).
                        if iHide > 0 then  
                            substring(cString,iChr,1) = substr(pcReplaceChar,iHide,1).
                    end.
                    else
                    do:     
                        // hidechar ? mask every character
                        if pcHideChar = ? 
                        or substring(cString,iChr,1) = pcHideChar then
                        do: 
                            substring(cString,iChr,1) = pcReplaceChar.
                            if plTrack then
                                pcReplaces[extent(pcReplaces)] = pcReplaces[extent(pcReplaces)] + substring(pcString,iChr,1).
                        end.    
                    end.
                end.    
            end.    
        end.
        if iQuoteCount modulo 2 <> 0 then 
        do:
            if iQuoteCount = 1 then 
                undo, throw new IllegalArgumentError(StringUtil:UpperCaseFirst(
                                                            subst("&1 quote found in string: &2",
                                                                  if cQuote = "'" then "single" else "double",
                                                                  pcString
                                                                 )
                                                            )
                                                    ).     
            else
                undo, throw new IllegalArgumentError(StringUtil:UpperCaseFirst(
                                                        subst("&1 quote found inside &1 quotes in string: &2",
                                                               if cQuote = "'" then "single" else "double",
                                                              pcString
                                                             )
                                                        )
                                                ).     
            
        end.        
        return cString.   
        
    end method.
    
    method public static character UnquoteValue(pcString as character) :
        define variable cQuote as character no-undo.
        return UnquoteValue(pcString, output cQuote).
    end method.
    
    method public static longchar UnquoteValue(pcString as longchar) :
        define variable cQuote as character no-undo.
        return UnquoteValue(pcString, output cQuote).
    end method.
    
    method public static longchar UnquoteValue(pcString as longchar, output plHasQuote as logical) :
        define variable cQuote as character no-undo.
        pcString = UnquoteValue(pcString, output cQuote).
        plHasQuote = cQuote = "'" or cQuote = '"'.
        return pcString. 
    end method.
    
    
    method public static character UnquoteValue(pcString as character, output plHasQuote as logical) :
        define variable cQuote as character no-undo.
        pcString = UnquoteValue(pcString, output cQuote).
        plHasQuote = cQuote = "'" or cQuote = '"'.
        return pcString. 
    end method.
    
    method public static character UnquoteValue(pcValue as character, output pcQuote as character) :
        define variable cLong as longchar no-undo.
        cLong = pcValue.
        return string(UnquoteValue(cLong, output pcQuote)).
    end method.    
    
    method public static longchar UnquoteValue(pcValue as longchar, output pcQuote as character) :
        define variable lSingleQuote as logical no-undo.
        define variable lDoubleQuote as logical no-undo.
        define variable cEnd         as character no-undo.
        define variable cValue       as character no-undo.
        define variable iPos         as integer no-undo.
        lSingleQuote = pcValue begins "'".
        lDoubleQuote = pcValue begins '"'.
             
        if lSingleQuote or lDoubleQuote then
        do: 
            // output - and used to check end 
            pcQuote = substr(pcValue,length(pcValue)).
            
            if (lSingleQuote and pcQuote <> "'")
            or (lDoubleQuote and pcQuote <> '"') then 
                undo, throw new IllegalArgumentError(subst("Missing end quote in value '&1'",pcValue)).
                
            cValue = substr(pcValue,2,length(pcValue) - 2).
            
            do while true:
                iPos = index(cValue,pcQuote,iPos + 1).
                if iPos > 0 then 
                do: 
                    if length(cValue) > iPos and substr(cValue,ipos + 1,1) = pcQuote then
                    do: 
                        substr(cValue,ipos,1) = "".
                    end.    
                    else do:
                        undo, throw new IllegalArgumentError(subst("Invalid quote in quoted value &1",pcValue)).
                    end.   
                end.
                else 
                    leave.
            end.    
            return cValue.
        end.
        return pcValue.    
    end method. 
    
    
    // Extract an ABL statement starting from the first occurance of a word 
    method public static char ExtractStatement(pcStatement as char, pcword as char, pcString as longchar, input-output piStart as int64) :
        define variable iFound        as integer no-undo.
        define variable iSearch       as integer no-undo.
        define variable iSpace as integer no-undo.
        define variable cMaskedString as longchar no-undo.
        define variable iPeriod       as integer no-undo.
        define variable cStatement    as character no-undo.
        define variable cWordNoPeriod as character no-undo.
        define variable iStart        as integer no-undo.
        define variable cAfter        as character no-undo.
        iFound = index(pcString,pcWord,piStart).
         
        if iFound > 0 then do:
            do iSpace = 1 to 6 by 1:
                iSearch = index(pcString,subst("&1&2&3",pcStatement,fill(" ",iSpace),pcWord),piStart).
                if iSearch > 0 then 
                do:
                    iFound = iSearch.
                    leave.
                end.   
            end.   
            if pcStatement <> "assign" and iSearch = 0 then
            do:
                piStart = 0. 
                return ?.
            end.
            cMaskedString = MaskQuotes(substr(pcString,iFound),chr(1)).
            cWordNoPeriod = replace(pcWord,"."," ").
            if cWordNoPeriod <> pcWord then
               cMaskedString = replace(cMaskedString,pcWord,cWordnoPeriod).
               
            iStart = 1.   
            // check if period really is end of statement or qualifier for field  
            do while true:
                iPeriod = index(cMaskedString,".",iStart).
                if length(cMaskedString) > iPeriod then
                do:
                    cAfter = substr(cMaskedString,iPeriod + 1,1). 
                    /* if the period is immediately followed by a character then continue search */
                    if cAfter <> "" and cAfter <> StringConstant:LF and cAfter <> StringConstant:CR then
                        istart = iPeriod + 2. // skip the char we found
                    else 
                        leave.
                end.
                else 
                    leave.      
            end.
            cStatement = string(substr(pcString,iFound,iPeriod)).
            piStart = iFound.
            return cStatement.        
       end.
       pistart = 0.
       return ?.
    end method.
    
    // Get the first parenthesis in the string from start to end  
    //  - optionally exclude the parenthesis 
    method public static char ExtractParenthesis ( pcString as longchar,plContentOnly as log ) :
        define variable cExpression as character no-undo. 
        cExpression = ExtractParenthesis(pcString).
        if plContentOnly then 
            return substr(cExpression,2,length(cExpression) - 2).  
        else 
            return cExpression.  
    end method.    
    
    method public static char ExtractParenthesis ( pcString as longchar ) :
        define variable iStart as integer no-undo.
        return ExtractParenthesis(pcString, output iStart).
    end method.
    
    // Get the first parenthesis in the string from start to end  
    method public static char ExtractParenthesis ( pcString as longchar , output piStart as integer ) :
        /*------------------------------------------------------------------------------
          Purpose: Utility function that extracts a parenthesis from a string
                   ignoring nested parenthesises               
        Parameters: pcString = string that might have a parenthesis
        Returns the first parenthsis with content.  
        ------------------------------------------------------------------------------*/
        define variable iChr          as integer   no-undo.
        define variable iState        as integer   no-undo.
        define variable cQuote        as character no-undo.         
        define variable cChr          as character no-undo.   
        
        define variable iSingleQuote  as integer no-undo.  
        define variable iDoubleQuote  as integer no-undo.
       
        piStart = index(pcString,"(").
        if piStart > 0 then 
        do:
            do iChr = piStart to length(pcString):
                cChr = substring(pcString,iChr,1).
                
                if cChr = ")" then 
                do:
                    iState = istate - 1.
                end.
                else if cChr = "(" then
                do:
                    iState = istate + 1.  
                end.  
                if iState = 0 then 
                   return string(substring(pcString,piStart,ichr - (piStart - 1))).
            end. 
            if istate <> 0 then 
                undo, throw new IllegalArgumentError(subst("'&1' is missing an end parenthesis",pcString)).   
        end.
        return ?.
    end method. 
    
    // convert a comma separated list to plain language with and as the last delimiter 
    method static public character GetMessageFromEntries(pcEntries as char):
        return GetMessageFromEntries(pcEntries,"and").
    end method.    
    
    // convert a comma separated list to plain language with and as the last delimiter 
    method static public character GetMessageFromEntries(pcEntries as char,plQuoted as logical):
        return GetMessageFromEntries(pcEntries,"and",plQuoted).
    end method.   
    
    // convert a comma separated list to plain language with paramter to specifify last delimiter (typically "and" or "or") 
    method static public character GetMessageFromEntries(pcEntries as char, pcAndOr as char):
        return GetMessageFromEntries(pcEntries, pcAndOr, false). 
    end method.  
    
    method static public character GetMessageFromEntries(pcEntries as char, pcAndOr as char, plQuoted as logical):
        define variable i as integer no-undo.
        define variable cEntry as character no-undo.
        define variable cMsg as character no-undo.
        define variable iEntries as integer no-undo.
        iEntries = num-entries(pcEntries).
        do i = 1 to iEntries:
            cEntry = entry(i,pcEntries).
            cmsg = cMsg
                 + (if cmsg = "" then "" else if i = IEntries then subst(" &1 ",pcAndOr) else ", ")
                 + if plQuoted then quoter(cEntry,"'") else cEntry. 
        end.    
        return cmsg.
    end method.  
    
    method static character RemoveEntryFromList(piEntry as integer, pcList as character ):
        return RemoveEntryFromList(piEntry,pcList,",").
    end method.
    method static character RemoveEntryFromList(piEntry as integer, pcList as character, pcDelimiter as characte ):
        if num-entries(pcList,pcDelimiter) >= piEntry then 
        do: 
            entry(piEntry,pcList,pcDelimiter) = "".
             
            return trim(replace(pcList,
                                 // avoid error 984 replace does not handle non zero length param  
                                subst("&1&1",if pcDelimiter = "" then StringConstant:SPACE else pcDelimiter),
                                pcDelimiter  
                                ),
                                pcDelimiter).
        end.   
        else 
            undo, throw new OutOfBoundsError(pientry).
             
    end method.
    
    method static character RemoveEntryFromList(pcEntry as character, pcList as character ):
        return RemoveEntryFromList(pcentry, pcList,","). 
    end method.
    method static character RemoveEntryFromList(pcEntry as character, pcList as character, pcDelimiter as character ):
        define variable iLookup as integer no-undo.
        iLookup = lookup(pcEntry,pcList,pcDelimiter).
        if iLookup > 0 then 
        do:            
            return RemoveEntryFromList(iLookup, pcList,pcDelimiter). 
        end.
        else 
            return pcList.
                
    end method.
    
    method public static character AppendEntry(pcString as character,pcAppend as character): 
        return AppendEntry(pcString,pcAppend,",").
    end method.    
      
    method public static character AppendEntry(pcString as character,piAppend as integer): 
        return AppendEntry(pcString,string(piAppend)). 
    end method.
    
    method public static character AppendEntry(pcString as character,pcAppend as character,pcdlm as character): 
        
        if pcString = ? then pcString = "".
        if pcAppend = ? then pcAppend = "".
        
        return trim(subst("&1&3&2",pcString,pcAppend,pcDlm),pcdlm).
    end method.    
    
          
    // convert a string date to a strign with specified date format    
    method static public character StringDateFromIsoDate(pcDate as character):
        return string(DateUtil:DateFromIsoDate(pcDate)).
/*                                                                                              */
/*        define variable i as integer no-undo.                                                 */
/*        define variable cMsgTemplate as character init "'&1' is not a valid iso-date" no-undo.*/
/*        if substr(pcdate,5,1) <> "-"                                                          */
/*        or substr(pcdate,8,1) <> "-"                                                          */
/*        or length(pcdate) <> 10 then                                                          */
/*             undo, throw new IllegalArgumentError(subst(cMsgTemplate,pcDate)).                */
/*                                                                                              */
/*        return StringDateFromOtherFormat(pcDate,"ymd").                                       */
/*        catch e as Progress.Lang.Error :                                                      */
/*            if e:GetMessageNum(1) = 85 then                                                   */
/*                 undo, throw new IllegalArgumentError(subst(cMsgTemplate,pcDate)).            */
/*            undo, throw e.                                                                    */
/*        end catch.                                                                            */
    end method.
    
    // convert a string date to a strign with specified date format    
    method static public character StringDateFromOtherFormat(pcDate as character, pcDateFormat as char):
        return string(DateUtil:DateFromOtherFormat(pcDate, pcDateFormat)).
        /*
        define variable cDateFormat as character no-undo.
        define variable dDate as date no-undo.
        define variable cDateOut as character no-undo.
        if pcDateFormat <> session:date-format then
        do on error undo, throw: 
            cDateFormat = session:date-format.
            session:date-format = pcDateFormat.
            dDate = date(pcDate).
            session:date-format = cDateFormat.
            cDateOut = string(dDate).
            return cDateOut.
            finally:
                if session:date-format <> cDateFormat then
                    session:date-format = cDateFormat.
            end.    
        end.
        else 
            return pcDate.
         */   
    end method.
    
    method static public character UpperCaseFirst(pcData as character):
        return caps(substr(pcData,1,1)) + substr(pcData,2).
    end.   
    
    method static public character LowerCaseFirst(pcData as character):
        return lc(substr(pcData,1,1)) + substr(pcData,2).
    end.   
    
    method static public character LowerCaseSentence(pcString as character):
        return LowerCaseSentence(pcString,"").       
    end.   
    
    method static public character LowerCaseSentence(pcString as character, pcUppercaseFirstWords as character):
        define variable i as integer no-undo.
        define variable cWord as character no-undo.
        define variable cNewWord as character no-undo.
        define variable cNewString as character no-undo.
        define variable lUppernext as logical no-undo.
        
        cNewString = pcString.
        lUpperNext = true.
        do i = 1 to num-entries(cNewString,""):
            cWord = entry(i,cNewString,"").
            if cWord <> "" then
            do:
                if lUpperNext or lookup(cWord,pcUppercaseFirstWords) > 0 then
                do: 
                    cNewWord = UpperCaseFirst(lc(cWord)).
                    lUpperNext = false.
                end.
                else 
                    cNewWord = lc(cWord).
                    
                // if ends with period or stansalone period ippercase first char in next word
                if substr(cNewWord,length(cNewWord),1) = "." then 
                    lUppernext = true.
                        
                entry(i,cNewString,"") = cNewWord.
            end.    
        end.        
        return cNewString.
        
    end.   
    
    method static public logical WordHasNumber(pcWord as character):
        define variable i as integer no-undo.
        if index(pcWord,"") > 0 then
            undo, throw new IllegalArgumentError(subst("Word '&1' has space(s)",pcWord)).
        do i = 1 to 9:
           if index(pcWord,string(i)) > 0 then
               return true.
        end.  
        return false.  
    end method.
    
    method static public character TitleFormat(pcData as character):
        define variable cNewName as character no-undo.
        define variable i as integer no-undo.
        define variable cWord as character no-undo.
        do i = 1 to num-entries(pcData," ").
            cword = entry(i,pcData," ").
            if cword > "" then
            do: 
                if (length(cWord) > 2 
                    or 
                    (length(cWord) <=2 and compare(caps(cWord),"=",cWord,"case-sensitive") = false)
                    ) 
                and WordHasNumber(cWord) = false  then
                    cWord = UpperCaseFirst(lc(cWord)).
                        
                cNewName = AppendEntry(cNewName,cWord," ").
            end.               
        end.
        return cNewName.
    end.
    
    method static public logical IsInteger(pcData as character):
        integer(pcData).
        return true.
        catch e as Progress.Lang.Error :
            return false.    
        end catch. 
    end.   
    
    method static public logical IsBlank(pcData as character):
        return length(trim(pcData)) = 0 or length(trim(pcData)) = ?.
    end.   
    
    method static public character CamelToWords(pcData as character):
        define variable cChar as character no-undo.
        define variable i as integer no-undo.
        define variable cWords as character no-undo.
        define variable lPrevCap as logical no-undo.
        
        do i = 1 to length(pcData):
           cChar = substr(pcData,i,1).
           if i = 1 then
           do: 
               cWords = caps(cChar).
               lPrevCap = true.
           end.
           else do:
               if lPrevCap = false and compare(cChar,"=",caps(cChar),"case-sensitive") then
               do:
                   cWords = cWords + " " + cChar.
                   lPrevCap = true.
               end.
               else do:
                   cWords = cWords + cChar.
                   lPrevCap = false.
               end.            
           end.
        end.    
        return cWords.
    end.
    
    method static public character UnderscoreToCamelCase(pcData as character):
        define variable cChar as character no-undo.
        define variable i as integer no-undo.
        define variable cWords as character no-undo.
        define variable lNextCap as logical no-undo.
        
        do i = 1 to length(pcData):
           cChar = substr(pcData,i,1).
           if lNextCap then
           do: 
               cWords = cWords + caps(cChar).
               lNextCap = false.
           end.
           else do:
               if cChar = "_" then 
                   lNextCap = true.
               else do: 
                   cWords = cWords + cChar.
               end.            
           end.
        end.    
        return cWords.
    end.
    
    
    method public static character StripBadChars ( input pcString as character ):
        assign
            pcString = replace(pcString, "#", "num")
            pcString = replace(pcString, "&", "and")
            pcString = replace(pcString, "%", "pct")
            pcString = replace(pcString, "$", "amt")
            pcString = replace(pcString, "-", "")
            .
        return pcString.
    end method. /* stripBadChars */
     
    // Ignoree whitespace differneces (but keep single spaces)
    method public static logical EqualsIgnoreWhiteSpace(pcString1 as character, pcString2 as character): 
        do while index("  ",pcString1) <> 0:
            pcstring1 = replace(pcString1,"  "," "). 
        end.  
        do while index("  ",pcString2) <> 0:
            pcstring2 = replace(pcString2,"  "," "). 
        end.  
        return pcString1 = pcString2. 
    end method. 
   
    /*------------------------------------------------------------------------------
     Purpose: compare two CLOB values
       Notes: The core does not currently support compare of CLOB s 
              Copied from adm2/dataquery.p 
    ------------------------------------------------------------------------------*/ 
    method public static logical CompareClobValues( 
        phColumn1  as handle,
        pcOperator as char,
        phcolumn2  as handle,
        pcStrength as char
    ):
   
        define variable cLong1    as longchar no-undo.
        define variable cLong2    as longchar no-undo.
        define variable lUnknown1 as logical  no-undo.
        define variable lUnknown2 as logical  no-undo.
        define variable lEqual    as logical  no-undo.
        define variable lCompare  as logical  no-undo.

        assign
            lEqual    = can-do('=,EQ':U,pcOperator)
            lUnknown1 = (phColumn1:buffer-value = ?)
            lUnknown2 = (phColumn2:buffer-value = ?).

        if lUnknown1 and lUnknown2 then
            lCompare = lEqual.
        else if lUnknown1 or lUnknown2 then
            lCompare = not lEqual.
        else if length(phColumn1:buffer-value) <> LENGTH(phColumn2:buffer-value) then
            lCompare = not lEqual.
        else 
        do:
            copy-lob from phColumn1:buffer-value to cLong1.
            copy-lob from phColumn2:buffer-value to cLong2.
            lCompare = compare(cLong1,pcOperator,cLong2,pcStrength).
        end.

        return lCompare.
    end method.
    
    method static public character extent GetWords(pcString as character):
        define variable i             as integer no-undo.
        define variable iWord         as integer no-undo.
        define variable iStart        as integer no-undo.
        define variable cWord         as character no-undo.
        define variable cChr          as character no-undo.
        define variable cMaskedString as character no-undo.
        define variable cTempArray    as character no-undo extent.
        define variable cWords        as character no-undo extent.
        extent(cTempArray) = num-entries(pcString,"").
        cMaskedString = MaskQuotes(pcString, "@") + " ".
        iStart = 1.
        do i = 1 to length(cMaskedString):
           cChr = substr(cMaskedString,i,1).
           if cChr = "" then
           do: 
               if cWord <> "" then
               do:
                   iWord = iWord + 1.
                   cTempArray[iWord] = cWord.
                   iStart = 0.   
                   cWord = "".
               end.
           end. 
           else do:
               cWord = cWord + substr(pcString,i,1).
           end.     
        end.
        extent(cWords) = iWord.
        do i = 1 to iWord:
            cWords[i] = cTempArray[i].    
        end.
        return cwords. 
    end method.     
    
end class.