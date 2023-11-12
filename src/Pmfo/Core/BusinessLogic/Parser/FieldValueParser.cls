 
 /*------------------------------------------------------------------------
    File        : FieldValueParser
    Purpose     : Abstract class to parse <field>=<value> expressions
                  returning a StringStringMap with the names and values
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Aug 28 00:13:21 EDT 2019
                   
    Notes         Tue Apr 5 2022 - no longer used for expressions with spaces and 'AND' join operator and 'EQ' queryooperator
                    (stopped working for this long before that - did nmot detect all spaces and ends of strings)
                  Use FieldExpressionParser for expression with spaces, 'AND' join operator and 'EQ' query operator
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using OpenEdge.Core.StringConstant from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.IllegalStateError from propath.
using Pmfo.Core.Error.BadRequestError from propath.

class Pmfo.Core.BusinessLogic.Parser.FieldValueParser  : 
    
    define public property ReplacedAliases as character no-undo get. protected set. 
    define public property ParametersWithAliases as character no-undo get. protected set. 
     
    method public IStringStringMap ParseExpressions (pcString as character) :
        return ParseExpressions(pcString, false). 
    end method.    
    
    method public IStringStringMap ParseExpressions (pcString as character,plKeepQuotes as logical) :
        define variable oMap as IStringStringMap no-undo.
        define variable cDummy as character extent no-undo.
        oMap = new StringStringMap(). 
        ParseExpressions(pcString,plKeepQuotes,oMap, ?, output cDummy, output cDummy).
        return oMap.
    end method.      
    
    method public IStringStringMap ParseExpressions (pcString as character,plKeepQuotes as logical, pAliasValues as IStringStringMap) :
        define variable oMap as IStringStringMap no-undo.
        define variable cDummy as character extent no-undo.
        oMap = new StringStringMap(). 
        ParseExpressions(pcString,plKeepQuotes,oMap, pAliasValues, output cDummy, output cDummy).
        return oMap.
    end method.  
    
    method public void ParseExpressions (pcString as character,plKeepQuotes as logical, output pcParams as character extent, output pcValues as character extent ) :
        ParseExpressions(pcString,plKeepQuotes,?, ?,output pcParams, output pcValues).
    end method.       
    
    method private void ParseExpressions (pcString as character,plKeepQuotes as logical, pMap as IStringStringMap, pAliasMap as IStringStringMap, output pcParams as character extent, output pcValues as character extent) :
        define variable iChr           as integer         no-undo.
        define variable iQuoteState    as integer         no-undo.
        define variable cQuote         as character       no-undo.         
        define variable cChr           as character       no-undo.   
        define variable iWord          as integer         no-undo.
        define variable cValue         as character       no-undo.
        define variable cToken         as character       no-undo.
        define variable cWords         as character       no-undo  extent 4.
        define variable cPrevChr       as character       no-undo.
        define variable iLength        as integer         no-undo.
        define variable iTestDec       as decimal         no-undo.
        define variable iTestNumber    as int64           no-undo.
        define variable iTestlog       as logical         no-undo.
        define variable oStringMap     as IStringStringMap no-undo.
        define variable iParam         as integer no-undo.
        define variable cAliasvalue    as character no-undo.
        
        assign 
            iWord      = 1
            cPrevChr   = ?
            pcString   = trim(pcString)
            iLength    = length(pcString).
            
        if valid-object(pMap) then  
            oStringMap = pMap.
        else do:
            if valid-object(pAliasMap) then
                undo, throw new IllegalOperationError("FieldValueParser ParseExpression optimized for non string values is called with valid Alias Map").
           
            // TODO - Testing with 70+ parameters in a separate method with the verty simple parsing needed for this did not imp[orve performanc 
            // It may still be worth while
            if index(pcString,"'") > 0 or index(pcString,'"') > 0 then 
            do:
                undo, throw new IllegalOperationError("FieldValueParser ParseExpression optimized for non string values is called with string values").
            end.
            extent(pcParams) = num-entries(pcString).
            extent(pcValues) = extent(pcParams).
        end.        
        do iChr = 1 to iLength :
            cChr = substring(pcString,iChr,1).
            case iQuoteState:
                when 0 then 
                do:
                    if cChr = '"':U or cChr = "'":U then
                    do: 
                        if iWord = 1  
                        or iWord = 4 
                        or cWords[3] <> StringConstant:SPACE then
                        do:
                            undo, throw new IllegalArgumentError(subst("Quote found in posisition &1 in expression '&2'",iChr,pcString)).
                        end.
                        
                        assign      
                            iQuoteState = 1
                            cQuote      = cChr
                            iWord       = 3.
                    end.        
                end.  
                when 1 then /* start quote is found */
                do:
                    if cChr = '~~':U then iQuoteState = 2. /* ignore next */
                    if cChr = cQuote then iQuoteState = 3. /* possible end */
                end.  
                when 2 then /* prev char was tilde */ 
                    iQuoteState = 1.
                when 3 then /* possible end quote was found */ 
                do:
                    /* if another quote then we're still in quoted string */
                    if cChr = cQuote then 
                    do:
                        iQuoteState = 1.
                        // if keep quotes then we must keep the extra escape quote until we unquote the value
                        if plKeepQuotes = false then cChr = "".
                    end.
                    else do: 
                        iQuoteState = 0.
                        iWord = 4.
                    end.        
                end.             
            end case.        
            if iQuoteState = 0 then 
            do:
               if cChr = "," then
               do:
                   if iWord = 3 or (iWord = 4 and cWords[4] = StringConstant:SPACE) then
                   do: 
                      cWords[4] = cChr.
                      iWord = 5.
                   end.
                   else do:
                       undo, throw new IllegalArgumentError(subst("Comma found in posisition &1 in expression '&2'",iChr,pcString)).
                   end.     
               end.    
               else if cChr = "=" then
               do:
                   if iWord = 1 or iWord = 2 then 
                   do:
                       cWords[2] = cChr.
                       iWord = 3.
                   end.
                   else 
                       undo, throw new IllegalArgumentError(subst("Operator '=' is not allowed in position &1 in expression '&2'",iChr,pcString)). 
               end.
               else if cChr = StringConstant:SPACE then
               do:
                    if  //cPrevChr <> StringConstant:SPACE 
                    // and 
                    cPrevChr <> ","
                    and cPrevChr <> "="
                    and cPrevChr <> "'"
                    and cPrevChr <> '"' then 
                    do: 
                       if cWords[2] = "=" then
                           undo, throw new IllegalArgumentError(subst("Double space is not allowed in position &1 after '=' in expression '&2'",iChr,pcString)). 
                       else do:    
                           cWords[1] = cWords[1] + cChr.
                       end.    
                    end.   
               end.
               else do:
                   if iQuoteState = 0 and (cPrevChr = '"':U or cPrevChr = "'":U) then
                       undo, throw new IllegalArgumentError(subst("Character '&1' is not allowed in position &2 in expression '&3'",cChr,iChr,pcString)).
                        
                   cWords[iWord] = cWords[iWord] + cChr.  
               end.
               
            end.
            else do:
               cWords[3] = cWords[3] + cChr.
            end.   
            /** for testing **/
/*            display iWord cChr cWords[1] label "1" cWords[2] label "2" cWords[3] label "3" cWords[4] label "4" iQuoteState.*/
/*            down.                                                                                                          */
            
            cPrevChr = Cchr.
            
            if iWord = 5 or (iChr = iLength and (iWord = 3 or (iWord = 4 and cChr = cQuote))) then 
            do:
                if iWord = 5 then
                do:
                    if iChr = ilength  then
                        undo, throw new IllegalArgumentError(subst("'&1' found in posisition &2 in expression '&3'",cChr,iChr,pcString)).  
                        
                    if /*cWords[4] <> "and" and*/ cWords[4] <> "," then 
                        undo, throw new IllegalArgumentError(subst("Found '&1' instead of delimiter",cWords[4])).  
                end.
                
                if /*cWords[2] <> "eq" and */ cWords[2] <> "=" then 
                    undo, throw new IllegalArgumentError(subst("Unsupported or invalid operator '&1'",cWords[2])).  
                
                if cwords[3] = "" then do:
                     undo, throw new IllegalArgumentError(subst('"&1&2" is missing a value in expression "&3"',cWords[1],cWords[2],pcString)).
                end.     
                if cWords[3] begins "'" or cWords[3] begins '"'  then
                do:
                    if substr(cWords[3],length(cWords[3]),1) <> substr(cWords[3],1,1) then  
                       undo, throw new IllegalArgumentError(subst('Missing end quote for value "&1" in expression "&2"',cWords[3],pcString)).
                    if plKeepQuotes = false then
                        cWords[3] = substr(cWords[3],2,length(cWords[3]) - 2).
                end.    
                /* unknown value - just quote it - works as expected in dynamic query 
                   @TODO we can fix the query parsing and logic to support unquoted */    
                else if cWords[3] = '?' then 
                    cWords[3] = ?. // // quoter(cWords[3]).
                    
                else if cWords[3] = 'null' then 
                do:
                    cWords[3] = ? .  
                end.
                else if cWords[3] begins "@" then
                do: 
                    if valid-object(pAliasMap) then 
                    do:
                        cAliasValue = pAliasMap:Get(cWords[3]).
                       
                        if cAliasValue = ? then 
                             undo, throw new BadRequestError(subst("No corresponding URL Query Parameter was provided for Parameter '&1' Alias '&2'",cWords[1],cWords[3])).  
                        // allow caller to track  TODO - needs clean up was added late  
                        ParametersWithAliases = ParametersWithAliases 
                                              + (if ReplacedAliases = "" then "" else ",")
                                              + cWords[1].
                        ReplacedAliases = ReplacedAliases
                                       + (if ReplacedAliases = "" then "" else ",")
                                       + cWords[3].
                        cWords[3] = cAliasValue.
                                        
                    end.
                    else
                        undo, throw new BadRequestError(subst("Parameter '&1' has alias value '&2', but no Alias were provided in the URL Query.",cWords[1],cWords[3])).  
                            
                end.    
                else do:
                    
                    assign
                        iTestDec = ?
                        iTestNumber = ?
                        iTestLog  = ?.
                    iTestDec    = decimal(cWords[3]) no-error. 
                    iTestNumber = int64(cWords[3]) no-error.
                    iTestlog    = logical(cWords[3]) no-error.
                    if iTestDec = ? and iTestNumber = ? and iTestLog = ? then
                        undo, throw new IllegalArgumentError(subst("Value &1 is not a valid value unless it is quoted",cWords[3])).  
                    
                end.    
                
                if valid-object(oStringMap) then
                do:
                    if oStringMap:ContainsKey(cWords[1]) then
                        undo, throw new IllegalArgumentError(subst('There are more than one value for "&1" in expression "&2"',cWords[1],pcString)).  
                    oStringMap:put(cWords[1],cWords[3]).
                end.
                else do:
                    iParam = iParam + 1.
                    pcParams[iParam] = cWords[1].
                    pcValues[iParam] = cWords[3].
                end.    
                cQuote  =  "". 
                cWords[1] = "".
                cWords[2] = "".
                cWords[3] = "".
                cWords[4] = "".
                iWord = 1.
                
            end.    
        end.
       
        if iWord <> 1  or cWords[1] <> "" then 
        do:
            undo, throw new IllegalArgumentError(subst("'&1' is not a valid key expression",pcString)).  
        end.
         
     end method.
end class.