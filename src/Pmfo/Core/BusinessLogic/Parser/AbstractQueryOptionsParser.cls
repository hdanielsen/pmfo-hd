/*------------------------------------------------------------------------
    File        : AbstractQueryOptionsParser
    Purpose     : Common super class for parsing of query options that has comples syntax and also supports a filter as a sub option 
    Description : 
    Author(s)   : hdaniels
    Created     : Mar 7 2023 
    Notes       : ABL is not very well suited for syntax parsing. The logic here is utilizing old existing code in StringUtil that 
                  can hide text inside quotes so that commas and slashes and such inside qiotes can be temporarily replaced to allow  
                  parsing of syntax outside of quotes to be parsed with simople ABL constructs like num-entries and entry
                - TODO? replace by real syntax parsing?     
                - TODO? Currently implemented as an abstract class - may be bette implemented as util/component/function 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.StringUtil from propath.

class Pmfo.Core.BusinessLogic.Parser.AbstractQueryOptionsParser abstract:
    define protected property IsResolved    as logical no-undo get. set.
    define protected property URIQuery      as IStringStringMap no-undo get. set.
    define protected property PropertyName  as character no-undo get. set.
    define protected property mFlatValue    as character no-undo get. set.


   /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    constructor public AbstractQueryOptionsParser (pPropertyName as character, pUriQuery as IStringStringMap):
        super ().
        URIQuery     = pURiQuery.
        PropertyName = pPropertyName.
    end constructor.

    method public void ResolveProperties():
        define variable cPropertyValue as character no-undo.
        if IsResolved = false then
        do:   
            cPropertyValue = string(UriQuery:Get(PropertyName)).
            if index(cPropertyValue,"(") > 0 then
            do:
                ParseExpression(cPropertyValue).               
            end.
            else 
                mFlatValue = cPropertyValue.           
        end.

        IsResolved = true.     
    end method.

    method protected character ReplaceCharsInParenthesis(pcExpression as character).
        define variable cExtract as character no-undo. 
        define variable cMaskedExpression as character no-undo.
        define variable cTrimExtract as character no-undo.
        define variable cString as character no-undo. 
        define variable iParenthesis as integer no-undo.        do while true: 
            cExtract = StringUtil:ExtractParenthesis(cString, output iParenthesis).
            if cExtract > "" then
            do: 
                // temprarily replace parentesis and commas inside of parenthesis  for the same reason as above
                cTrimExtract  = substr(cExtract,2,length(cExtract) - 2). 
                cTrimExtract = replace(cTrimExtract,"(","<").
                cTrimExtract = replace(cTrimExtract,")",">").
                cTrimExtract = replace(cTrimExtract,",","|").
                cMaskedExpression = cMaskedExpression + substr(cString,1,iParenthesis - 1 ) + "(" + cTrimExtract + ")".

                cString = substr(cString,iParenthesis + length(cExtract) ).
            end. 
            else do:
                cMaskedExpression = cMaskedExpression + cString.
                leave.
            end.

        end.    
        return cMaskedExpression.
    end method.


    method protected void ParseExpression(pcExpression as character). 

        define variable cReplaceLeft  as character no-undo. 
        define variable cReplaceRight as character no-undo. 
        define variable cReplaceComma as character no-undo. 

        define variable i as integer no-undo.
        define variable cProcessValue as character no-undo.
        define variable cExtract as character no-undo.
        define variable cString as character no-undo.
        define variable iParenthesis as integer no-undo.
        define variable cTrimExtract as character no-undo.
        define variable cExpandentry as character no-undo.
        define variable cExpandEntity as character no-undo.
        define variable cExpandExpression as character no-undo.
        define variable cHideChars as character no-undo.
        define variable cReplaceHideChars as character no-undo.

        do i = 1 to 4:
           if index(pcExpression,chr(i)) > 0 then  
               undo, throw new IllegalArgumentError(subst("character CHR(&1) found in Expand '&1'",i,pcExpression)).
        end.  
        // temprarily replace parenthesis and commas inside quotes so that we can parse parenthesises and commas for the option syntax  
        cHideChars = "(),".
        cReplaceHideChars = subst("&1&2&3",chr(1),chr(2),chr(3)).

        cString = StringUtil:MaskCharInQuotes(pcExpression,cHideChars,cReplaceHideChars).

        cProcessValue  = ReplaceCharsInParenthesis(cString).

        // loop through expand (comma separator) and then use left parethesis as entry param for the found entry to find the ines that have ab expression       
        do i = 1 to num-entries(cProcessValue):
            cExpandentry = entry(i,cProcessValue).

            /** Too late to check this as there already is bad UI code 
            if cExpandentry = "" then
                undo, throw new IllegalArgumentError(subst("Expand '&1' has a leading, ending or double comma",pcExpression)).
            **/

            if cExpandentry > "" then
            do: 
                if num-entries(cExpandentry,"(") > 1 then 
                do:
                    cExpandEntity     = entry(1,cExpandentry,"(").
                    // remove right parenthesis 
                    cExpandExpression = right-trim(entry(2,cExpandentry,"("),")").

                    // replace the parenthesis and commas that were temporarily removed for parsing above 
                    cExpandExpression = replace(cExpandExpression,"<","(").
                    cExpandExpression = replace(cExpandExpression,">",")").
                    cExpandExpression = replace(cExpandExpression,"|",",").

                    // also set back parenthesis and commas inside quotes as replaced on top of loop   
                    cExpandExpression = StringUtil:MaskCharInQuotes(cExpandExpression,cReplaceHideChars, cHideChars).
                    ExtractOptions(cExpandEntity,cExpandExpression).
                end.
                else 
                do:
                    cExpandEntity = cExpandEntry.
                    cExpandExpression = "".  
                end.     

                if lookup(cExpandEntity,mFlatValue) > 0 then 
                     undo, throw new IllegalArgumentError(subst("Entry '&1' in expand '&2' cannot be specifed more than once",cExpandentity,pcExpression)).

                mFlatValue = left-trim(subst("&1,&2",mFlatValue,cExpandEntity),",").
            end.

       end.     
   end method.    

   method protected void  ExtractOptions(pcExpand as character,pcExpression as character):
        define variable i              as integer no-undo.
        define variable cSubExpression as character no-undo. 
        define variable cWork          as character no-undo.
        define variable cOption        as character no-undo.
        define variable cValue         as character no-undo.

        if index(pcExpression,"filter") > 0 then 
        do:
            // if there is a filter temporarily replace  semicolons and equals insoide of quotes to allow parsing with these as entries outside commas
            cWork = StringUtil:MaskCharInQuotes(pcExpression, ";=",chr(1) + chr(2)).
        end.
        else 
            cWork = pcExpression.

        do i = 1 to num-entries(cWork,";").
            cSubExpression = entry(i,cWork,";").
            if num-entries(cSubExpression,"=") = 1 then 
                undo, throw new BadRequestError(subst("The expand entry '&1(&2)' is missing a query option and equals sign before the data",pcExpand,pcExpression)).
            if num-entries(cSubExpression,"=") > 2 then 
            do:

                undo, throw new BadRequestError(subst("The expand entry '&1(&2)' has invalid format. Multiple equals sign found. This could happen if the option=value pairs in the parenthesis are comma separated instead of semi-colon separated.",pcExpand,pcExpression)).
            end.
            cOption = trim(entry(1,cSubExpression,"=")).
            cValue  = trim(entry(2,cSubExpression,"=")).

            if cOption = "filter" then 
                // put back semicolons and  equals
                cValue = StringUtil:MaskCharInQuotes(cValue, chr(1) + chr(2),";=").
            ProcessExpandOption(pcExpand,cOption,cValue).
        end. 
    end method. 

    method protected abstract void ProcessExpandOption(pcExpand as character,pcOption as character, input pcValue as character).



end class.