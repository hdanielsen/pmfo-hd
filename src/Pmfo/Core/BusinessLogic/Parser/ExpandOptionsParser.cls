 
 /*------------------------------------------------------------------------
    File        : ExpandOptionsParser
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon Oct 04 23:37:55 EDT 2021
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Error.IllegalOperationError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.StringUtil from propath.

class Pmfo.Core.BusinessLogic.Parser.ExpandOptionsParser:
    define protected property ExpandIsResolved as logical no-undo get. set.
      
    define protected property URIQuery         as IStringStringMap no-undo get. set.
    
    define protected property ExpandProp        as character init "expand"   no-undo get. set. 
    
    define protected property mFlatExpand       as character        no-undo get. set. 
    define protected property ExpandSelects     as IStringStringMap no-undo get. set. 
    define protected property ExpandFilters     as IStringStringMap no-undo get. set. 
    define protected property ExpandSort        as IStringStringMap no-undo get. set. 
    define protected property mInnerJoins       as character no-undo get. set. 
      
    define public property Expand         as character    no-undo 
        get():
            if not ExpandIsResolved then 
                ResolveProperties().
                
            return mFlatExpand.
        end.   
    
    define public property InnerJoins     as character    no-undo 
        get():
            if not ExpandIsResolved then 
                ResolveProperties().
                
            return mInnerJoins.
        end.   
    
        /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public ExpandOptionsParser (pURiQuery as IStringStringMap, pExpandProp as character, pSelectProp as character ):
        super ().
        URIQuery = pURiQuery.
        ExpandProp = pExpandProp. 
       
    end constructor.

    method public character GetExpandFilter (pcExpand as character):
        if not ExpandIsResolved then 
            ResolveProperties().
        if valid-object(ExpandFilters) then 
            return string(ExpandFilters:Get(pcExpand)).
        else
            return "".           
    end method.
    
    method public character GetExpandSort (pcExpand as character):
        if not ExpandIsResolved then 
            ResolveProperties().
        if valid-object(ExpandSort) then 
            return string(ExpandSort:Get(pcExpand)).
        else
            return "".           
    end method.
    
    method public character GetExpandSelect (pcExpand as character):
        if not ExpandIsResolved then 
            ResolveProperties().
        if valid-object(ExpandSelects) then 
            return string(ExpandSelects:Get(pcExpand)). 
        else
            return "".           
    end method.
    
    method public void ResolveProperties():
        define variable cExpand as character no-undo.
        if ExpandIsResolved = false then
        do:   
            cExpand     = string(UriQuery:Get(ExpandProp)).
            if index(cExpand,"(") > 0 then
            do:
                ParseExpression(cExpand).               
            end.
            else 
                mFlatExpand = cExpand.           
        end.
     
        ExpandIsResolved = true.     
    end method.
    
    method protected void ParseExpression(pcExpression as character):
        define variable cReplaceLeft  as character no-undo. 
        define variable cReplaceRight as character no-undo. 
        define variable cReplaceComma as character no-undo. 
         
        define variable i as integer no-undo.
        define variable cExpand as character no-undo.
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
        // temprarily replace parenthesis and commas inside quotes so that we can parse parenthesises and commas for the expand syntax  
        cHideChars = "(),".
        cReplaceHideChars = subst("&1&2&3",chr(1),chr(2),chr(3)).
        
        cString = StringUtil:MaskCharInQuotes(pcExpression,cHideChars,cReplaceHideChars).
              
        do while true: 
            cExtract = StringUtil:ExtractParenthesis(cString, output iParenthesis).
            if cExtract > "" then
            do: 
                // temprarily replace parentesis and commas inside of parenthesis  for the same reason as above
                cTrimExtract  = substr(cExtract,2,length(cExtract) - 2). 
                cTrimExtract = replace(cTrimExtract,"(","<").
                cTrimExtract = replace(cTrimExtract,")",">").
                cTrimExtract = replace(cTrimExtract,",","|").
                cExpand = cExpand + substr(cString,1,iParenthesis - 1 ) + "(" + cTrimExtract + ")".
                
                cString = substr(cString,iParenthesis + length(cExtract) ).
            end. 
            else do:
                cExpand = cExpand + cString.
                leave.
            end.
                
        end.    
          
        // loop through expand (comma separator) and then use left parethesis as entry param for the found entry to find the ines that have ab expression       
        do i = 1 to num-entries(cExpand):
            cExpandentry = entry(i,cExpand).
            
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
                
                if lookup(cExpandEntity,mFlatExpand) > 0 then 
                     undo, throw new IllegalArgumentError(subst("Entry '&1' in expand '&2' cannot be specifed more than once",cExpandentity,pcExpression)).
                
                mFlatExpand = left-trim(subst("&1,&2",mFlatExpand,cExpandEntity),",").
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
    
    method protected void ProcessExpandOption(pcExpand as character,pcOption as character, input pcValue as character):
        define variable cQualifier as character no-undo.
        define variable i as integer no-undo.
        define variable lLogicalOption as logical no-undo.
        case pcOption:
            when "select" then 
            do:
                // select is never transformed to intenrla tables names 
                // but we only keep the last qualifer 
                // TODO - this is actually a problem since grand children  
                // may have same name as their second cousin 
                cQualifier =  entry(num-entries(pcExpand,"/"),pcExpand,"/").                                                                                                    
                do i = 1 to num-entries(pcValue):
                    if not valid-object(ExpandSelects) then 
                        ExpandSelects= new StringStringMap().
                     
                   ExpandSelects:put(pcExpand,pcValue).        
                end.                   
            end.    
            when "filter" then 
            do: 
                if not valid-object(ExpandFilters) then 
                    ExpandFilters = new StringStringMap().
                ExpandFilters:put(pcExpand,pcValue).        
            end.   
            when "orderBy" then 
            do: 
                if not valid-object(ExpandSort) then 
                    ExpandSort = new StringStringMap().
                ExpandSort:put(pcExpand,pcValue).        
            end.   
            when "innerJoin" then 
            do on error undo, throw: 
                lLogicalOption = logical(pcValue).
                // if false do nothing - it may be convenient to change this to false instead o f remving the entrie option in test or maybe if built from code 
                if lLogicalOption = true then
                    mInnerJoins = mInnerJoins 
                                + (if mInnerJoins = "" then "" else ",")
                                + pcExpand.
                catch e as Progress.Lang.Error :
                    if e:GetMessageNum(1) = 87 then
                        undo, throw new IllegalOperationError(subst("Invalid option '&1=&2' in Expand '&3' expression. The value is not a boolean.",pcOption,pcValue,pcExpand)).
                    
                    else 
                        undo, throw new IllegalOperationError(subst("Faileds to process option '&1=&2' in Expand expression'&3'",pcOption,pcValue,pcExpand),e).
                       
                end catch.
                                     
            end. 
            otherwise 
                undo, throw new UnsupportedOperationError(subst("Option '&1' in Expand '&2' expression is not a valid expand option",pcOption,pcExpand)). 
        end.      
    end method.    
 
    
     
      
end class.