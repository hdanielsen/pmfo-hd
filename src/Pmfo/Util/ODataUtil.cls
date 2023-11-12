 
 /*------------------------------------------------------------------------
    File        : ODataUtil
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Wed Oct 25 09:25:47 EDT 2023
    Notes       : 
  ----------------------------------------------------------------------*/

using OpenEdge.Core.Collections.ICollection from propath.

using Pmfo.Util.Array from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.StringUtil from propath.


block-level on error undo, throw.


class Pmfo.Util.ODataUtil: 
    
    method public static void GetCollectionProperties(pPath as character, output pCollections as character extent, output pKeyWheres as character extent):
        define variable i                 as integer no-undo.
        define variable iLeftParenthesis  as integer no-undo.
        define variable iLeftParenthesis2 as integer no-undo.
        define variable iRightParenthesis as integer no-undo.
        define variable cUriSegments      as character extent no-undo.
       
        define variable cReplaceQuote     as character no-undo.
        
        cUriSegments = Array:Split(trim(pPath,"/"),"/",true). // true ignore delimiter in quotes
        
        extent(pCollections) = extent(cUriSegments).
        extent(pKeyWheres)  = extent(cUriSegments).
        do i = 1 to extent(cUriSegments):
            iLeftParenthesis= index(cUriSegments[i],"(").
            if iLeftParenthesis > 0  then 
            do:
               iRightParenthesis = index(cUriSegments[i],")",iLeftParenthesis). 
               if iRightParenthesis = 0 then 
                   undo, throw new BadRequestError(subst("The URI segment '&1' is missing a rigth parenthesis",cUriSegments[i])) .
              
               if iRightParenthesis lt length(cUriSegments[i]) then
               do: 
                   // if a right parenthesis was found in a wrong spot maks quotes and check again
                   
                   // this is a bit paranoid - these should not come from the web/json 
                   if index(cUriSegments[i],chr(3)) = 0 then 
                       cReplaceQuote = chr(3).     
                   else if index(cUriSegments[i],chr(4)) = 0 then 
                       cReplaceQuote = chr(4).     
                   else 
                       undo, throw new UnsupportedOperationError(subst("The URI segment '&1' has unsupported characters",cUriSegments[i])). 
                   
                   iRightParenthesis = index(StringUtil:MaskQuotes(cUriSegments[i], cReplaceQuote),")",iLeftParenthesis).
                   // if still a problem throw error
                   if iRightParenthesis lt length(cUriSegments[i]) then 
                   do:
                       undo, throw new BadRequestError(subst("The URI segment '&1' is invalid. The parenthesis must be at the end of the segment.",cUriSegments[i])) .
                   end.
               end. 
               iLeftParenthesis2  = index(cUriSegments[i],"(",iLeftParenthesis + 1).
               if iLeftParenthesis2 > 0 and iLeftParenthesis2 lt iRightParenthesis then 
               do: 
                   // if a left  parenthesis was found in a wrong spot maks quotes and check again
                   
                   // this is a bit paranoid - these should not come from the web/json 
                   if index(cUriSegments[i],chr(3)) = 0 then 
                       cReplaceQuote = chr(3).     
                   else if index(cUriSegments[i],chr(4)) = 0 then 
                       cReplaceQuote = chr(4).     
                   else 
                       undo, throw new UnsupportedOperationError(subst("The URI segment '&1' has unsupported characters",
                                                                        cUriSegments[i]
                                                                        )
                                                                     ). 
                   iLeftParenthesis2 = index(StringUtil:MaskQuotes(cUriSegments[i], cReplaceQuote),"(",iLeftParenthesis + 1).
                   // if still a problem thow error
                   if iLeftParenthesis2 > 0 and iLeftParenthesis2 lt iRightParenthesis then
                   do: 
                       undo, throw new BadRequestError(subst("The URI segment for resource '&1' is invalid. A left parenthesis was found in position &2 inside the parenthesized key '(&3)'.",
                                                             substr(cUriSegments[i],1,iLeftParenthesis - 1),
                                                             iLeftParenthesis2 - iLeftParenthesis,
                                                             substr(cUriSegments[i],iLeftParenthesis + 1,length(cUriSegments[i]) - (iLeftParenthesis + 1))
                                                             )) .
                   end.   
               end.
               
               pKeyWheres[i] = substr(cUriSegments[i],iLeftParenthesis + 1,length(cUriSegments[i]) - (iLeftParenthesis + 1)).
               
               // TODO handle quotes in quotes 
               // note that double qiotes are valid for blank string in actions,functions and keys 
               // so this caues errors : KeyWheres[i] = replace(KeyWheres[i],"''","~~'").
               // this is only necessary when we are inside a quote 
               pCollections[i] = substr(cUriSegments[i],1,iLeftParenthesis - 1).
               
            end. 
            else   
               pCollections[i] = cUriSegments[i].
        end.
    end method. 
end class.