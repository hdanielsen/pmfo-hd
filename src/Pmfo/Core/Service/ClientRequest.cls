 
 /*------------------------------------------------------------------------
    File        : ClientRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 31 16:33:22 EDT 2019
    Notes       : Busness Entity Data or Update Request based on
                  public names 
                - Resource name, dataset serialize-name or table serialize-name
                  as exposed to the UI and used in REST URIs   
                - The request is newed without knowledge of the dataset or buffers
                  and passed to the BE that then uses it to identify the requested
                  dataset from the Collections.
                - Subclasses have geters that returns the request that is 
                  to be passed to the DataAccess based on a dataset parameter      
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Collections.ICollection from propath.
using OpenEdge.Core.Collections.IIterator from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using OpenEdge.Core.String from propath.
using OpenEdge.DataAdmin.Error.IllegalOperationError from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.BusinessLogic.ResponseMetaDataEnum from propath.
using Pmfo.Core.Common.IMethod from propath.
using Pmfo.Core.Error.BadRequestError from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Core.Service.IClientRequest from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Core.Error.OutOfBoundsError from propath.
using OpenEdge.Core.Collections.StringStringMap from propath.
using Pmfo.Util.StringUtil from propath.
using Pmfo.Core.Error.UnsupportedOperationError from propath.
using Pmfo.Util.ODataUtil from propath.

class Pmfo.Core.Service.ClientRequest abstract implements IClientRequest: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/ 
    define public property UriQuery     as IStringStringMap           no-undo 
        get. 
        protected set(pMap as IStringStringMap):
            if pMap = ? then 
                undo, throw new IllegalArgumentError("UriQuery cannot be null").
            DecodeUriQuery(pMap).  // see comment below    
            UriQuery = pmap.
        end.
    
    define public  property Method      as character no-undo get. protected set. 
        
    define public  property Collections as character extent no-undo get. protected set. 
    define public  property KeyWheres   as character extent no-undo get. set. 
    define public  property MetaDataProp as character init "metadata"    no-undo get.  
    
    define public property ContextProperties        as char extent no-undo get. protected set. 
    define public property ServiceURI  as character no-undo get. set. 
    
    // see interface
    define public property RedirectFrom           as character no-undo get. protected set.
    
    // to be prepared by BE before building the request (@TODO not sure if this is the best place) 
    define public property ServiceMethod          as IMethod no-undo get. set.
    
    define public property HasAlias               as logical no-undo get. protected set. 
      
    // Aliases can be used in place of literal values in entity keys, function parameters, or within a filter or orderby expression.
    // Aliases MUST start with an @ character. 
    define public property Aliases                as IStringStringMap no-undo 
        get():
            if not valid-object(Aliases) then 
                Aliases = new StringStringMap().
            return Aliases.    
        end.  
        protected set(pAliases as IStringStringMap):
            Aliases = pAliases.
            HasAlias = true.
        end set.    
        
    define public property UsedAliases            as character no-undo get. protected set.
    
        
    define public property Relations              as ICollection no-undo get. set.
       
    constructor public ClientRequest ( pMethod as character, pServiceURI as character , pQuery as IStringStringMap):
        super ().
        Method = pMethod.
        ServiceURI = pServiceURI.
        UriQuery   = pQuery.
        AssignCollectionProperties(pServiceURI).
    end constructor.
  
    define public property MetaDataEnum  as ResponseMetaDataEnum  no-undo 
        get():
            define variable cValue as character no-undo.
            if not UriQuery:ContainsKey(MetaDataProp) then
                return ResponseMetaDataEnum:None.
            else do:    
                cValue = UriQuery:Get(MetaDataProp).
                case cValue:
                    when "none" then
                        return ResponseMetaDataEnum:None.
                    when "minimal" then
                        return ResponseMetaDataEnum:Minimal.
                    when "full" then
                        return ResponseMetaDataEnum:Full.
                        
                    otherwise do on error undo, throw:
                        return  ResponseMetaDataEnum:GetEnum(cValue).
                        catch e as Progress.Lang.Error :
                            undo, throw new BadRequestError(subst("&1=&2",MetaDataProp,cValue) + ". The supported options are metdata=none (default),  metadata=full, metadata=minimal, metadata=links, metadata=navigationlinks and metadata=id ").
                        end. 
                    end.       
                end case.        
            end.
        end. 
     
    // subclasses override to decode uri query character values to support old web 
    // that replaces space with +.
    // The framework make sure that any value with + is added with spaces
    // to avoid any ambiguity   
    method protected void DecodeURIQuery(pURIQuery as IStringStringMap):
    end method.      
    
    method protected character extent GetValidProperties(pRequestType as RequestTypeEnum):
        define variable cEmptyArray as character extent no-undo. 
        return cEmptyArray.
    end method.     
    
    method protected character GetUriQueryErrorText(pRequestType as RequestTypeEnum):
        case pRequestType:
            when RequestTypeEnum:SingleRow then
                return "single entity".
            when RequestTypeEnum:SingleTopTable then
                return "collection".
            when RequestTypeEnum:MultiTable then
                return "resource".
        end.    
    end method.  
       
    method public void AssertUriQuery(pRequestType as RequestTypeEnum):
        AssertURIQuery(UriQuery,GetValidProperties(pRequestType),GetUriQueryErrorText(pRequestType)).
    end method.
    
    method protected void AssertRedirect():
        if extent(Collections) = ? then 
            undo, throw new IllegalOperationError("Redirect of request with no collections").
        
        if extent(Collections) = 1 then 
            undo, throw new IllegalOperationError("Redirect of request with one collection").
    end method.
    
    /**
     Simnple redirect replaces collection 1 and 2 with a single redirect
     Typcially used for the same dsata as the first collection, but could be any collrctioon
     aslong as the first collection only is used for routing (no data and no use of resource name )
     ***/    
    method public void Redirect(pcRedirect as character):
        define variable cNewCollections as character extent no-undo.
        define variable cNewKeyWheres   as character extent no-undo.
        
        define variable i as integer no-undo.
         
        AssertRedirect().
        
        if KeyWheres[1] > "" then  
            undo, throw new IllegalOperationError(subst("URI &1(&2)/&3 is invalid. Cannot specify key in link from '&1' to '&3'.",
                                                        Collections[1],
                                                        KeyWheres[1],
                                                        Collections[2]
                                                       )
                                                   ).
        extent(cNewCollections) = extent(Collections) - 1.
        extent(cNewKeyWheres) = extent(cNewCollections).
        
        RedirectFrom = Collections[1].
        cNewCollections[1] = pcRedirect.
        cNewKeyWheres[1] = KeyWheres[2].
        
        do i = 2 to extent(cNewCollections):
            cNewCollections[i] = Collections[i + 1].
            cNewKeyWheres[i] = KeyWheres[i + 1].
        end.    
        extent(Collections) = ?.
        extent(KeyWheres) = ?.
        Collections = cNewCollections.
        KeyWheres   = cNewKeyWheres.
    end method.
    
    // Redirect to a collection further doen in the url - used when all prior collections only provides 
    // context in the form of a name and no link or data is needed  
    method public void RedirectTo(piRedirect as integer).
        define variable cNewCollections as character extent no-undo.
        define variable cNewKeyWheres   as character extent no-undo.
      
        define variable i as integer no-undo.
        
        AssertRedirect().
        
        if piRedirect = 1 then 
            undo, throw new IllegalArgumentError("Redirect to Collection 1").
        
        if piRedirect < 2 then 
            undo, throw new IllegalArgumentError(subst("Redirect to Collection &1. Must be greater than 1",piRedirect)).
        
        if piRedirect gt extent(Collections) then  
            undo, throw new OutOfBoundsError(piRedirect).
        
        do i = 1 to piRedirect - 1:
            if KeyWheres[i] > "" then  
                undo, throw new IllegalOperationError(subst("URI &1(&2)/&3 is invalid. Cannot specify key in link from '&1' to '&3'.",
                                                            Collections[i],
                                                            KeyWheres[i],
                                                            Collections[i + 1]
                                                           )
                                                       ).
        end.                                           
        
        extent(cNewCollections) = extent(Collections) - (piRedirect - 1).
        extent(cNewKeyWheres)    = extent(cNewCollections).
        
        // if more than one down we cannot keep track of from
        if piRedirect = 2 then
           RedirectFrom = Collections[1].
           
        do i = 1 to extent(cNewCollections):
            cNewCollections[i] = Collections[i + (piRedirect - 1)].
            cNewKeyWheres[i] = KeyWheres[i + (piRedirect - 1)].
        end.    
        extent(Collections) = ?.
        extent(KeyWheres) = ?.
        Collections = cNewCollections.
        KeyWheres   = cNewKeyWheres.
    end method.  
    
    method protected void AssertUriQuery(pQueryMap as IStringStringMap,pValidProps as char extent,pcTypeOrMethodForMessage as char):
        define variable cList      as character no-undo.
        define variable oKeyValues as ICollection no-undo.
        define variable oIterator  as IIterator no-undo.
        define variable cKey       as character no-undo.
        
        if extent(pValidProps) <> ? then
        do:
            cList = Array:ToString(pValidProps).
             
            oKeyValues = pQueryMap:KeySet.
            oIterator  = oKeyValues:Iterator().
             
            do while oIterator:HasNext():
                cKey =  cast(oIterator:Next(), String):Value.
                if cKey = "" then 
                   undo, throw new BadRequestError("The URL has an empty query option. Please remove any '&&' from the URL that has no value.").
                if (cKey begins "@") then 
                    Aliases:Put(cKey,pQueryMap:Get(cKey)). // thse will be processed by the appropriate parser or f 
                else if lookup(cKey,cList) = 0 then 
                    undo, throw new BadRequestError(BadQueryOptionError(cKey,pcTypeOrMethodForMessage)).
                 
            end.
        end.
         
    end method.    
    
    method public void AssertUriQueryAliases():
        define variable oKeyValues as ICollection no-undo.
        define variable oIterator  as IIterator no-undo.
        define variable cKey       as character no-undo.
        
        oKeyValues = Aliases:KeySet.
        oIterator  = oKeyValues:Iterator().
        do while oIterator:HasNext():
            cKey =  cast(oIterator:Next(), String):Value.
            if lookup(cKey,UsedAliases) = 0 then
                undo, throw new BadRequestError(subst("'&1' is not a valid URL query option. Aliases are only valid when referenced elsewhere in the URL.",cKey)).
        end.
         
         
    end method.    
    
    method protected character BadQueryOptionError(pcOption as character, pcTypeOrMethodForMessage as character ):
        return subst("'&1' is not a valid URI query option for &2",pcOption,pcTypeOrMethodForMessage).
    end.     
    
    method protected void AssignCollectionProperties(pServiceURI as character):
        ODataUtil:GetCollectionProperties(pServiceURI,output Collections, output KeyWheres).
        /**
        define variable i                 as integer no-undo.
        define variable iLeftParenthesis  as integer no-undo.
        define variable iLeftParenthesis2 as integer no-undo.
        define variable iRightParenthesis as integer no-undo.
        define variable cUriSegments      as character extent no-undo.
       
        define variable cReplaceQuote     as character no-undo.
        
        cUriSegments = Array:Split(trim(pServiceURI,"/"),"/",true). // true ignore delimiter in quotes
        
        extent(Collections) = extent(cUriSegments).
        extent(KeyWheres)  = extent(cUriSegments).
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
               
               KeyWheres[i] = substr(cUriSegments[i],iLeftParenthesis + 1,length(cUriSegments[i]) - (iLeftParenthesis + 1)).
               
               // TODO handle quotes in quotes 
               // note that double qiotes are valid for blank string in actions,functions and keys 
               // so this caues errors : KeyWheres[i] = replace(KeyWheres[i],"''","~~'").
               // this is only necessary when we are inside a quote 
               Collections[i] = substr(cUriSegments[i],1,iLeftParenthesis - 1).
               
            end. 
            else   
               Collections[i] = cUriSegments[i].
        end.
        **/
    end method. 
    
    method protected void AssertCollections():
        define variable cCollectionList as character no-undo.
        define variable i as integer no-undo.
        cCollectionList = Array:ToString(Collections).
        do i = 1 to extent(Collections):
            if lookup(Collections[i],cCollectionList) <> i then
               undo, throw new BadRequestError(subst("Entity '&1' cannot be referenced twice in the URL",Collections[i])) .
             
        end.
    end method.    
        
    method public void AddUsedAliases(pcAliasNames as character):
        define variable i as integer no-undo.
        define variable cName as character no-undo.
        
        do i = 1 to num-entries(pcAliasNames):
            cName = entry(i,pcAliasNames). 
            if lookup(cName,UsedAliases) = 0 then 
                UsedAliases = UsedAliases
                            + (if UsedAliases = "" then "" else ",")
                            + cName. 
        end.              
    end method.        
    
    
end class.