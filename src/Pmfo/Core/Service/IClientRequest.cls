
/*------------------------------------------------------------------------
    File        : IClientRequest
    Purpose     : Builds GetDataRequest from dataset handle using public names
    Syntax      : 
    Description : Defines a data request from public names   
    Author(s)   : hdaniels
    Created     : Sun Mar 31 15:46:35 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Ccs.BusinessLogic.IGetDataRequest from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Common.IMethod from propath.
using OpenEdge.Core.Collections.ICollection from propath.

interface Pmfo.Core.Service.IClientRequest:  
    define public property ServiceURI             as character no-undo get.  
    define public property Method                 as character no-undo get.  
    define public property Collections            as character extent no-undo get.  
    define public property KeyWheres              as character extent no-undo get. set.
    define public property UriQuery               as IStringStringMap no-undo get. 
     
    define public property MetaDataProp as character  no-undo get.  
    define public property ContextProperties as char extent no-undo get.  
    
    // set by DatasetRouter to tell the builder that 1st entry in 
    // the collection was passed as the 2nd and this was the first  
    define public property RedirectFrom            as character no-undo get. 
    
    // to be prepared by BE before building the request (not sure if this is the best place) 
    define public property ServiceMethod          as IMethod no-undo get. set.
    define public property Relations              as ICollection no-undo get. set. 
    
    define public property HasAlias               as logical no-undo get.  
    
    // Aliases can be used in place of literal values in entity keys, function parameters, or within a filter or orderby expression.
    // Aliases MUST start with an @ character. 
    define public property Aliases                as IStringStringMap no-undo get.
    // keep track of the ALiases that are used to allow throw error if any reference is not used   
    define public property UsedAliases            as character no-undo get. .
    
    /**
     Simnple redirect replaces collection 1 and 2 with a single redirect and moved all other entries up accordingly
     Typcially used for the same dsata as the first collection, but could be any collrctioon
     aslong as the first collection only is used for routing (no data and no use of resource name )
     ***/    
    method public void Redirect(pcRedirect as character).
    
    method public void AddUsedAliases(pcAliasNames as character).
    
    /**
    Redirect to a collection further doWn in the url - used when all prior collections only provides 
    context in the form of a name and no link or data is needed  
    ***/    
    method public void RedirectTo(piCollection as integer).  
    
    method public void AssertUriQuery(pRequestType as RequestTypeEnum). 
    
    method public void AssertUriQueryAliases().
end interface.