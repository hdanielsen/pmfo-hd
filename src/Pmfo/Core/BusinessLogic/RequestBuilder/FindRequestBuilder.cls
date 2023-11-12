 
 /*------------------------------------------------------------------------
    File        : FindRequestBuilder
    Purpose     : Find only top table in dataset 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Apr 18 10:47:38 EDT 2021
    Notes       : Intended for find of nodes with no body during POST 
                  where POST specific child tables may have been added                     
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using OpenEdge.Core.Collections.ICollection from propath.
using Pmfo.Core.BusinessLogic.RequestBuilder.QueryRequestBuilder from propath.
using Pmfo.Core.Service.IClientGetRequest from propath.

class Pmfo.Core.BusinessLogic.RequestBuilder.FindRequestBuilder inherits QueryRequestBuilder: 
    constructor public FindRequestBuilder (  ):
        super ().
    end constructor.
    
    method protected override void AddTableChildrenToRequestTree( prequests as ICollection, pClientRequest as IClientGetRequest, phBuffer as handle, pcRelativePath as character,plSkipRepos as logical ):
        // no children for find       
    end method.
    
end class.