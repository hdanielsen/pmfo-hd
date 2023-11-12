 
 /*------------------------------------------------------------------------
    File        : ClientRequest
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Sun Mar 31 16:33:22 EDT 2019
    Notes       : The properties that expose url properties do NOT return 
                  default values  for poperties like skip, top and count 
                  It may be necessary to know if the prop was passed 
                  The caller must treat ? as default. 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using OpenEdge.Core.Assert from propath.
using OpenEdge.Core.Collections.IStringStringMap from propath.
using Pmfo.Core.BusinessLogic.RequestTypeEnum from propath.
using Pmfo.Core.Service.ClientRequest from propath.
using Pmfo.Core.Service.IClientCatalogRequest from propath.
using Pmfo.Util.Array from propath.
using Pmfo.Core.Error.BadRequestError from propath.

class Pmfo.Core.Service.ClientCatalogRequest inherits ClientRequest implements IClientCatalogRequest: 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    
    define public property SelectProp        as character init "select"   no-undo get.  
    
    define public property Properties        as char extent no-undo get. protected set.
       
    define public property Select         as character    no-undo 
        get():
            return string(UriQuery:Get(SelectProp)).
        end.     
    
    define protected property ValidSelectValues  as character   no-undo
        get:
            if ValidSelectValues = "" then
               ValidSelectValues = GetValidSelectValues().
            return ValidSelectValues.
        end.     
        protected set.
    
    constructor public ClientCatalogRequest ( pMethod as character, pServiceURI as character, pQuery as IStringStringMap ):
        super (pMethod, pServiceURI, pQuery).
        Assert:isTrue(pMethod = "GET" or pMethod = "HEAD","Method is GET or HEAD").
        Properties = GetProperties ().
    end constructor.
    
    method protected override character GetUriQueryErrorText(pRequestType as RequestTypeEnum):
        return "catalog request".
    end method.  
    
    method public void AssertUriQuery():
        if extent(Collections) = 1 then do:
            if UriQuery:Size > 0 then
                undo, throw new BadRequestError("URI Query not allowed on /catalog request with no resource segment").
        end.    
        AssertURIQuery(UriQuery,GetValidProperties(?),GetUriQueryErrorText(?)).
    end method.
    
    method protected character extent GetProperties ():
        return Array:ToArray(SelectProp).
    end method. 
    
    method protected character GetValidSelectValues ():
        return "name,path,type,key,textField,links,methods,resources,functions,actions,properties,deactivatedMethods,foreignKeyHidden,foreignKey,parentJoin".
    end method. 
    
    method protected override character extent GetValidProperties(pRequestType as RequestTypeEnum):
        return Properties. 
    end method. 
    
    method public void AssertSelect():
        define variable cSelect as character no-undo.
        define variable i as integer no-undo.
        cSelect = this-object:Select.
        do i = 1 to num-entries(cSelect):
           if lookup(entry(i,cSelect),ValidSelectValues) = 0 then
           do: 
               undo, throw new BadRequestError(subst("&1=&2","select",entry(i,cSelect))).
           end.    
        end.
    end method.     
    
end class.