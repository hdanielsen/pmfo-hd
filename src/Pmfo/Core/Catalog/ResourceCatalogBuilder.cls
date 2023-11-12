 
 /*------------------------------------------------------------------------
    File        : ResourceCatalogBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon May 27 10:52:42 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/

using Progress.Lang.*.
using Pmfo.Core.Catalog.IResourceCatalogBuilder.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.Catalog.ICatalogRequest from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Util.Array from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using OpenEdge.Core.DataTypeEnum from propath.
using OpenEdge.Core.DataTypeHelper from propath.
using Pmfo.Util.JsonUtil from propath.

block-level on error undo, throw.

class Pmfo.Core.Catalog.ResourceCatalogBuilder implements IResourceCatalogBuilder abstract: 
    define protected property Request as ICatalogRequest get. set. 
   
    constructor public ResourceCatalogBuilder (  ):
        super ().
    end constructor.
    
    method public abstract JsonObject BuildCatalog( input pRequest as ICatalogRequest, input poEntity as IEntity ):
    
    method protected character GetFullPath(pcPath as char):
        if valid-object(Request) then
            return subst("&1&2",Request:Path,pcPath).
        else 
            return pcPath.
    end method.     
    
    method protected void AddPath(pJson  as JsonObject, pcObjectPath as char):
        pjson:Add("path",GetFullPath(pcObjectPath)).
    end method.    
    
    method protected character GetParameterPathTemplate(pParameterJson as jsonObject,pcPath as char):
           define variable cnames as character extent no-undo.
           define variable cParamExpression as character no-undo.
           define variable i as integer no-undo.
           define variable cDlm as character no-undo.
           cNames = pParameterJson:GetNames().
           do i = 1 to extent(cNames):
               cParamExpression = cParamExpression
                                + cDlm 
                                + subst("&1=@p&2",cnames[i],i).
               cDlm = ",".                 
           end.    
           return GetFullPath(subst("&1(&2)",pcPath,cParamExpression)).
    end method.
    
    method protected JsonArray GetJsonArray(pcValues as char extent):
        define variable jsonArray    as JsonArray  no-undo.
        define variable i                    as integer no-undo.
        
        jsonArray =  new JsonArray().
        if extent(pcValues) <> ? then 
        do:
             do i = 1 to extent(pcValues):
                 jsonArray:Add(pcValues[i]). 
             end.     
        end.
        return jsonArray.
    end method.    
    
      /**
    TODO - somewhat duplicated in CrudoperationHandler - spaces in delimiter 
    **/
    method protected character extent MethodsAllowedList(pEntity as IEntity):
        define variable cAllowed      as character no-undo.
        define variable cDlm          as character no-undo.
        cAllowed = "GET".
        if not pEntity:ReadOnly then
        do:
            cDlm = ",".
            cAllowed  = cAllowed + cDlm + "PUT".
            if pEntity:CanCreate then
                cAllowed  = cAllowed + cDlm + "POST".
            if pEntity:CanDelete then
                cAllowed  = cAllowed + cDlm + "DELETE".     
        end.   
        return Array:Split(cAllowed).
    end method.
    
    method protected character GetJsonType ( input pABLType as DataTypeEnum):
        return JsonUtil:GetJsonType(pAblType).
    end method.    
    
    method protected character GetJsonType ( input pcABLType as character ):
         return JsonUtil:GetJsonType(pcAblType).
    end method. /* getJsonType */
    
    
end class.