 
 /*------------------------------------------------------------------------
    File        : ServiceEntityCatalogBuilder
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Mon May 27 10:49:29 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Progress.Json.ObjectModel.JsonObject from propath.
using Progress.Reflect.Property from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.Catalog.ICatalogRequest from propath.
using Pmfo.Core.Catalog.IServiceEntityCatalogBuilder from propath.
using Pmfo.Core.Catalog.ResourceCatalogBuilder from propath.
using Pmfo.Core.Error.IllegalArgumentError from propath.
using Pmfo.Util.Array from propath.

class Pmfo.Core.Catalog.ServiceEntityCatalogBuilder inherits ResourceCatalogBuilder implements IServiceEntityCatalogBuilder: 

   method final public override JsonObject BuildCatalog(pRequest as ICatalogRequest, pEntity as IEntity):
        Request = pRequest.
        if type-of(pEntity,IServiceEntity) then
        do:
            return BuildCatalog(cast(pEntity,IServiceEntity)).  
        end.
        else 
            undo, throw new IllegalArgumentError(subst("Illegal type '&1'. Must be IBusinessEntity.",pEntity:GetClass():TypeName)).
    end method.
    
   method protected JsonObject BuildCatalog(pEntity as IServiceEntity):
       define variable oProperties as Property extent no-undo.
       define variable json as JsonObject no-undo.
       define variable jsonParam as JsonObject no-undo.
       define variable cPath as character no-undo.
       define variable cType as character no-undo.
       
       json = new JsonObject().
       json:Add("name",pEntity:Resource).
       jsonParam = GetParameters(pEntity).
       if valid-object(jsonParam) then 
       do:
           cType = if pEntity:ReadOnly then "function" else "body".
       end.    
       else 
           cType = if pEntity:ReadOnly then "object" else "uri".
           
       json:Add("type",cType).
       cPath = "/" + pEntity:Resource.
       if valid-object(jsonParam) then do:
           json:Add("parameters",GetParameters(pEntity)).
           json:Add("pathTemplate",GetParameterPathTemplate(jsonParam,cPath)).
       end.
       else AddPath(json,cPath).
       
       json:Add("properties",GetProperties(pEntity,valid-object(jsonParam) = false)).
       json:Add("methods",GetJsonArray(MethodsAllowedList(pEntity))).
       return json. 
   end method.
   
   method protected JsonObject GetProperties(pEntity as IServiceEntity,plAddPath as logical):
       define variable json as JsonObject no-undo.
       define variable jsonProp as JsonObject no-undo.
       define variable i as integer no-undo.
       if extent(pEntity:Properties) > 0 then
       do:
           json = new JsonObject().
           
           do i = 1 to extent(pEntity:Properties):
               jsonProp = new JsonObject().
               json:Add(pEntity:Properties[i]:Name,jsonProp).
               jsonProp:Add("type", if pEntity:Properties[i] :extent = 0 then "object" else "array").
               jsonProp:Add("datatype",GetJsonType(pEntity:Properties[i]:DataType:toString())).
               if plAddPath then
                   AddPath(jsonProp,subst("/&1/&2",pEntity:Resource,pEntity:Properties[i]:Name)).
           end.
       end.
       return json.
   end method.
   
    // Cannot support both GET and PUT POST
    method protected override character extent MethodsAllowedList(pEntity as IEntity):
        define variable cAllowed      as character no-undo.
        define variable cDlm          as character no-undo.
        
        if not pEntity:ReadOnly then
        do:
            if pEntity:CanUpdate then
                cAllowed  = cAllowed + cDlm + "PUT".
            if pEntity:CanCreate then
                cAllowed  = cAllowed + cDlm + "POST".
            if pEntity:CanDelete then
                cAllowed  = cAllowed + cDlm + "DELETE".
            cDlm = ",".         
        end.   
        else
            cAllowed = "GET".
        
        return Array:Split(cAllowed).
    end method.
   
   method protected JsonObject GetParameters(pEntity as IServiceEntity):
       define variable json as JsonObject no-undo.
       define variable jsonProp as JsonObject no-undo.
       define variable i as integer no-undo.
       if extent(pEntity:Parameters) <> ? then
       do:
           json = new JsonObject().
           do i = 1 to extent(pEntity:Parameters):
               jsonProp = new JsonObject().
               json:Add(pEntity:Parameters[i]:Name,jsonProp).
               jsonProp:Add("type", "object").
               jsonProp:Add("datatype",GetJsonType(pEntity:Parameters[i]:DataType)).
           end.
       end.
       return json.
   end method.
     
       
end class.