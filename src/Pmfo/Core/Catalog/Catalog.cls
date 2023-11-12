/*------------------------------------------------------------------------
    Purpose     : create catalog from temp-table with classes 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Notes       : purely here for reference
                  The JSDO catalog cannot handle our relationships 
                  out of the box.   
                  It can handle the BEs with a single dataset 
                    
  ----------------------------------------------------------------------*/
block-level on error undo, throw.

using Ccs.Common.Application from propath.
using OpenEdge.Core.Assert from propath.
using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataRequest from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.Catalog.CatalogRequest from propath.
using Pmfo.Core.Catalog.IBusinessEntityCatalogBuilder from propath.
using Pmfo.Core.Catalog.ICatalog from propath.
using Pmfo.Core.Catalog.ICatalogRequest from propath.
using Pmfo.Core.Catalog.IResourceCatalogBuilder from propath.
using Pmfo.Core.Catalog.IServiceEntityCatalogBuilder from propath.
using Pmfo.Core.Common.DataService from propath.
using Pmfo.Core.Manager.IServiceManager from propath.
using Pmfo.Repository.Business.ResourceBE from propath.
using Pmfo.Util.QueryUtil from propath.
using Progress.Json.ObjectModel.JsonArray from propath.
using Progress.Json.ObjectModel.JsonObject from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
 

class Pmfo.Core.Catalog.Catalog inherits DataService implements ICatalog, IEntity: 
    
    {Pmfo/Repository/Business/defs/resources-dataset.i}
     
    define public property Resource       as character init "Catalog" no-undo get.  
    define public property Public         as logical   init true      no-undo get.  
    define public property ReadOnly       as logical   init true      no-undo get.  
    define public property CanDelete      as logical                  no-undo get.
    define public property CanCreate      as logical                  no-undo get.
    define public property CanUpdate      as logical                  no-undo get.
    define public property CanRead        as logical   init true      no-undo get.
    define public property ClientOnly     as logical                  no-undo get.
    define public property ServerType     as ServerTypeEnum           no-undo get. protected set. 
    
    define public property ServiceManager as IServiceManager no-undo
        get():
            return cast(Application:ServiceManager,IServiceManager).
        end get.
        
    define variable mResourceBE as REsourceBE no-undo. 
    
    define public property ResourceBE as REsourceBE no-undo 
        get():
            if not valid-object(mResourceBE) then
                mResourceBe = cast(ServiceManager:CreateBusinessEntity("resources"),ResourceBE).
            return mResourceBE.     
        end.

    define public property ServiceURI as character no-undo 
        get.
        set. 
 
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
        
    constructor public Catalog ( pcService as char):
        super ().
        ServerType = ServerTypeEnum:Authorization 
                     or 
                     ServerTypeEnum:General
                     or 
                     ServerTypeEnum:Report
                     or 
                     ServerTypeEnum:TextPlain.
                     
    end constructor.

    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    method public JsonObject GetResources(pcBasePath as character,pcCollections as character extent, pResponseName as character ):
        define variable catalogJson   as JsonObject no-undo.
        define variable cPath         as character no-undo.
        define variable cRoot         as character no-undo.
        define variable oBody        as JsonObject no-undo.
        assign catalogJson = new JsonObject().
        Assert:HasDeterminateExtent(pcCollections,"Path segments").
            
        if extent(pcCollections) = 1 then 
        do:
            cPath = subst("&1/&2",pcBasePath,pcCollections[1]).
            catalogJson:Add("version", 1.0). /* not sure */
            catalogJson:Add("lastModified", ResourceBE:LastModified).
            catalogJson:Add("name", pcCollections[1]).
            catalogJson:Add("address", cPath).
            catalogJson:Add("resources", getResourceList(pcBasePath,pcCollections[1])).
            if pResponseName > "" then
            do:
                oBody = catalogJson. 
                catalogJson = new JsonObject().
                catalogJson:Add(pResponseName,oBody).
            end.         
        end.
        else do:
            cRoot = if pResponseName > "" then pResponseName else "resource". 
            catalogJson:Add(cRoot,GetResource(pcBasePath,pcCollections[1],pcCollections[2])). 
        end.    
        return catalogJson.
        finally:
            if valid-object(mResourceBE) then
                delete object mResourceBE.
        end.    
    end method. /* getCatalog */
    
    method public JsonObject GetResources(pcBasePath as character,pcCollections as character extent ):
        return GetResources(pcBasePath,pcCollections,""). 
    end method. /* getCatalog */
  
    method protected JsonArray GetResourceList (pcPath as char , pcCatalogServioe as char):
        define variable oGetDataRequest as IGetDataRequest no-undo.        
        oGetDataRequest = QueryUtil:GetFindRequest("ttEntity","public eq true").
        return GetResourceList(pcPath, pcCatalogServioe,oGetDataRequest).
    end method. /* getResources */
    
    method protected JsonArray GetResourceList(pcPath as char , pcCatalogServioe as char,pRequest as IGetDataRequest ):
        
        define variable resourceArr     as JsonArray  no-undo.
        define variable resourceJson    as JsonObject no-undo.
       
        ResourceBE:GetData(pRequest,output dataset dsResources by-reference).          
        
        resourceArr = new JsonArray().
        for each ttEntity:
            resourceJson = new JsonObject().
            resourceJson:Add("name",ttEntity.Resource). 
            resourceJson:Add("path",subst("&1/&2",pcPath,ttEntity.Resource)). 
            resourceJson:Add("@readLink",subst("&1/&2/&3",pcPath,pcCatalogServioe,ttEntity.Resource)). 
            resourceArr:Add(resourceJson).
        end.
      
        return resourceArr.
    end method.  
    
    method protected JsonObject GetResource  (pcPath as char, pcCatalogService as char, pcName as character ):
        define variable resourceJson     as JsonObject no-undo.  
        define variable oEntity          as IEntity no-undo.
        define variable oCatalogRequest  as ICatalogRequest no-undo.
        define variable oResourceCatalog as IResourceCatalogBuilder no-undo.
        
        oEntity = cast(ServiceManager:getService(get-class(IEntity),pcName),IEntity).    
                  
        if type-of(oEntity,IBusinessEntity) then 
            oResourceCatalog = cast(ServiceManager:getService(get-class(IBusinessEntityCatalogBuilder)),IResourceCatalogBuilder).
        else if type-of(oEntity,IServiceEntity) then 
            oResourceCatalog = cast(ServiceManager:getService(get-class(IServiceEntityCatalogBuilder)),IResourceCatalogBuilder).
        
        oCatalogRequest = new CatalogRequest(pcPath,pcCatalogService). 
        resourceJson = oResourceCatalog:BuildCatalog(oCatalogRequest,oEntity).
        return resourceJson. 
        
    end method.
    
    method public override void Initialize():
    end method.
    
    method public override void Destroy():
    end method.
end class.