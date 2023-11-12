 
 /*------------------------------------------------------------------------
    File        : BusinessEntityDS
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : hdaniels
    Created     : Fri Mar 22 11:42:02 EDT 2019
    Notes       : 
  ----------------------------------------------------------------------*/


block-level on error undo, throw.

using Pmfo.Core.BusinessLogic.IBusinessEntity from propath.
using Pmfo.Core.BusinessLogic.IEntity from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableRequest from propath.
using Pmfo.Core.BusinessLogic.IGetDataTableResponse from propath.
using Pmfo.Core.BusinessLogic.IServiceEntity from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataRequest from propath.
using Pmfo.Core.Error.ApplicationError from propath.
using Pmfo.Core.Manager.ServiceManager from propath.
using Pmfo.Repository.DataSource.ResourceSource from propath.
using Progress.Lang.ParameterList from propath.
using Progress.Reflect.Method from propath.
using Progress.Reflect.Parameter from propath.
using Pmfo.Core.BusinessLogic.IUpdateDataEventArgs from propath.
using Pmfo.Core.Common.ServerTypeEnum from propath.
using Pmfo.Core.BusinessLogic.IClientEntity from propath.
using Pmfo.Core.BusinessLogic.IClientFilteredEntity from propath.

class Pmfo.Repository.DataSource.EntityDS inherits ResourceSource: 
    {Pmfo/Repository/schema/entity.i reference-only}
    
    define public override property EntityName as char init "entities"  no-undo get. 
        
    constructor public EntityDS (  ):
        super ().
    end constructor.
    
    method override protected character GetFileDirectory():
        return ServiceManager:EntityDirectory.
    end method.    
    
    // separated out for override
    method protected logical SubscribeUpdate(pEntity as Progress.Lang.Object ):
        define variable oMethod    as Method  no-undo.
             
        if type-of(pEntity,IBusinessEntity) then
        do: 
            // subscribe if there is a hook with input to the first table in  dataset[1]     
            oMethod = GetTableHook(cast(pEntity,IBusinessEntity),"validateTable").           
            if valid-object(oMethod) then
                return true.   
        end.
        
        return false.  
    end method.
    
    // separated out for override
    method protected logical SubscribeAction(pEntity as Progress.Lang.Object ):
        define variable oMethod    as Method  no-undo.
             
        if type-of(pEntity,IBusinessEntity) then
        do: 
            oMethod = GetTableHook(cast(pEntity,IBusinessEntity),"validateAction").           
            if valid-object(oMethod) then
                return true.                
        end.
        
        return false.  
    end method.
    
        /**
    methods with static table or dataset requires the actual static object in 
    the parameterList for getMethod so we must loop through getMethods to find
    this instead  
    **/
    method protected Method GetTableHook(pEntity as IBusinessEntity,pcName as char):
        define variable oMethods as Method extent no-undo.
        define variable oFlags as Progress.Reflect.Flags no-undo.
        define variable oParams as Parameter extent no-undo.
        define variable i as integer no-undo.
        define variable hDataset as handle no-undo.
        define variable hTable as handle no-undo.
        oFlags = (Progress.Reflect.Flags:Protected or Progress.Reflect.Flags:Public) or Progress.Reflect.Flags:Instance or Progress.Reflect.Flags:DeclaredOnly.
        oMethods = pEntity:GetClass():GetMethods(oFlags).
        do i = 1 to extent(oMethods):
            if oMethods[i]:Name = pcname and oMethods[i]:NumParameters = 2 then
            do:
               hDataset = pEntity:Datasets[1]. 
               if valid-handle(hDataset) then 
               do: 
                   oParams = oMethods[i]:GetParameters().
                   if entry(4,oParams[2]:ToString()," ") = hDataset:get-buffer-handle(1):table-handle:name 
                   and oParams[1]:DataTypeName = get-class(IUpdateDataEventArgs):TypeName
                   and oParams[2]:DataType = Progress.Reflect.DataType:Table 
                   and oParams[1]:mode = Progress.Reflect.ParameterMode:Input
                   and oParams[2]:mode = Progress.Reflect.ParameterMode:Input then
                   do: 
                      return oMethods[i]. 
                   end.
               end.            
            end.
        end.   
        return ?. 
    end method.  
    
    // separated out for override
    method protected logical SubscribeReceive(pEntity as Progress.Lang.Object ):
        define variable oParamList as ParameterList  no-undo.
        define variable hHandle as handle no-undo.
        define variable oRequest as IGetDataTableRequest no-undo.
        define variable oResponse as IGetDataTableResponse no-undo.
        define variable oMethod as Method  no-undo.
        
        // we must always call receive in client BE 
        if type-of(pEntity,IClientEntity) then
            return true. 
        
        // we must always call receive in client filtered BE 
        if type-of(pEntity,IClientFilteredEntity) then
            return true. 
        
        if type-of(pEntity,IBusinessEntity) then
        do: 
            oParamList = new ParameterList(4).
            oParamList:SetParameter(1,get-class(IBusinessEntity):TypeName,"input",pEntity).
            oParamList:SetParameter(2,get-class(IGetDataTableRequest):TypeName,"input",oRequest).
            oParamList:SetParameter(3,get-class(IGetDataTableResponse):TypeName,"input",oResponse).
            oParamList:SetParameter(4,"handle","input",hhandle).
            
            oMethod = pEntity:GetClass():GetMethod("ReceiveBuffer",Progress.Reflect.Flags:Protected or Progress.Reflect.Flags:Instance or Progress.Reflect.Flags:DeclaredOnly,oParamList).
            return valid-object(oMethod).
        end.
        return false. 
    end method.
    
    // separated out for override
    method protected logical SubscribeGet(pEntity as Progress.Lang.Object ):
        define variable oParamList as ParameterList  no-undo.
        define variable hHandle as handle no-undo.
        define variable oRequest as IGetDataTableRequest no-undo.
        define variable oResponse as IGetDataTableResponse no-undo.
        define variable oMethod as Method  no-undo.
        if type-of(pEntity,IBusinessEntity) then
        do: 
            oParamList = new ParameterList(2).
            oParamList:SetParameter(1,get-class(IGetDataTableRequest):TypeName,"input",oRequest).
            oParamList:SetParameter(2,"handle","input",hhandle).
            
            oMethod = pEntity:GetClass():GetMethod("PrepareGetTableRequest",Progress.Reflect.Flags:Public or Progress.Reflect.Flags:Instance or Progress.Reflect.Flags:DeclaredOnly,oParamList).
            return valid-object(oMethod).
        end.
        return false. 
    end method.
    
     // get entiry references 
    method protected character GetEntityReferences(pBusinessEntity as IBusinessEntity, output pContainer as logical):
        define variable iDataset    as integer no-undo.
        define variable iBuffer     as integer no-undo.
        define variable hBuffer     as handle no-undo.
        define variable cReferences as character no-undo.
        define variable cDlm        as character no-undo.
        
        do iDataset = 1 to extent(pBusinessEntity:Datasets):
            do iBuffer = 1 to pBusinessEntity:Datasets[iDataset]:num-buffers:
                hBuffer = pBusinessEntity:Datasets[iDataset]:get-buffer-handle(iBuffer).
                if hBuffer:table-handle:serialize-name <> pBusinessEntity:Resource 
                and lookup(hBuffer:table-handle:serialize-name,cReferences) = 0 then
                do:
                    assign
                        cReferences = subst("&1&2&3",cReferences,cDlm,hBuffer:table-handle:serialize-name)
                        cDlm        = ",".
                    if iDataset = 1 and iBuffer = 1 then  
                        pContainer  = true.  
                end.
            end.
        end.
        
        return cReferences. 
    end method.
    
    method override protected void FillFromObject( input poObject as Progress.Lang.Object ):
        define variable oEntity as IEntity no-undo.
        
        if type-of(poObject,IEntity) then 
        do on error undo, throw:
            oEntity = cast(poObject,IEntity).
            if oEntity:Resource = "" then
                undo, throw new ApplicationError(GetCodeCreateErrorMessage("Entity","with blank Resource property",oEntity:getClass():TypeName,?)). 
            
            if oEntity:Resource = "resources" then 
            do:
                undo, throw new ApplicationError(ReservedResourceMessage("Entity" ,oEntity:GetClass():TypeName, "Resource","resources")).
            end.  
              
            create ttEntity.
            assign
                ttEntity.ClassName        = oEntity:getClass():TypeName
                // it is possible to serialize as object, but error prone as null causes entire row to be ignored  
                ttEntity.ServerTypes = if valid-object(oEntity:ServerType) then oEntity:ServerType:ToString() else ServerTypeEnum:Default:ToString()  
           
                ttEntity.Public           = oEntity:Public
                ttEntity.ReadOnly         = oEntity:ReadOnly
                ttEntity.CanCreate        = oEntity:CanCreate
                ttEntity.CanDelete        = oEntity:CanDelete
                ttEntity.Resource         = oEntity:Resource
                ttEntity.ClientOnly       = oEntity:ClientOnly
                ttEntity.SubscribeAction  = SubscribeAction(oEntity)
                ttEntity.SubscribeUpdate  = SubscribeUpdate(oEntity)
                ttEntity.SubscribeReceive = SubscribeReceive(oEntity)
                ttEntity.SubscribeGet     = SubscribeGet(oEntity)
                .
            if type-of(oEntity,IBusinessEntity) then
            do:
                assign 
                    ttEntity.TypeName         = get-class(IBusinessEntity):TypeName
                    ttEntity.KeyList          = cast(oEntity,IBusinessEntity):GetKeys().
                    ttEntity.EntityReferences = GetEntityReferences(cast(oEntity,IBusinessEntity), output ttEntity.IsDataContainer)
                    .
            end.
            else if type-of(oEntity,IServiceEntity) then
                ttEntity.TypeName = get-class(IServiceEntity):TypeName.
            else
                ttEntity.TypeName = get-class(IEntity):TypeName.
            
            catch e1 as ApplicationError:
                undo, throw e1.
            end catch.        
            catch e2 as Progress.Lang.Error :
                if avail ttEntity then 
                    delete ttEntity.
                undo, throw new ApplicationError(GetCodeCreateErrorMessage("Entity",oEntity:Resource,oEntity:getClass():TypeName,e2),e2). 
            end catch.      
        end.    
    end method.
    
    method protected void Bind(input table ttEntity bind ):
    end method.     

    method override protected void PrepareBuffer( input phBuffer as handle ):
        define variable hTable as handle no-undo.
        hTable = phBuffer:table-handle.
        this-object:Bind(table-handle hTable bind).
        super:PrepareBuffer(phBuffer).
    end method.
    
    method public override void Cleanup (input table-handle pDataSource):
        CleanupDataSource(input table-handle pDataSource by-reference).
    end method.

    method protected void CleanupDataSource (input table ttEntity):
        // define variable icount as int no-undo.
        for each ttEntity where ttEntity.ClassName > "":
            //   iCount = iCount + 1.
            if search(subst("&1.r",replace(ttEntity.ClassName,".","/"))) = ?
            and search(subst("&1.cls",replace(ttEntity.ClassName,".","/"))) = ? then
                delete ttEntity. 
        end. 
    end method.     
    
end class.